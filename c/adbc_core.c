/*
 * ADBC Core FFI - Generic ADBC interface for Lean 4
 * Dynamically loads ADBC driver via dlopen
 */
#include <lean/lean.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdarg.h>
#include <dlfcn.h>
#include <time.h>

/* === Constants === */
#define CELL_BUF_SIZE 64

/* === ADBC Structures (from Arrow ADBC spec) === */

typedef uint8_t AdbcStatusCode;
#define ADBC_STATUS_OK 0
#define ADBC_STATUS_UNKNOWN 1
#define ADBC_STATUS_NOT_IMPLEMENTED 2
#define ADBC_STATUS_NOT_FOUND 3
#define ADBC_STATUS_ALREADY_EXISTS 4
#define ADBC_STATUS_INVALID_ARGUMENT 5
#define ADBC_STATUS_INVALID_STATE 6
#define ADBC_STATUS_INVALID_DATA 7
#define ADBC_STATUS_INTEGRITY 8
#define ADBC_STATUS_INTERNAL 9
#define ADBC_STATUS_IO 10
#define ADBC_STATUS_CANCELLED 11
#define ADBC_STATUS_TIMEOUT 12
#define ADBC_STATUS_UNAUTHENTICATED 13
#define ADBC_STATUS_UNAUTHORIZED 14

struct AdbcError {
    char* message;
    int32_t vendor_code;
    char sqlstate[5];
    void (*release)(struct AdbcError*);
    void* private_data;
    void* private_driver;
};

struct AdbcDatabase {
    void* private_data;
    void* private_driver;
};

struct AdbcConnection {
    void* private_data;
    void* private_driver;
};

struct AdbcStatement {
    void* private_data;
    void* private_driver;
};

/* === Arrow C Data Interface === */

struct ArrowSchema {
    const char* format;
    const char* name;
    const char* metadata;
    int64_t flags;
    int64_t n_children;
    struct ArrowSchema** children;
    struct ArrowSchema* dictionary;
    void (*release)(struct ArrowSchema*);
    void* private_data;
};

struct ArrowArray {
    int64_t length;
    int64_t null_count;
    int64_t offset;
    int64_t n_buffers;
    int64_t n_children;
    const void** buffers;
    struct ArrowArray** children;
    struct ArrowArray* dictionary;
    void (*release)(struct ArrowArray*);
    void* private_data;
};

struct ArrowArrayStream {
    int (*get_schema)(struct ArrowArrayStream*, struct ArrowSchema* out);
    int (*get_next)(struct ArrowArrayStream*, struct ArrowArray* out);
    const char* (*get_last_error)(struct ArrowArrayStream*);
    void (*release)(struct ArrowArrayStream*);
    void* private_data;
};

/* === ADBC Function Pointers (loaded via dlsym) === */

typedef AdbcStatusCode (*PFN_AdbcDatabaseNew)(struct AdbcDatabase*, struct AdbcError*);
typedef AdbcStatusCode (*PFN_AdbcDatabaseSetOption)(struct AdbcDatabase*, const char*, const char*, struct AdbcError*);
typedef AdbcStatusCode (*PFN_AdbcDatabaseInit)(struct AdbcDatabase*, struct AdbcError*);
typedef AdbcStatusCode (*PFN_AdbcDatabaseRelease)(struct AdbcDatabase*, struct AdbcError*);
typedef AdbcStatusCode (*PFN_AdbcConnectionNew)(struct AdbcConnection*, struct AdbcError*);
typedef AdbcStatusCode (*PFN_AdbcConnectionInit)(struct AdbcConnection*, struct AdbcDatabase*, struct AdbcError*);
typedef AdbcStatusCode (*PFN_AdbcConnectionRelease)(struct AdbcConnection*, struct AdbcError*);
typedef AdbcStatusCode (*PFN_AdbcStatementNew)(struct AdbcConnection*, struct AdbcStatement*, struct AdbcError*);
typedef AdbcStatusCode (*PFN_AdbcStatementSetSqlQuery)(struct AdbcStatement*, const char*, struct AdbcError*);
typedef AdbcStatusCode (*PFN_AdbcStatementExecuteQuery)(struct AdbcStatement*, struct ArrowArrayStream*, int64_t*, struct AdbcError*);
typedef AdbcStatusCode (*PFN_AdbcStatementRelease)(struct AdbcStatement*, struct AdbcError*);

// | ADBC function pointers (indexed by enum)
enum { FN_DB_NEW, FN_DB_OPT, FN_DB_INIT, FN_DB_REL, FN_CONN_NEW, FN_CONN_INIT,
       FN_CONN_REL, FN_STMT_NEW, FN_STMT_SQL, FN_STMT_EXEC, FN_STMT_REL, FN_COUNT };
static void* g_fn[FN_COUNT];
static const char* g_fn_names[] = {
    "AdbcDatabaseNew", "AdbcDatabaseSetOption", "AdbcDatabaseInit", "AdbcDatabaseRelease",
    "AdbcConnectionNew", "AdbcConnectionInit", "AdbcConnectionRelease",
    "AdbcStatementNew", "AdbcStatementSetSqlQuery", "AdbcStatementExecuteQuery", "AdbcStatementRelease"
};
#define pAdbcDatabaseNew       ((PFN_AdbcDatabaseNew)g_fn[FN_DB_NEW])
#define pAdbcDatabaseSetOption ((PFN_AdbcDatabaseSetOption)g_fn[FN_DB_OPT])
#define pAdbcDatabaseInit      ((PFN_AdbcDatabaseInit)g_fn[FN_DB_INIT])
#define pAdbcDatabaseRelease   ((PFN_AdbcDatabaseRelease)g_fn[FN_DB_REL])
#define pAdbcConnectionNew     ((PFN_AdbcConnectionNew)g_fn[FN_CONN_NEW])
#define pAdbcConnectionInit    ((PFN_AdbcConnectionInit)g_fn[FN_CONN_INIT])
#define pAdbcConnectionRelease ((PFN_AdbcConnectionRelease)g_fn[FN_CONN_REL])
#define pAdbcStatementNew      ((PFN_AdbcStatementNew)g_fn[FN_STMT_NEW])
#define pAdbcStatementSetSqlQuery   ((PFN_AdbcStatementSetSqlQuery)g_fn[FN_STMT_SQL])
#define pAdbcStatementExecuteQuery  ((PFN_AdbcStatementExecuteQuery)g_fn[FN_STMT_EXEC])
#define pAdbcStatementRelease  ((PFN_AdbcStatementRelease)g_fn[FN_STMT_REL])

/* === Global State === */

static void* g_lib = NULL;
static struct AdbcDatabase g_db = {0};
static struct AdbcConnection g_conn = {0};
static int g_initialized = 0;
static FILE* g_log = NULL;

/* === Logging === */
static void log_msg(const char* fmt, ...) {
    if (!g_log) g_log = fopen("/tmp/tv.log", "a");
    if (!g_log) return;
    struct timespec ts; clock_gettime(CLOCK_REALTIME, &ts);
    struct tm* t = localtime(&ts.tv_sec);
    fprintf(g_log, "%02d:%02d:%02d.%03ld ", t->tm_hour, t->tm_min, t->tm_sec, ts.tv_nsec / 1000000);
    va_list args;
    va_start(args, fmt);
    vfprintf(g_log, fmt, args);
    va_end(args);
    fflush(g_log);
}

/* === Helpers === */
static void init_error(struct AdbcError* err) { memset(err, 0, sizeof(*err)); }
static void free_error(struct AdbcError* err) { if (err->release) err->release(err); }

/* === Load ADBC functions from driver === */
static int load_adbc_funcs_from_path(const char* path) {
    g_lib = dlopen(path, RTLD_NOW | RTLD_GLOBAL);
    if (!g_lib) { log_msg("[adbc] dlopen %s failed: %s\n", path, dlerror()); return 0; }
    log_msg("[adbc] loaded %s\n", path);
    for (int i = 0; i < FN_COUNT; i++) {
        g_fn[i] = dlsym(g_lib, g_fn_names[i]);
        if (!g_fn[i]) { log_msg("[adbc] missing symbol: %s\n", g_fn_names[i]); return 0; }
    }
    return 1;
}

// | Check ADBC call, log and goto fail on error
#define ADBC_CHECK(call, msg) do { \
    if ((call) != ADBC_STATUS_OK) { \
        log_msg("[adbc] %s: %s\n", msg, err.message ? err.message : "?"); \
        goto fail; \
    } \
} while(0)

// | Generic ADBC init with configurable driver
lean_obj_res lean_adbc_init_driver(b_lean_obj_arg driver_obj, b_lean_obj_arg entry_obj, lean_obj_arg world) {
    if (g_initialized) return lean_io_result_mk_ok(lean_box(1));

    const char* driver = lean_string_cstr(driver_obj);
    const char* entry = lean_string_cstr(entry_obj);

    if (!load_adbc_funcs_from_path(driver)) return lean_io_result_mk_ok(lean_box(0));

    struct AdbcError err;
    init_error(&err);
    int have_db = 0, have_conn = 0;

    ADBC_CHECK(pAdbcDatabaseNew(&g_db, &err), "DatabaseNew");
    have_db = 1;
    ADBC_CHECK(pAdbcDatabaseSetOption(&g_db, "driver", driver, &err), "SetOption(driver)");
    ADBC_CHECK(pAdbcDatabaseSetOption(&g_db, "entrypoint", entry, &err), "SetOption(entrypoint)");
    ADBC_CHECK(pAdbcDatabaseSetOption(&g_db, "path", "", &err), "SetOption(path)");
    ADBC_CHECK(pAdbcDatabaseInit(&g_db, &err), "DatabaseInit");
    ADBC_CHECK(pAdbcConnectionNew(&g_conn, &err), "ConnectionNew");
    have_conn = 1;
    ADBC_CHECK(pAdbcConnectionInit(&g_conn, &g_db, &err), "ConnectionInit");

    log_msg("[adbc] initialized OK with driver=%s entry=%s\n", driver, entry);
    g_initialized = 1;
    return lean_io_result_mk_ok(lean_box(1));

fail:
    if (have_conn) pAdbcConnectionRelease(&g_conn, &err);
    if (have_db) pAdbcDatabaseRelease(&g_db, &err);
    free_error(&err);
    return lean_io_result_mk_ok(lean_box(0));
}

// | Shutdown ADBC
lean_obj_res lean_adbc_shutdown(lean_obj_arg world) {
    if (!g_initialized) return lean_io_result_mk_ok(lean_box(0));

    struct AdbcError err;
    init_error(&err);
    pAdbcConnectionRelease(&g_conn, &err);
    pAdbcDatabaseRelease(&g_db, &err);
    free_error(&err);

    g_initialized = 0;
    memset(&g_conn, 0, sizeof(g_conn));
    memset(&g_db, 0, sizeof(g_db));

    if (g_lib) { dlclose(g_lib); g_lib = NULL; }
    return lean_io_result_mk_ok(lean_box(0));
}

/* === Query Result (opaque to Lean) === */

typedef struct {
    struct ArrowSchema schema;
    struct ArrowArray* batches;
    int64_t* prefix;      // prefix[i] = sum of rows in batches[0..i-1]
    int64_t n_batches;
    int64_t total_rows;
} QueryResult;

// | Finalize QueryResult
static void qr_finalize(void* p) {
    QueryResult* qr = (QueryResult*)p;
    if (qr->schema.release) qr->schema.release(&qr->schema);
    for (int64_t i = 0; i < qr->n_batches; i++) {
        if (qr->batches[i].release) qr->batches[i].release(&qr->batches[i]);
    }
    free(qr->batches);
    free(qr->prefix);
    free(qr);
}

static void qr_foreach(void* p, b_lean_obj_arg f) { (void)p; (void)f; }

static lean_external_class* g_qr_class = NULL;

static lean_external_class* get_qr_class(void) {
    if (!g_qr_class) g_qr_class = lean_register_external_class(qr_finalize, qr_foreach);
    return g_qr_class;
}

// | Execute SQL query, return QueryResult
lean_obj_res lean_adbc_query(b_lean_obj_arg sql_obj, lean_obj_arg world) {
    if (!g_initialized) return lean_io_result_mk_error(lean_mk_io_user_error(lean_mk_string("ADBC not initialized")));

    const char* sql = lean_string_cstr(sql_obj);
    struct AdbcError err;
    init_error(&err);
    struct AdbcStatement stmt = {0};
    struct ArrowArrayStream stream = {0};
    QueryResult* qr = NULL;
    const char* fail_msg = NULL;

    #define QUERY_CHECK(call, msg) if ((call) != ADBC_STATUS_OK) { fail_msg = err.message ? err.message : msg; goto fail; }
    #define STREAM_CHECK(call, msg) if ((call) != 0) { fail_msg = msg; goto fail; }

    QUERY_CHECK(pAdbcStatementNew(&g_conn, &stmt, &err), "StatementNew");
    QUERY_CHECK(pAdbcStatementSetSqlQuery(&stmt, sql, &err), "SetSqlQuery");
    int64_t rows_affected = -1;
    QUERY_CHECK(pAdbcStatementExecuteQuery(&stmt, &stream, &rows_affected, &err), "ExecuteQuery");

    qr = calloc(1, sizeof(QueryResult));
    STREAM_CHECK(stream.get_schema(&stream, &qr->schema), "get_schema");

    // collect batches
    int64_t cap = 16;
    qr->batches = malloc(cap * sizeof(struct ArrowArray));
    qr->prefix = malloc((cap + 1) * sizeof(int64_t));
    qr->prefix[0] = 0;
    while (1) {
        struct ArrowArray batch = {0};
        if (stream.get_next(&stream, &batch) != 0 || !batch.release) break;
        if (qr->n_batches >= cap) {
            cap *= 2;
            qr->batches = realloc(qr->batches, cap * sizeof(struct ArrowArray));
            qr->prefix = realloc(qr->prefix, (cap + 1) * sizeof(int64_t));
        }
        qr->batches[qr->n_batches] = batch;
        qr->total_rows += batch.length;
        qr->prefix[++qr->n_batches] = qr->total_rows;
    }

    if (stream.release) stream.release(&stream);
    pAdbcStatementRelease(&stmt, &err);
    free_error(&err);
    return lean_io_result_mk_ok(lean_alloc_external(get_qr_class(), qr));

fail:
    if (qr) { free(qr->batches); free(qr->prefix); free(qr); }
    if (stream.release) stream.release(&stream);
    if (stmt.private_data) pAdbcStatementRelease(&stmt, &err);
    free_error(&err);
    return lean_io_result_mk_error(lean_mk_io_user_error(lean_mk_string(fail_msg)));
    #undef QUERY_CHECK
    #undef STREAM_CHECK
}

// | Get column count
lean_obj_res lean_qr_ncols(b_lean_obj_arg qr_obj, lean_obj_arg world) {
    QueryResult* qr = (QueryResult*)lean_get_external_data(qr_obj);
    return lean_io_result_mk_ok(lean_box_uint64((uint64_t)qr->schema.n_children));
}

// | Get row count
lean_obj_res lean_qr_nrows(b_lean_obj_arg qr_obj, lean_obj_arg world) {
    QueryResult* qr = (QueryResult*)lean_get_external_data(qr_obj);
    return lean_io_result_mk_ok(lean_box_uint64((uint64_t)qr->total_rows));
}

// | Get column name
lean_obj_res lean_qr_col_name(b_lean_obj_arg qr_obj, uint64_t col, lean_obj_arg world) {
    QueryResult* qr = (QueryResult*)lean_get_external_data(qr_obj);
    if ((int64_t)col >= qr->schema.n_children) return lean_io_result_mk_ok(lean_mk_string(""));
    const char* name = qr->schema.children[col]->name;
    return lean_io_result_mk_ok(lean_mk_string(name ? name : ""));
}

// | Get column format (Arrow type string)
lean_obj_res lean_qr_col_fmt(b_lean_obj_arg qr_obj, uint64_t col, lean_obj_arg world) {
    QueryResult* qr = (QueryResult*)lean_get_external_data(qr_obj);
    if ((int64_t)col >= qr->schema.n_children) return lean_io_result_mk_ok(lean_mk_string(""));
    const char* fmt = qr->schema.children[col]->format;
    return lean_io_result_mk_ok(lean_mk_string(fmt ? fmt : ""));
}

/* === Cell Access === */

// | Binary search for batch containing global row
static int find_batch(QueryResult* qr, int64_t row, int64_t* batch_idx, int64_t* local_row) {
    if (row < 0 || row >= qr->total_rows) return 0;
    int64_t lo = 0, hi = qr->n_batches;
    while (lo < hi) {
        int64_t mid = lo + (hi - lo + 1) / 2;
        if (qr->prefix[mid] <= row) lo = mid;
        else hi = mid - 1;
    }
    *batch_idx = lo;
    *local_row = row - qr->prefix[lo];
    return 1;
}

// | Check validity bitmap for null
static int is_null(struct ArrowArray* arr, int64_t row) {
    if (arr->null_count == 0 || !arr->buffers[0]) return 0;
    const uint8_t* validity = (const uint8_t*)arr->buffers[0];
    int64_t idx = arr->offset + row;
    return !(validity[idx / 8] & (1 << (idx % 8)));
}

typedef struct { struct ArrowArray* arr; struct ArrowSchema* sch; const char* fmt; int64_t lr; } CellInfo;

static CellInfo get_cell(QueryResult* qr, int64_t row, int64_t col) {
    CellInfo ci = {NULL, NULL, NULL, 0};
    int64_t bi;
    if (!find_batch(qr, row, &bi, &ci.lr)) return ci;
    if (col >= qr->schema.n_children) return ci;
    ci.arr = qr->batches[bi].children[col];
    ci.sch = qr->schema.children[col];
    ci.fmt = ci.sch->format;
    return ci;
}

// forward decls
static size_t format_cell_batch(struct ArrowArray* arr, const char* fmt, int64_t lr, char* buf, size_t buflen, uint8_t decimals);
static size_t format_cell_full(struct ArrowArray* arr, struct ArrowSchema* sch, int64_t lr, char* buf, size_t buflen, uint8_t decimals);

// | Get cell as string
lean_obj_res lean_qr_cell_str(b_lean_obj_arg qr_obj, uint64_t row, uint64_t col, lean_obj_arg world) {
    QueryResult* qr = (QueryResult*)lean_get_external_data(qr_obj);
    CellInfo c = get_cell(qr, row, col);
    if (!c.arr) return lean_io_result_mk_ok(lean_mk_string(""));
    char buf[CELL_BUF_SIZE];
    format_cell_full(c.arr, c.sch, c.lr, buf, sizeof(buf), 3);
    return lean_io_result_mk_ok(lean_mk_string(buf));
}

// | Get cell as Int (handles signed/unsigned integer types)
lean_obj_res lean_qr_cell_int(b_lean_obj_arg qr_obj, uint64_t row, uint64_t col, lean_obj_arg world) {
    QueryResult* qr = (QueryResult*)lean_get_external_data(qr_obj);
    CellInfo c = get_cell(qr, row, col);
    if (!c.arr || is_null(c.arr, c.lr)) return lean_io_result_mk_ok(lean_int64_to_int(0));
    int64_t val = 0;
    char f = c.fmt[0];
    if (f == 'l')      val = ((const int64_t*)c.arr->buffers[1])[c.arr->offset + c.lr];
    else if (f == 'L') val = (int64_t)((const uint64_t*)c.arr->buffers[1])[c.arr->offset + c.lr];
    else if (f == 'i') val = ((const int32_t*)c.arr->buffers[1])[c.arr->offset + c.lr];
    else if (f == 'I') val = ((const uint32_t*)c.arr->buffers[1])[c.arr->offset + c.lr];
    else if (f == 's') val = ((const int16_t*)c.arr->buffers[1])[c.arr->offset + c.lr];
    else if (f == 'S') val = ((const uint16_t*)c.arr->buffers[1])[c.arr->offset + c.lr];
    else if (f == 'c') val = ((const int8_t*)c.arr->buffers[1])[c.arr->offset + c.lr];
    else if (f == 'C') val = ((const uint8_t*)c.arr->buffers[1])[c.arr->offset + c.lr];
    return lean_io_result_mk_ok(lean_int64_to_int(val));
}

// | Get cell as Float
lean_obj_res lean_qr_cell_float(b_lean_obj_arg qr_obj, uint64_t row, uint64_t col, lean_obj_arg world) {
    QueryResult* qr = (QueryResult*)lean_get_external_data(qr_obj);
    CellInfo c = get_cell(qr, row, col);
    if (!c.arr || is_null(c.arr, c.lr)) return lean_io_result_mk_ok(lean_box_float(0.0));
    double val = 0.0;
    if (c.fmt[0] == 'g')      val = ((const double*)c.arr->buffers[1])[c.arr->offset + c.lr];
    else if (c.fmt[0] == 'f') val = ((const float*)c.arr->buffers[1])[c.arr->offset + c.lr];
    return lean_io_result_mk_ok(lean_box_float(val));
}

// | Check if cell is null
lean_obj_res lean_qr_cell_is_null(b_lean_obj_arg qr_obj, uint64_t row, uint64_t col, lean_obj_arg world) {
    QueryResult* qr = (QueryResult*)lean_get_external_data(qr_obj);
    CellInfo c = get_cell(qr, row, col);
    return lean_io_result_mk_ok(lean_box(!c.arr || is_null(c.arr, c.lr) ? 1 : 0));
}

/* === Cell Formatting === */

// | Format cell value to string
static size_t format_cell_batch(struct ArrowArray* arr, const char* fmt, int64_t lr, char* buf, size_t buflen, uint8_t decimals) {
    char tmp[CELL_BUF_SIZE];
    if (!buf) { buf = tmp; buflen = sizeof(tmp); }
    if (is_null(arr, lr)) { buf[0] = '\0'; return 0; }

    char f = fmt[0];
    if (f == 'l') return snprintf(buf, buflen, "%ld", ((const int64_t*)arr->buffers[1])[arr->offset + lr]);
    if (f == 'L') return snprintf(buf, buflen, "%lu", ((const uint64_t*)arr->buffers[1])[arr->offset + lr]);
    if (f == 'i') return snprintf(buf, buflen, "%d", ((const int32_t*)arr->buffers[1])[arr->offset + lr]);
    if (f == 'I') return snprintf(buf, buflen, "%u", ((const uint32_t*)arr->buffers[1])[arr->offset + lr]);
    if (f == 's') return snprintf(buf, buflen, "%d", ((const int16_t*)arr->buffers[1])[arr->offset + lr]);
    if (f == 'S') return snprintf(buf, buflen, "%u", ((const uint16_t*)arr->buffers[1])[arr->offset + lr]);
    if (f == 'c') return snprintf(buf, buflen, "%d", ((const int8_t*)arr->buffers[1])[arr->offset + lr]);
    if (f == 'C') return snprintf(buf, buflen, "%u", ((const uint8_t*)arr->buffers[1])[arr->offset + lr]);
    if (f == 'g') return snprintf(buf, buflen, "%.*f", decimals, ((const double*)arr->buffers[1])[arr->offset + lr]);
    if (f == 'f') return snprintf(buf, buflen, "%.*f", decimals, ((const float*)arr->buffers[1])[arr->offset + lr]);
    if (f == 'e') {  // half-float
        uint16_t h = ((const uint16_t*)arr->buffers[1])[arr->offset + lr];
        uint32_t sign = (h & 0x8000) << 16;
        uint32_t exp = (h >> 10) & 0x1F;
        uint32_t mant = h & 0x3FF;
        uint32_t f32;
        if (exp == 0) f32 = sign | (mant << 13);
        else if (exp == 31) f32 = sign | 0x7F800000 | (mant << 13);
        else f32 = sign | ((exp + 112) << 23) | (mant << 13);
        float val; memcpy(&val, &f32, 4);
        return snprintf(buf, buflen, "%.*f", decimals, (double)val);
    }
    if (f == 'u') {  // utf8
        const int32_t* off = (const int32_t*)arr->buffers[1];
        const char* data = (const char*)arr->buffers[2];
        int64_t idx = arr->offset + lr;
        int32_t len = off[idx + 1] - off[idx];
        if ((size_t)len >= buflen) len = buflen - 1;
        memcpy(buf, data + off[idx], len);
        buf[len] = '\0';
        return len;
    }
    if (f == 'z') {  // binary: hex
        const int32_t* off = (const int32_t*)arr->buffers[1];
        const uint8_t* data = (const uint8_t*)arr->buffers[2];
        int64_t idx = arr->offset + lr;
        int32_t len = off[idx + 1] - off[idx];
        int show = len > 8 ? 8 : len;
        int pos = 0;
        for (int i = 0; i < show && (size_t)pos < buflen - 3; i++)
            pos += snprintf(buf + pos, buflen - pos, "%02x", data[off[idx] + i]);
        if (len > 8 && (size_t)pos < buflen - 3) pos += snprintf(buf + pos, buflen - pos, "..");
        return pos;
    }
    if (f == 'U') {  // large utf8
        const int64_t* off = (const int64_t*)arr->buffers[1];
        const char* data = (const char*)arr->buffers[2];
        int64_t idx = arr->offset + lr;
        int64_t len = off[idx + 1] - off[idx];
        if ((size_t)len >= buflen) len = buflen - 1;
        memcpy(buf, data + off[idx], len);
        buf[len] = '\0';
        return len;
    }
    if (f == 'Z') {  // large binary
        const int64_t* off = (const int64_t*)arr->buffers[1];
        const uint8_t* data = (const uint8_t*)arr->buffers[2];
        int64_t idx = arr->offset + lr;
        int64_t len = off[idx + 1] - off[idx];
        int show = len > 8 ? 8 : len;
        int pos = 0;
        for (int i = 0; i < show && (size_t)pos < buflen - 3; i++)
            pos += snprintf(buf + pos, buflen - pos, "%02x", data[off[idx] + i]);
        if (len > 8 && (size_t)pos < buflen - 3) pos += snprintf(buf + pos, buflen - pos, "..");
        return pos;
    }
    if (f == 'b') {  // bool
        const uint8_t* data = (const uint8_t*)arr->buffers[1];
        int64_t idx = arr->offset + lr;
        return snprintf(buf, buflen, "%s", ((data[idx/8] >> (idx%8)) & 1) ? "true" : "false");
    }
    if (f == 'd' && fmt[1] == ':') {  // decimal
        int scale = 0;
        const char* p = fmt + 2;
        while (*p && *p != ',') p++;
        if (*p == ',') { p++; while (*p >= '0' && *p <= '9') scale = scale * 10 + (*p++ - '0'); }
        double val = (double)((const int64_t*)arr->buffers[1])[(arr->offset + lr) * 2];
        for (int i = 0; i < scale; i++) val /= 10.0;
        return snprintf(buf, buflen, "%.*f", scale, val);
    }
    if (f == 't' && fmt[1] == 's') {  // timestamp
        int64_t us = ((const int64_t*)arr->buffers[1])[arr->offset + lr];
        time_t secs = us / 1000000;
        struct tm* tm = gmtime(&secs);
        return snprintf(buf, buflen, "%04d-%02d-%02d %02d:%02d:%02d",
                 tm->tm_year + 1900, tm->tm_mon + 1, tm->tm_mday, tm->tm_hour, tm->tm_min, tm->tm_sec);
    }
    if (f == 't' && fmt[1] == 't') {  // time
        int64_t s = ((const int64_t*)arr->buffers[1])[arr->offset + lr] / 1000000;
        return snprintf(buf, buflen, "%02d:%02d:%02d", (int)((s/3600)%24), (int)((s/60)%60), (int)(s%60));
    }
    if (f == 't' && fmt[1] == 'd') {  // date32
        int32_t days = ((const int32_t*)arr->buffers[1])[arr->offset + lr];
        time_t secs = (time_t)days * 86400;
        struct tm* tm = gmtime(&secs);
        return snprintf(buf, buflen, "%04d-%02d-%02d", tm->tm_year + 1900, tm->tm_mon + 1, tm->tm_mday);
    }
    if (f == '+' && fmt[1] == 'l') {  // list
        if (!arr->buffers[1]) { buf[0] = '\0'; return 0; }
        const int32_t* off = (const int32_t*)arr->buffers[1];
        int64_t idx = arr->offset + lr;
        int32_t start = off[idx], end = off[idx + 1];
        int32_t n = end - start;
        int pos = snprintf(buf, buflen, "[%d]", n);
        if (n > 0 && arr->n_children > 0 && arr->children && arr->children[0] && arr->children[0]->buffers[1]) {
            struct ArrowArray* child = arr->children[0];
            for (int32_t i = 0; i < n && i < 3 && (size_t)pos < buflen - 10; i++) {
                pos += snprintf(buf + pos, buflen - pos, "%s", i ? "; " : " ");
                int64_t v = ((const int64_t*)child->buffers[1])[start + i];
                pos += snprintf(buf + pos, buflen - pos, "%ld", (long)v);
            }
            if (n > 3 && (size_t)pos < buflen - 4) pos += snprintf(buf + pos, buflen - pos, "...");
        }
        return pos;
    }
    if (f == '+' && fmt[1] == 'L') {  // large_list
        if (!arr->buffers[1]) { buf[0] = '\0'; return 0; }
        const int64_t* off = (const int64_t*)arr->buffers[1];
        int64_t idx = arr->offset + lr;
        int64_t start = off[idx], end = off[idx + 1];
        int64_t n = end - start;
        int pos = snprintf(buf, buflen, "[%ld]", (long)n);
        if (n > 0 && arr->n_children > 0 && arr->children && arr->children[0] && arr->children[0]->buffers[1]) {
            struct ArrowArray* child = arr->children[0];
            for (int64_t i = 0; i < n && i < 3 && (size_t)pos < buflen - 10; i++) {
                pos += snprintf(buf + pos, buflen - pos, "%s", i ? "; " : " ");
                int64_t v = ((const int64_t*)child->buffers[1])[start + i];
                pos += snprintf(buf + pos, buflen - pos, "%ld", (long)v);
            }
            if (n > 3 && (size_t)pos < buflen - 4) pos += snprintf(buf + pos, buflen - pos, "...");
        }
        return pos;
    }
    if (f == '+' && fmt[1] == 'w') {  // fixed_size_list
        int n = 0;
        const char* p = fmt + 3;
        while (*p >= '0' && *p <= '9') n = n * 10 + (*p++ - '0');
        int pos = snprintf(buf, buflen, "[%d]", n);
        if (n > 0 && arr->n_children > 0 && arr->children && arr->children[0] && arr->children[0]->buffers[1]) {
            struct ArrowArray* child = arr->children[0];
            int64_t start = (arr->offset + lr) * n;
            for (int i = 0; i < n && i < 3 && (size_t)pos < buflen - 10; i++) {
                pos += snprintf(buf + pos, buflen - pos, "%s", i ? "; " : " ");
                double v = ((const double*)child->buffers[1])[start + i];
                pos += snprintf(buf + pos, buflen - pos, "%.3f", v);
            }
            if (n > 3 && (size_t)pos < buflen - 4) pos += snprintf(buf + pos, buflen - pos, "...");
        }
        return pos;
    }
    if (f == '+' && fmt[1] == 's') {  // struct
        return snprintf(buf, buflen, "{%d}", (int)arr->n_children);
    }
    if (f == 't' && fmt[1] == 'D') {  // duration
        char unit = fmt[2];
        int64_t v = ((const int64_t*)arr->buffers[1])[arr->offset + lr];
        int neg = v < 0; if (neg) v = -v;
        int64_t s, ms;
        if (unit == 'n') { s = v / 1000000000; ms = (v % 1000000000) / 1000000; }
        else if (unit == 'u') { s = v / 1000000; ms = (v % 1000000) / 1000; }
        else if (unit == 'm') { s = v / 1000; ms = v % 1000; }
        else { s = v; ms = 0; }
        return snprintf(buf, buflen, "%s%ld.%03ld s", neg ? "-" : "", (long)s, (long)ms);
    }
    buf[0] = '\0';
    return 0;
}

// | Format cell with struct support
static size_t format_cell_full(struct ArrowArray* arr, struct ArrowSchema* sch, int64_t lr, char* buf, size_t buflen, uint8_t decimals) {
    if (!arr || !sch || !sch->format) { buf[0] = '\0'; return 0; }
    const char* fmt = sch->format;

    if (fmt[0] == '+' && fmt[1] == 's') {
        int n = arr->n_children;
        if (!arr->children || !sch->children || n == 0) return snprintf(buf, buflen, "{%d}", n);
        int show = n > 3 ? 3 : n;
        int pos = snprintf(buf, buflen, "{%d}", n);
        for (int i = 0; i < show && (size_t)pos < buflen - 10; i++) {
            struct ArrowArray* ch = arr->children[i];
            struct ArrowSchema* cs = sch->children[i];
            if (!ch || !cs || !cs->format) continue;
            const char* nm = cs->name ? cs->name : "";
            pos += snprintf(buf + pos, buflen - pos, " %s=", nm);
            char cf = cs->format[0];
            if (cf == 'l' && ch->buffers[1]) {
                pos += snprintf(buf + pos, buflen - pos, "%ld", (long)((int64_t*)ch->buffers[1])[ch->offset + lr]);
            } else if ((cf == 'U' || cf == 'u') && ch->buffers[1] && ch->buffers[2]) {
                const int64_t* off = (cf == 'U') ? (int64_t*)ch->buffers[1] : NULL;
                const int32_t* off32 = (cf == 'u') ? (int32_t*)ch->buffers[1] : NULL;
                const char* data = (char*)ch->buffers[2];
                int64_t idx = ch->offset + lr;
                int64_t start = off ? off[idx] : off32[idx];
                int64_t len = off ? (off[idx+1] - start) : (off32[idx+1] - start);
                if (len > 12) len = 12;
                pos += snprintf(buf + pos, buflen - pos, "%.*s", (int)len, data + start);
            } else if (cf == 'b' && ch->buffers[1]) {
                int v = (((uint8_t*)ch->buffers[1])[(ch->offset + lr)/8] >> ((ch->offset + lr)%8)) & 1;
                pos += snprintf(buf + pos, buflen - pos, "%s", v ? "T" : "F");
            } else {
                pos += snprintf(buf + pos, buflen - pos, "?");
            }
        }
        return pos;
    }

    return format_cell_batch(arr, fmt, lr, buf, buflen, decimals);
}

// | Get column widths
lean_obj_res lean_qr_col_widths(b_lean_obj_arg qr_obj, lean_obj_arg world) {
    QueryResult* qr = (QueryResult*)lean_get_external_data(qr_obj);
    int64_t nc = qr->schema.n_children;
    int64_t nr = qr->total_rows;

    lean_object* arr = lean_alloc_array(nc, nc);
    for (int64_t c = 0; c < nc; c++) {
        const char* name = qr->schema.children[c]->name;
        size_t w = name ? strlen(name) : 0;
        for (int64_t r = 0; r < nr; r++) {
            CellInfo ci = get_cell(qr, r, c);
            if (!ci.arr) continue;
            size_t cw = format_cell_batch(ci.arr, ci.fmt, ci.lr, NULL, 0, 3);
            if (cw > w) w = cw;
        }
        lean_array_set_core(arr, c, lean_box(w));
    }
    return lean_io_result_mk_ok(arr);
}
