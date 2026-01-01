/*
 * ADBC FFI shim for Lean 4
 * Dynamically loads libduckdb.so to avoid glibc version conflicts
 */
#include <lean/lean.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdarg.h>
#include <dlfcn.h>
#include <time.h>
#include <termbox2.h>

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

/* === Logging to file === */
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

/* === Helper: init error struct === */
static void init_error(struct AdbcError* err) {
    memset(err, 0, sizeof(*err));
}

/* === Helper: free error if needed === */
static void free_error(struct AdbcError* err) {
    if (err->release) err->release(err);
}

/* === Lean FFI Functions === */

// | Load ADBC functions from libduckdb.so
static int load_adbc_funcs(void) {
    const char* paths[] = {"/usr/lib/libduckdb.so", "/usr/local/lib/libduckdb.so", "libduckdb.so", NULL};
    for (int i = 0; paths[i]; i++) {
        g_lib = dlopen(paths[i], RTLD_NOW | RTLD_GLOBAL);
        if (g_lib) { log_msg("[adbc] loaded %s\n", paths[i]); break; }
    }
    if (!g_lib) { log_msg("[adbc] dlopen failed: %s\n", dlerror()); return 0; }
    for (int i = 0; i < FN_COUNT; i++) {
        g_fn[i] = dlsym(g_lib, g_fn_names[i]);
        if (!g_fn[i]) return 0;
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

// | Init ADBC (in-memory DuckDB)
lean_obj_res lean_adbc_init(lean_obj_arg world) {
    if (g_initialized) return lean_io_result_mk_ok(lean_box(1));
    if (!load_adbc_funcs()) { log_msg("[adbc] load_adbc_funcs failed\n"); return lean_io_result_mk_ok(lean_box(0)); }

    struct AdbcError err;
    init_error(&err);
    int have_db = 0, have_conn = 0;

    ADBC_CHECK(pAdbcDatabaseNew(&g_db, &err), "DatabaseNew");
    have_db = 1;
    ADBC_CHECK(pAdbcDatabaseSetOption(&g_db, "driver", "/usr/lib/libduckdb.so", &err), "SetOption(driver)");
    ADBC_CHECK(pAdbcDatabaseSetOption(&g_db, "entrypoint", "duckdb_adbc_init", &err), "SetOption(entrypoint)");
    ADBC_CHECK(pAdbcDatabaseSetOption(&g_db, "path", "", &err), "SetOption(path)");
    ADBC_CHECK(pAdbcDatabaseInit(&g_db, &err), "DatabaseInit");
    ADBC_CHECK(pAdbcConnectionNew(&g_conn, &err), "ConnectionNew");
    have_conn = 1;
    ADBC_CHECK(pAdbcConnectionInit(&g_conn, &g_db, &err), "ConnectionInit");

    log_msg("[adbc] initialized OK\n");
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
    if (!g_initialized) {
        return lean_io_result_mk_ok(lean_box(0));
    }

    struct AdbcError err;
    init_error(&err);

    pAdbcConnectionRelease(&g_conn, &err);
    pAdbcDatabaseRelease(&g_db, &err);
    free_error(&err);

    g_initialized = 0;
    memset(&g_conn, 0, sizeof(g_conn));
    memset(&g_db, 0, sizeof(g_db));

    if (g_lib) {
        dlclose(g_lib);
        g_lib = NULL;
    }

    return lean_io_result_mk_ok(lean_box(0));
}

/* === Query Result (opaque to Lean) === */

typedef struct {
    struct ArrowSchema schema;
    struct ArrowArray* batches;
    int64_t* prefix;      // prefix[i] = sum of rows in batches[0..i-1], prefix[n_batches] = total
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

// | Foreach noop
static void qr_foreach(void* p, b_lean_obj_arg f) { (void)p; (void)f; }

static lean_external_class* g_qr_class = NULL;

static lean_external_class* get_qr_class(void) {
    if (!g_qr_class) {
        g_qr_class = lean_register_external_class(qr_finalize, qr_foreach);
    }
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
    if ((int64_t)col >= qr->schema.n_children) {
        return lean_io_result_mk_ok(lean_mk_string(""));
    }
    const char* name = qr->schema.children[col]->name;
    return lean_io_result_mk_ok(lean_mk_string(name ? name : ""));
}

// | Get column format (Arrow type string)
lean_obj_res lean_qr_col_fmt(b_lean_obj_arg qr_obj, uint64_t col, lean_obj_arg world) {
    QueryResult* qr = (QueryResult*)lean_get_external_data(qr_obj);
    if ((int64_t)col >= qr->schema.n_children) {
        return lean_io_result_mk_ok(lean_mk_string(""));
    }
    const char* fmt = qr->schema.children[col]->format;
    return lean_io_result_mk_ok(lean_mk_string(fmt ? fmt : ""));
}

/* === Cell Access Helpers === */

// | Find batch and local row for global row index (binary search on prefix sums)
static int find_batch(QueryResult* qr, int64_t row, int64_t* batch_idx, int64_t* local_row) {
    if (row < 0 || row >= qr->total_rows) return 0;
    // binary search: find largest i where prefix[i] <= row
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

// | Check if cell is null
static int is_null(struct ArrowArray* arr, int64_t row) {
    if (arr->null_count == 0) return 0;
    if (arr->buffers[0] == NULL) return 0;
    const uint8_t* validity = (const uint8_t*)arr->buffers[0];
    int64_t idx = arr->offset + row;
    return !(validity[idx / 8] & (1 << (idx % 8)));
}

// | Cell info: batch array + format + local row (NULL arr if invalid)
typedef struct { struct ArrowArray* arr; const char* fmt; int64_t lr; } CellInfo;

// | Get cell info (returns NULL arr if out of bounds)
static CellInfo get_cell(QueryResult* qr, int64_t row, int64_t col) {
    CellInfo ci = {NULL, NULL, 0};
    int64_t bi;
    if (!find_batch(qr, row, &bi, &ci.lr)) return ci;
    if (col >= qr->schema.n_children) return ci;
    ci.arr = qr->batches[bi].children[col];
    ci.fmt = qr->schema.children[col]->format;
    return ci;
}

// forward decl
static size_t format_cell_batch(struct ArrowArray* arr, const char* fmt, int64_t lr, char* buf, size_t buflen, uint8_t decimals);

// | Get cell as string (uses format_cell_batch, 3 decimal places)
lean_obj_res lean_qr_cell_str(b_lean_obj_arg qr_obj, uint64_t row, uint64_t col, lean_obj_arg world) {
    QueryResult* qr = (QueryResult*)lean_get_external_data(qr_obj);
    CellInfo c = get_cell(qr, row, col);
    if (!c.arr) return lean_io_result_mk_ok(lean_mk_string(""));
    char buf[CELL_BUF_SIZE];
    format_cell_batch(c.arr, c.fmt, c.lr, buf, sizeof(buf), 3);
    return lean_io_result_mk_ok(lean_mk_string(buf));
}

// | Get cell as Int (0 for null/non-int)
lean_obj_res lean_qr_cell_int(b_lean_obj_arg qr_obj, uint64_t row, uint64_t col, lean_obj_arg world) {
    QueryResult* qr = (QueryResult*)lean_get_external_data(qr_obj);
    CellInfo c = get_cell(qr, row, col);
    if (!c.arr || is_null(c.arr, c.lr)) return lean_io_result_mk_ok(lean_int64_to_int(0));
    int64_t val = 0;
    if (c.fmt[0] == 'l')      val = ((const int64_t*)c.arr->buffers[1])[c.arr->offset + c.lr];
    else if (c.fmt[0] == 'i') val = ((const int32_t*)c.arr->buffers[1])[c.arr->offset + c.lr];
    else if (c.fmt[0] == 's') val = ((const int16_t*)c.arr->buffers[1])[c.arr->offset + c.lr];
    else if (c.fmt[0] == 'c') val = ((const int8_t*)c.arr->buffers[1])[c.arr->offset + c.lr];
    return lean_io_result_mk_ok(lean_int64_to_int(val));
}

// | Get cell as Float (0.0 for null/non-float)
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

// | Format cell or compute length (buf=NULL for length only)
//
// Arrow buffer layout:
//   buffers[0]: validity bitmap (1 bit per row, 0=null)
//   buffers[1]: data (fixed-width) or offsets (variable-length)
//   buffers[2]: data bytes (variable-length only: utf8, binary)
//
// arr: column ArrowArray from batch
// fmt: Arrow format string (l=int64, u=utf8, g=float64, etc.)
// lr: local row index within batch
// buf: output buffer (NULL = length only mode)
// buflen: buffer size (ignored if buf=NULL)
// decimals: decimal places for floats
// returns: length written/needed
static size_t format_cell_batch(struct ArrowArray* arr, const char* fmt, int64_t lr, char* buf, size_t buflen, uint8_t decimals) {
    char tmp[CELL_BUF_SIZE];
    if (!buf) { buf = tmp; buflen = sizeof(tmp); }  // length-only mode
    if (is_null(arr, lr)) { buf[0] = '\0'; return 0; }

    if (fmt[0] == 'l') {  // int64
        return snprintf(buf, buflen, "%ld", ((const int64_t*)arr->buffers[1])[arr->offset + lr]);
    }
    if (fmt[0] == 'i') {  // int32
        return snprintf(buf, buflen, "%d", ((const int32_t*)arr->buffers[1])[arr->offset + lr]);
    }
    if (fmt[0] == 's') {  // int16
        return snprintf(buf, buflen, "%d", ((const int16_t*)arr->buffers[1])[arr->offset + lr]);
    }
    if (fmt[0] == 'c') {  // int8
        return snprintf(buf, buflen, "%d", ((const int8_t*)arr->buffers[1])[arr->offset + lr]);
    }
    if (fmt[0] == 'g') {  // float64
        return snprintf(buf, buflen, "%.*f", decimals, ((const double*)arr->buffers[1])[arr->offset + lr]);
    }
    if (fmt[0] == 'f') {  // float32
        return snprintf(buf, buflen, "%.*f", decimals, ((const float*)arr->buffers[1])[arr->offset + lr]);
    }
    if (fmt[0] == 'u' || fmt[0] == 'z') {  // utf8, binary
        const int32_t* off = (const int32_t*)arr->buffers[1];
        const char* data = (const char*)arr->buffers[2];
        int64_t idx = arr->offset + lr;
        int32_t len = off[idx + 1] - off[idx];
        if ((size_t)len >= buflen) len = buflen - 1;
        memcpy(buf, data + off[idx], len);
        buf[len] = '\0';
        return len;
    }
    if (fmt[0] == 'U' || fmt[0] == 'Z') {  // large utf8
        const int64_t* off = (const int64_t*)arr->buffers[1];
        const char* data = (const char*)arr->buffers[2];
        int64_t idx = arr->offset + lr;
        int64_t len = off[idx + 1] - off[idx];
        if ((size_t)len >= buflen) len = buflen - 1;
        memcpy(buf, data + off[idx], len);
        buf[len] = '\0';
        return len;
    }
    if (fmt[0] == 'b') {  // bool
        const uint8_t* data = (const uint8_t*)arr->buffers[1];
        int64_t idx = arr->offset + lr;
        return snprintf(buf, buflen, "%s", ((data[idx/8] >> (idx%8)) & 1) ? "true" : "false");
    }
    if (fmt[0] == 'd' && fmt[1] == ':') {  // decimal
        int scale = 0;
        const char* p = fmt + 2;
        while (*p && *p != ',') p++;
        if (*p == ',') { p++; while (*p >= '0' && *p <= '9') scale = scale * 10 + (*p++ - '0'); }
        double val = (double)((const int64_t*)arr->buffers[1])[(arr->offset + lr) * 2];
        for (int i = 0; i < scale; i++) val /= 10.0;
        return snprintf(buf, buflen, "%.*f", scale, val);
    }
    if (fmt[0] == 't' && fmt[1] == 's') {  // timestamp (us since epoch)
        int64_t us = ((const int64_t*)arr->buffers[1])[arr->offset + lr];
        time_t secs = us / 1000000;
        struct tm* tm = gmtime(&secs);
        return snprintf(buf, buflen, "%04d-%02d-%02d %02d:%02d:%02d",
                 tm->tm_year + 1900, tm->tm_mon + 1, tm->tm_mday, tm->tm_hour, tm->tm_min, tm->tm_sec);
    }
    if (fmt[0] == 't' && fmt[1] == 't') {  // time (us since midnight)
        int64_t s = ((const int64_t*)arr->buffers[1])[arr->offset + lr] / 1000000;
        return snprintf(buf, buflen, "%02d:%02d:%02d", (int)((s/3600)%24), (int)((s/60)%60), (int)(s%60));
    }
    buf[0] = '\0';
    return 0;
}

// | Get column widths (max of header and all cells, capped at 50)
lean_obj_res lean_qr_col_widths(b_lean_obj_arg qr_obj, lean_obj_arg world) {
    QueryResult* qr = (QueryResult*)lean_get_external_data(qr_obj);
    int64_t nc = qr->schema.n_children;
    int64_t nr = qr->total_rows;

    // Alloc Lean array
    lean_object* arr = lean_alloc_array(nc, nc);

    for (int64_t c = 0; c < nc; c++) {
        // Start with header width
        const char* name = qr->schema.children[c]->name;
        size_t w = name ? strlen(name) : 0;

        // Scan all rows for max width (default decimals=3)
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

/* === Direct Terminal Rendering === */

// | termbox2 attributes
#define TB_BOLD      0x01000000
#define TB_UNDERLINE 0x02000000
#define HEADER_ATTR  (TB_BOLD | TB_UNDERLINE)

// | Print string at (x,y) with padding to width w
static void tb_print_pad(int x, int y, int w, uint32_t fg, uint32_t bg, const char* s, int right_align) {
    int slen = 0;
    for (const char* p = s; *p; p++) slen++;
    int pad = w - slen;
    if (pad < 0) pad = 0;
    int i = 0;
    // left pad for right-align
    if (right_align) for (; i < pad && x + i < tb_width(); i++) tb_set_cell(x + i, y, ' ', fg, bg);
    // string
    for (const char* p = s; *p && x + i < tb_width(); p++, i++) tb_set_cell(x + i, y, (uint32_t)*p, fg, bg);
    // right pad for left-align
    if (!right_align) for (; i < w && x + i < tb_width(); i++) tb_set_cell(x + i, y, ' ', fg, bg);
}

// | Check if column is numeric (right-align)
static int is_num_fmt(char f) { return f == 'l' || f == 'i' || f == 's' || f == 'c' || f == 'g' || f == 'f' || f == 'd'; }

// | Find first batch containing row r0 (binary search), sets *offset to batch start
static int64_t find_batch_start(QueryResult* qr, int64_t r0, int64_t* offset) {
    if (r0 >= qr->total_rows) { *offset = qr->total_rows; return qr->n_batches; }
    int64_t lo = 0, hi = qr->n_batches;
    while (lo < hi) {
        int64_t mid = lo + (hi - lo + 1) / 2;
        if (qr->prefix[mid] <= r0) lo = mid;
        else hi = mid - 1;
    }
    *offset = qr->prefix[lo];
    return lo;
}

// | Compute column width (header + visible rows, capped)
static int col_width(QueryResult* qr, int64_t col, int64_t r0, int64_t r1, int maxW, uint8_t decimals) {
    const char* name = qr->schema.children[col]->name;
    const char* fmt = qr->schema.children[col]->format;
    int w = name ? (int)strlen(name) : 0;
    int64_t batch_off;
    int64_t bi = find_batch_start(qr, r0, &batch_off);
    for (; bi < qr->n_batches && batch_off < r1; bi++) {
        struct ArrowArray* batch = &qr->batches[bi];
        struct ArrowArray* arr = batch->children[col];
        int64_t lr0 = (r0 > batch_off) ? r0 - batch_off : 0;
        int64_t lr1 = (r1 < batch_off + batch->length) ? r1 - batch_off : batch->length;
        for (int64_t lr = lr0; lr < lr1; lr++) {
            int cw = (int)format_cell_batch(arr, fmt, lr, NULL, 0, decimals);
            if (cw > w) w = cw;
            if (w >= maxW) return maxW;
        }
        batch_off += batch->length;
    }
    return w < maxW ? w : maxW;
}

// | Style indices for styles array
#define STYLE_CURSOR     0   // cursor cell
#define STYLE_SEL_ROW    1   // selected row
#define STYLE_SEL_CUR    2   // selected col + cursor row
#define STYLE_SEL_COL    3   // selected col
#define STYLE_CUR_ROW    4   // cursor row
#define STYLE_CUR_COL    5   // cursor col
#define STYLE_DEFAULT    6   // default
#define NUM_STYLES       7

// | Column layout info for rendering
typedef struct { int64_t idx; int x, w; } ColInfo;

// | Build column selection bitset (max 256 cols)
static void build_col_bitset(b_lean_obj_arg selColIdxs, size_t nSelCols, uint64_t* bits) {
    bits[0] = bits[1] = bits[2] = bits[3] = 0;
    for (size_t i = 0; i < nSelCols; i++) {
        size_t c = lean_unbox(lean_array_get_core(selColIdxs, i));
        if (c < 256) bits[c / 64] |= 1ULL << (c % 64);
    }
}
#define IS_COL_SEL(bits, col) ((col) < 256 && ((bits)[(col)/64] & (1ULL << ((col)%64))))

// | Build row selection bitset for visible range [r0, r1)
static uint64_t* build_row_bitset(b_lean_obj_arg selRows, size_t nSelRows, int64_t r0, int64_t r1) {
    int64_t n = r1 - r0;
    if (n <= 0) return NULL;
    size_t nwords = (n + 63) / 64;
    uint64_t* bits = calloc(nwords, sizeof(uint64_t));
    for (size_t i = 0; i < nSelRows; i++) {
        int64_t r = (int64_t)lean_unbox(lean_array_get_core(selRows, i));
        if (r >= r0 && r < r1) bits[(r - r0) / 64] |= 1ULL << ((r - r0) % 64);
    }
    return bits;
}
#define IS_ROW_SEL(bits, r, r0) ((bits) && ((bits)[((r)-(r0))/64] & (1ULL << (((r)-(r0))%64))))

// | Determine cell style index based on cursor/selection state
static int get_cell_style(int isCursor, int isSelRow, int isSel, int isCurRow, int isCurCol) {
    if (isCursor)               return STYLE_CURSOR;
    if (isSelRow)               return STYLE_SEL_ROW;
    if (isSel && isCurRow)      return STYLE_SEL_CUR;
    if (isSel)                  return STYLE_SEL_COL;
    if (isCurRow)               return STYLE_CUR_ROW;
    if (isCurCol)               return STYLE_CUR_COL;
    return STYLE_DEFAULT;
}


// | Render table batch-by-batch to terminal
// colIdxs: Array Nat (column indices in display order, key cols first)
// nKeyCols: number of key columns
// colOff: column offset (skip first colOff columns in display order)
// r0, r1: row range
// curRow, curCol: cursor
// selColIdxs: Array Nat (selected column indices)
// selRows: Array Nat (selected row indices)
// styles: Array UInt32 (14 values: 7 states × (fg|attrs, bg))
// maxWStr: max width for string columns
// maxWOther: max width for numeric/other columns
// decimals: decimal places
lean_obj_res lean_render_table(
    b_lean_obj_arg qr_obj,
    b_lean_obj_arg colIdxs,   // Array Nat
    uint64_t nKeyCols,        // number of key columns
    uint64_t colOff,          // column offset for horizontal scroll
    uint64_t r0, uint64_t r1,
    uint64_t curRow, uint64_t curCol,
    b_lean_obj_arg selColIdxs,// Array Nat (selected column indices)
    b_lean_obj_arg selRows,   // Array Nat
    b_lean_obj_arg styles,    // Array UInt32 (14 values)
    uint8_t maxWStr,          // max width for strings
    uint8_t maxWOther,        // max width for others
    uint8_t decimals,
    lean_obj_arg world) {

    QueryResult* qr = (QueryResult*)lean_get_external_data(qr_obj);
    size_t ncols = lean_array_size(colIdxs);
    size_t nSelCols = lean_array_size(selColIdxs);
    int screenW = tb_width();
    char buf[CELL_BUF_SIZE];

    // precompute selection bitsets
    uint64_t colBits[4];
    build_col_bitset(selColIdxs, nSelCols, colBits);
    uint64_t* rowBits = build_row_bitset(selRows, lean_array_size(selRows), r0, r1);

    // compute widths and x positions (start from colOff)
    size_t startCol = (colOff < ncols) ? colOff : ncols;
    ColInfo* ci = malloc(ncols * sizeof(ColInfo));
    int x = 0;
    size_t visCols = 0;
    for (size_t i = startCol; i < ncols && x < screenW; i++) {
        int64_t idx = lean_unbox(lean_array_get_core(colIdxs, i));
        if (idx >= qr->schema.n_children) continue;
        const char* fmt = qr->schema.children[idx]->format;
        int is_str = (fmt[0] == 'u' || fmt[0] == 'U' || fmt[0] == 'z' || fmt[0] == 'Z');
        int maxW = is_str ? maxWStr : maxWOther;
        int w = col_width(qr, idx, r0, r1, maxW, decimals);
        // truncate width if partially visible
        if (x + w > screenW) w = screenW - x;
        ci[visCols].idx = idx;
        ci[visCols].x = x;
        ci[visCols].w = w;
        x += w + 1;  // +1 for gap
        visCols++;
    }

    // separator position: after last visible key column
    int sepX = 0;
    size_t visKeys = (colOff < nKeyCols) ? nKeyCols - colOff : 0;
    if (visKeys > visCols) visKeys = visCols;
    if (visKeys > 0) sepX = ci[visKeys - 1].x + ci[visKeys - 1].w;

    // extract styles
    uint32_t stFg[NUM_STYLES], stBg[NUM_STYLES];
    for (int s = 0; s < NUM_STYLES; s++) {
        stFg[s] = lean_unbox_uint32(lean_array_get_core(styles, s * 2));
        stBg[s] = lean_unbox_uint32(lean_array_get_core(styles, s * 2 + 1));
    }

    // render header at y=0 and y=nRows+1 (bottom)
    int nDataRows = (int)(r1 - r0);
    int yBottom = nDataRows + 1;
    for (size_t c = 0; c < visCols; c++) {
        int64_t colIdx = ci[c].idx;
        const char* name = qr->schema.children[colIdx]->name;
        if (!name) name = "";
        int isSel = IS_COL_SEL(colBits, colIdx);
        int isCurCol = (colIdx == (int64_t)curCol);
        int si = isCurCol ? STYLE_CURSOR : (isSel ? STYLE_SEL_COL : STYLE_DEFAULT);
        uint32_t fg = stFg[si] | HEADER_ATTR;
        uint32_t bg = stBg[si];
        tb_print_pad(ci[c].x, 0, ci[c].w, fg, bg, name, 0);
        tb_print_pad(ci[c].x, yBottom, ci[c].w, fg, bg, name, 0);
    }
    // header separator
    if (sepX > 0) {
        tb_set_cell(sepX, 0, '|', stFg[STYLE_DEFAULT], stBg[STYLE_DEFAULT]);
        tb_set_cell(sepX, yBottom, '|', stFg[STYLE_DEFAULT], stBg[STYLE_DEFAULT]);
    }

    // find starting batch for r0
    int64_t batch_offset;
    int64_t bi_start = find_batch_start(qr, (int64_t)r0, &batch_offset);

    // iterate only needed batches
    for (int64_t bi = bi_start; bi < qr->n_batches && batch_offset < (int64_t)r1; bi++) {
        struct ArrowArray* batch = &qr->batches[bi];
        int64_t batch_end = batch_offset + batch->length;

        // local row range within batch
        int64_t lr0 = ((int64_t)r0 > batch_offset) ? (int64_t)r0 - batch_offset : 0;
        int64_t lr1 = ((int64_t)r1 < batch_end) ? (int64_t)r1 - batch_offset : batch->length;

        // for each visible column
        for (size_t c = 0; c < visCols; c++) {
            int64_t colIdx = ci[c].idx;
            int cx = ci[c].x, cw = ci[c].w;
            if (colIdx >= qr->schema.n_children) continue;

            struct ArrowArray* carr = batch->children[colIdx];
            const char* fmt = qr->schema.children[colIdx]->format;
            int is_num = is_num_fmt(fmt[0]);
            int isSel = IS_COL_SEL(colBits, colIdx);
            int isCurCol = (colIdx == (int64_t)curCol);

            // render each row in batch
            for (int64_t lr = lr0; lr < lr1; lr++) {
                int64_t rowIdx = batch_offset + lr;
                int y = (int)(rowIdx - r0 + 1);  // +1 for header
                int isSelRow = IS_ROW_SEL(rowBits, rowIdx, r0);
                int isCurRow = (rowIdx == (int64_t)curRow);
                int si = get_cell_style(isCurRow && isCurCol, isSelRow, isSel, isCurRow, isCurCol);
                uint32_t fg = stFg[si], bg = stBg[si];

                format_cell_batch(carr, fmt, lr, buf, sizeof(buf), decimals);
                tb_print_pad(cx, y, cw, fg, bg, buf, is_num);

                // draw separator (once per row, on first column)
                if (c == 0 && sepX > 0) {
                    tb_set_cell(sepX, y, '|', stFg[STYLE_DEFAULT], stBg[STYLE_DEFAULT]);
                }
            }
        }
        batch_offset = batch_end;
    }

    // build return array: Array (Nat × Nat × Nat) = Array (Nat × (Nat × Nat))
    lean_object* result = lean_alloc_array(visCols, visCols);
    for (size_t c = 0; c < visCols; c++) {
        lean_object* inner = lean_alloc_ctor(0, 2, 0);  // (x, w)
        lean_ctor_set(inner, 0, lean_box(ci[c].x));
        lean_ctor_set(inner, 1, lean_box(ci[c].w));
        lean_object* outer = lean_alloc_ctor(0, 2, 0);  // (idx, (x, w))
        lean_ctor_set(outer, 0, lean_box(ci[c].idx));
        lean_ctor_set(outer, 1, inner);
        lean_array_set_core(result, c, outer);
    }

    free(ci);
    free(rowBits);
    return lean_io_result_mk_ok(result);
}
