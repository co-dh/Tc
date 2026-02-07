/*
 * ADBC Core FFI - Generic ADBC interface for Lean 4
 * Dynamically loads ADBC driver via dlopen
 * Uses nanoarrow for type-safe Arrow array access
 */
#include <lean/lean.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdarg.h>
#include <dlfcn.h>
#include <time.h>
#include "nanoarrow.h"

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

/* Arrow C Data Interface structs (ArrowSchema, ArrowArray, ArrowArrayStream)
   are provided by nanoarrow.h */

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
    struct ArrowArrayView* views;     // [n_batches * n_children] nanoarrow views
    struct ArrowSchemaView* col_types; // [n_children] parsed column types
    int64_t n_children;
} QueryResult;

// | Finalize QueryResult
static void qr_finalize(void* p) {
    QueryResult* qr = (QueryResult*)p;
    int64_t nc = qr->n_children;
    if (qr->views) {
        for (int64_t i = 0; i < qr->n_batches * nc; i++)
            ArrowArrayViewReset(&qr->views[i]);
        free(qr->views);
    }
    free(qr->col_types);
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

    // Build nanoarrow views for typed access
    {
        int64_t nc = qr->schema.n_children;
        qr->n_children = nc;

        qr->col_types = calloc(nc, sizeof(struct ArrowSchemaView));
        for (int64_t c = 0; c < nc; c++) {
            if (ArrowSchemaViewInit(&qr->col_types[c], qr->schema.children[c], NULL) != NANOARROW_OK)
                log_msg("[adbc] ArrowSchemaViewInit failed col=%ld\n", (long)c);
        }

        qr->views = calloc(qr->n_batches * nc, sizeof(struct ArrowArrayView));
        for (int64_t b = 0; b < qr->n_batches; b++) {
            for (int64_t c = 0; c < nc; c++) {
                int64_t idx = b * nc + c;
                if (ArrowArrayViewInitFromSchema(&qr->views[idx], qr->schema.children[c], NULL) != NANOARROW_OK) {
                    log_msg("[adbc] ArrowArrayViewInitFromSchema failed b=%ld c=%ld\n", (long)b, (long)c);
                    continue;
                }
                if (ArrowArrayViewSetArray(&qr->views[idx], qr->batches[b].children[c], NULL) != NANOARROW_OK)
                    log_msg("[adbc] ArrowArrayViewSetArray failed b=%ld c=%ld\n", (long)b, (long)c);
            }
        }
    }

    return lean_io_result_mk_ok(lean_alloc_external(get_qr_class(), qr));

fail:
    ;lean_object* err_str = lean_mk_string(fail_msg ? fail_msg : "unknown error");
    if (qr) { free(qr->batches); free(qr->prefix); free(qr); }
    if (stream.release) stream.release(&stream);
    if (stmt.private_data) pAdbcStatementRelease(&stmt, &err);
    free_error(&err);
    return lean_io_result_mk_error(lean_mk_io_user_error(err_str));
    #undef QUERY_CHECK
    #undef STREAM_CHECK
}

// | Get column count
lean_obj_res lean_qr_ncols(b_lean_obj_arg qr_obj, lean_obj_arg world) {
    QueryResult* qr = (QueryResult*)lean_get_external_data(qr_obj);
    return lean_io_result_mk_ok(lean_box_uint64((uint64_t)qr->n_children));
}

// | Get row count
lean_obj_res lean_qr_nrows(b_lean_obj_arg qr_obj, lean_obj_arg world) {
    QueryResult* qr = (QueryResult*)lean_get_external_data(qr_obj);
    return lean_io_result_mk_ok(lean_box_uint64((uint64_t)qr->total_rows));
}

// | Get column name
lean_obj_res lean_qr_col_name(b_lean_obj_arg qr_obj, uint64_t col, lean_obj_arg world) {
    QueryResult* qr = (QueryResult*)lean_get_external_data(qr_obj);
    if ((int64_t)col >= qr->n_children) return lean_io_result_mk_ok(lean_mk_string(""));
    const char* name = qr->schema.children[col]->name;
    return lean_io_result_mk_ok(lean_mk_string(name ? name : ""));
}

// | Get column format (Arrow type string)
lean_obj_res lean_qr_col_fmt(b_lean_obj_arg qr_obj, uint64_t col, lean_obj_arg world) {
    QueryResult* qr = (QueryResult*)lean_get_external_data(qr_obj);
    if ((int64_t)col >= qr->n_children) return lean_io_result_mk_ok(lean_mk_string(""));
    const char* fmt = qr->schema.children[col]->format;
    return lean_io_result_mk_ok(lean_mk_string(fmt ? fmt : ""));
}

// | Get column type name (e.g. "time", "timestamp", "i64", "f64", "str", "bool", "date", "list", "struct")
lean_obj_res lean_qr_col_type(b_lean_obj_arg qr_obj, uint64_t col, lean_obj_arg world) {
    QueryResult* qr = (QueryResult*)lean_get_external_data(qr_obj);
    if ((int64_t)col >= qr->n_children) return lean_io_result_mk_ok(lean_mk_string("?"));
    enum ArrowType t = qr->col_types[col].type;
    const char* s;
    switch (t) {
    case NANOARROW_TYPE_INT8: case NANOARROW_TYPE_INT16: case NANOARROW_TYPE_INT32: case NANOARROW_TYPE_INT64:
    case NANOARROW_TYPE_UINT8: case NANOARROW_TYPE_UINT16: case NANOARROW_TYPE_UINT32: case NANOARROW_TYPE_UINT64:
        s = "int"; break;
    case NANOARROW_TYPE_FLOAT: case NANOARROW_TYPE_DOUBLE: case NANOARROW_TYPE_HALF_FLOAT:
        s = "float"; break;
    case NANOARROW_TYPE_STRING: case NANOARROW_TYPE_LARGE_STRING:
        s = "str"; break;
    case NANOARROW_TYPE_BINARY: case NANOARROW_TYPE_LARGE_BINARY: case NANOARROW_TYPE_FIXED_SIZE_BINARY:
        s = "binary"; break;
    case NANOARROW_TYPE_BOOL:
        s = "bool"; break;
    case NANOARROW_TYPE_DATE32: case NANOARROW_TYPE_DATE64:
        s = "date"; break;
    case NANOARROW_TYPE_TIME32: case NANOARROW_TYPE_TIME64:
        s = "time"; break;
    case NANOARROW_TYPE_TIMESTAMP:
        s = "timestamp"; break;
    case NANOARROW_TYPE_DURATION:
        s = "duration"; break;
    case NANOARROW_TYPE_DECIMAL128: case NANOARROW_TYPE_DECIMAL256:
    case NANOARROW_TYPE_DECIMAL32: case NANOARROW_TYPE_DECIMAL64:
        s = "decimal"; break;
    case NANOARROW_TYPE_LIST: case NANOARROW_TYPE_LARGE_LIST: case NANOARROW_TYPE_FIXED_SIZE_LIST:
        s = "list"; break;
    case NANOARROW_TYPE_STRUCT:
        s = "struct"; break;
    default:
        s = "?"; break;
    }
    return lean_io_result_mk_ok(lean_mk_string(s));
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

typedef struct {
    struct ArrowArrayView* view;
    struct ArrowSchemaView* sv;
    int64_t lr;
} CellInfo;

static CellInfo get_cell(QueryResult* qr, int64_t row, int64_t col) {
    CellInfo ci = {NULL, NULL, 0};
    int64_t bi;
    if (!find_batch(qr, row, &bi, &ci.lr)) return ci;
    if (col >= qr->n_children) return ci;
    ci.view = &qr->views[bi * qr->n_children + col];
    ci.sv = &qr->col_types[col];
    return ci;
}

/* === Cell Formatting === */

// | Format a cell value using nanoarrow typed accessors
// | Convert ArrowDecimal to double using its scale
static double decimal_to_double(const struct ArrowDecimal* d) {
    double scale = 1.0;
    for (int i = 0; i < d->scale; i++) scale *= 10.0;
    if (d->n_words <= 1) {
        return (double)ArrowDecimalGetIntUnsafe(d) / scale;
    }
    // 128-bit: (int64_t)hi * 2^64 + (uint64_t)lo
    int64_t hi = (int64_t)d->words[d->high_word_index];
    uint64_t lo = d->words[d->low_word_index];
    double val = (double)hi * 18446744073709551616.0 + (double)lo;
    return val / scale;
}

static size_t format_cell_view(const struct ArrowArrayView* view,
    const struct ArrowSchemaView* sv, int64_t lr, char* buf, size_t buflen, uint8_t decimals)
{
    if (ArrowArrayViewIsNull(view, lr)) { buf[0] = '\0'; return 0; }

    switch (sv->type) {
    case NANOARROW_TYPE_INT8: case NANOARROW_TYPE_INT16: case NANOARROW_TYPE_INT32: case NANOARROW_TYPE_INT64:
        return snprintf(buf, buflen, "%ld", (long)ArrowArrayViewGetIntUnsafe(view, lr));

    case NANOARROW_TYPE_UINT8: case NANOARROW_TYPE_UINT16: case NANOARROW_TYPE_UINT32: case NANOARROW_TYPE_UINT64:
        return snprintf(buf, buflen, "%lu", (unsigned long)ArrowArrayViewGetUIntUnsafe(view, lr));

    case NANOARROW_TYPE_FLOAT: case NANOARROW_TYPE_DOUBLE: case NANOARROW_TYPE_HALF_FLOAT:
        return snprintf(buf, buflen, "%.*f", decimals, ArrowArrayViewGetDoubleUnsafe(view, lr));

    case NANOARROW_TYPE_BOOL:
        return snprintf(buf, buflen, "%s", ArrowArrayViewGetIntUnsafe(view, lr) ? "true" : "false");

    case NANOARROW_TYPE_STRING: case NANOARROW_TYPE_LARGE_STRING: {
        struct ArrowStringView sv2 = ArrowArrayViewGetStringUnsafe(view, lr);
        int64_t len = sv2.size_bytes;
        if ((size_t)len >= buflen) len = buflen - 1;
        memcpy(buf, sv2.data, len);
        buf[len] = '\0';
        return len;
    }

    case NANOARROW_TYPE_BINARY: case NANOARROW_TYPE_LARGE_BINARY: {
        struct ArrowBufferView bv = ArrowArrayViewGetBytesUnsafe(view, lr);
        int show = bv.size_bytes > 8 ? 8 : (int)bv.size_bytes;
        int pos = 0;
        for (int i = 0; i < show && (size_t)pos < buflen - 3; i++)
            pos += snprintf(buf + pos, buflen - pos, "%02x", bv.data.as_uint8[i]);
        if (bv.size_bytes > 8 && (size_t)pos < buflen - 3)
            pos += snprintf(buf + pos, buflen - pos, "..");
        return pos;
    }

    case NANOARROW_TYPE_DATE32: {
        int32_t days = (int32_t)ArrowArrayViewGetIntUnsafe(view, lr);
        time_t secs = (time_t)days * 86400;
        struct tm* tm = gmtime(&secs);
        return snprintf(buf, buflen, "%04d-%02d-%02d", tm->tm_year + 1900, tm->tm_mon + 1, tm->tm_mday);
    }

    case NANOARROW_TYPE_DATE64: {
        int64_t ms = ArrowArrayViewGetIntUnsafe(view, lr);
        time_t secs = ms / 1000;
        struct tm* tm = gmtime(&secs);
        return snprintf(buf, buflen, "%04d-%02d-%02d", tm->tm_year + 1900, tm->tm_mon + 1, tm->tm_mday);
    }

    case NANOARROW_TYPE_TIME32: case NANOARROW_TYPE_TIME64: {
        int64_t raw = ArrowArrayViewGetIntUnsafe(view, lr);
        int64_t s;
        switch (sv->time_unit) {
        case NANOARROW_TIME_UNIT_SECOND: s = raw; break;
        case NANOARROW_TIME_UNIT_MILLI:  s = raw / 1000; break;
        case NANOARROW_TIME_UNIT_MICRO:  s = raw / 1000000; break;
        case NANOARROW_TIME_UNIT_NANO:   s = raw / 1000000000; break;
        default: s = raw; break;
        }
        return snprintf(buf, buflen, "%02d:%02d:%02d", (int)((s/3600)%24), (int)((s/60)%60), (int)(s%60));
    }

    case NANOARROW_TYPE_TIMESTAMP: {
        int64_t raw = ArrowArrayViewGetIntUnsafe(view, lr);
        time_t secs;
        switch (sv->time_unit) {
        case NANOARROW_TIME_UNIT_SECOND: secs = raw; break;
        case NANOARROW_TIME_UNIT_MILLI:  secs = raw / 1000; break;
        case NANOARROW_TIME_UNIT_MICRO:  secs = raw / 1000000; break;
        case NANOARROW_TIME_UNIT_NANO:   secs = raw / 1000000000; break;
        default: secs = raw; break;
        }
        struct tm* tm = gmtime(&secs);
        return snprintf(buf, buflen, "%04d-%02d-%02d %02d:%02d:%02d",
                 tm->tm_year + 1900, tm->tm_mon + 1, tm->tm_mday, tm->tm_hour, tm->tm_min, tm->tm_sec);
    }

    case NANOARROW_TYPE_DURATION: {
        int64_t raw = ArrowArrayViewGetIntUnsafe(view, lr);
        int neg = raw < 0; if (neg) raw = -raw;
        int64_t s, ms;
        switch (sv->time_unit) {
        case NANOARROW_TIME_UNIT_NANO:   s = raw / 1000000000; ms = (raw % 1000000000) / 1000000; break;
        case NANOARROW_TIME_UNIT_MICRO:  s = raw / 1000000; ms = (raw % 1000000) / 1000; break;
        case NANOARROW_TIME_UNIT_MILLI:  s = raw / 1000; ms = raw % 1000; break;
        case NANOARROW_TIME_UNIT_SECOND: s = raw; ms = 0; break;
        default: s = raw; ms = 0; break;
        }
        return snprintf(buf, buflen, "%s%ld.%03ld s", neg ? "-" : "", (long)s, (long)ms);
    }

    case NANOARROW_TYPE_DECIMAL128: case NANOARROW_TYPE_DECIMAL256:
    case NANOARROW_TYPE_DECIMAL32: case NANOARROW_TYPE_DECIMAL64: {
        int bw = sv->type == NANOARROW_TYPE_DECIMAL32 ? 32
               : sv->type == NANOARROW_TYPE_DECIMAL64 ? 64
               : sv->type == NANOARROW_TYPE_DECIMAL128 ? 128 : 256;
        struct ArrowDecimal dec;
        ArrowDecimalInit(&dec, bw, sv->decimal_precision, sv->decimal_scale);
        ArrowArrayViewGetDecimalUnsafe(view, lr, &dec);
        double val = decimal_to_double(&dec);
        int dp = sv->decimal_scale > 0 ? sv->decimal_scale : decimals;
        return snprintf(buf, buflen, "%.*f", dp, val);
    }

    case NANOARROW_TYPE_LIST: case NANOARROW_TYPE_LARGE_LIST: {
        int64_t oi = view->offset + lr;
        int64_t start = ArrowArrayViewListChildOffset(view, oi);
        int64_t end = ArrowArrayViewListChildOffset(view, oi + 1);
        int64_t n = end - start;
        int pos = snprintf(buf, buflen, "[%ld]", (long)n);
        if (n > 0 && view->n_children > 0 && view->children[0]) {
            const struct ArrowArrayView* child = view->children[0];
            for (int64_t i = 0; i < n && i < 3 && (size_t)pos < buflen - 10; i++) {
                pos += snprintf(buf + pos, buflen - pos, "%s", i ? "; " : " ");
                if (child->storage_type == NANOARROW_TYPE_DOUBLE || child->storage_type == NANOARROW_TYPE_FLOAT)
                    pos += snprintf(buf + pos, buflen - pos, "%.3f", ArrowArrayViewGetDoubleUnsafe(child, start + i));
                else
                    pos += snprintf(buf + pos, buflen - pos, "%ld", (long)ArrowArrayViewGetIntUnsafe(child, start + i));
            }
            if (n > 3 && (size_t)pos < buflen - 4) pos += snprintf(buf + pos, buflen - pos, "...");
        }
        return pos;
    }

    case NANOARROW_TYPE_FIXED_SIZE_LIST: {
        int64_t n = view->layout.child_size_elements;
        int pos = snprintf(buf, buflen, "[%ld]", (long)n);
        if (n > 0 && view->n_children > 0 && view->children[0]) {
            const struct ArrowArrayView* child = view->children[0];
            int64_t start = (view->offset + lr) * n;
            for (int64_t i = 0; i < n && i < 3 && (size_t)pos < buflen - 10; i++) {
                pos += snprintf(buf + pos, buflen - pos, "%s", i ? "; " : " ");
                pos += snprintf(buf + pos, buflen - pos, "%.3f", ArrowArrayViewGetDoubleUnsafe(child, start + i));
            }
            if (n > 3 && (size_t)pos < buflen - 4) pos += snprintf(buf + pos, buflen - pos, "...");
        }
        return pos;
    }

    case NANOARROW_TYPE_STRUCT:
        return snprintf(buf, buflen, "{%ld}", (long)view->n_children);

    default:
        buf[0] = '\0';
        return 0;
    }
}

// | Format struct cell with child field preview
static size_t format_struct_cell(const struct ArrowArrayView* view,
    const struct ArrowSchema* sch, int64_t lr, char* buf, size_t buflen, uint8_t decimals)
{
    if (ArrowArrayViewIsNull(view, lr)) { buf[0] = '\0'; return 0; }
    int64_t n = view->n_children;
    if (!view->children || !sch->children || n == 0)
        return snprintf(buf, buflen, "{%ld}", (long)n);

    int show = n > 3 ? 3 : (int)n;
    int pos = snprintf(buf, buflen, "{%ld}", (long)n);

    for (int i = 0; i < show && (size_t)pos < buflen - 10; i++) {
        struct ArrowArrayView* ch = view->children[i];
        struct ArrowSchema* cs = sch->children[i];
        if (!ch || !cs) continue;
        const char* nm = cs->name ? cs->name : "";
        pos += snprintf(buf + pos, buflen - pos, " %s=", nm);

        if (ArrowArrayViewIsNull(ch, lr)) {
            pos += snprintf(buf + pos, buflen - pos, "null");
            continue;
        }

        struct ArrowSchemaView csv;
        if (ArrowSchemaViewInit(&csv, cs, NULL) != NANOARROW_OK) {
            pos += snprintf(buf + pos, buflen - pos, "?");
            continue;
        }

        char tmp[32];
        format_cell_view(ch, &csv, lr, tmp, sizeof(tmp), decimals);
        pos += snprintf(buf + pos, buflen - pos, "%s", tmp);
    }
    return pos;
}

// | Get cell as string
lean_obj_res lean_qr_cell_str(b_lean_obj_arg qr_obj, uint64_t row, uint64_t col, lean_obj_arg world) {
    QueryResult* qr = (QueryResult*)lean_get_external_data(qr_obj);
    CellInfo c = get_cell(qr, row, col);
    if (!c.view || !c.view->array) return lean_io_result_mk_ok(lean_mk_string(""));
    char buf[CELL_BUF_SIZE];
    if (c.sv->type == NANOARROW_TYPE_STRUCT)
        format_struct_cell(c.view, qr->schema.children[col], c.lr, buf, sizeof(buf), 3);
    else
        format_cell_view(c.view, c.sv, c.lr, buf, sizeof(buf), 3);
    return lean_io_result_mk_ok(lean_mk_string(buf));
}

// | Get cell as Int
lean_obj_res lean_qr_cell_int(b_lean_obj_arg qr_obj, uint64_t row, uint64_t col, lean_obj_arg world) {
    QueryResult* qr = (QueryResult*)lean_get_external_data(qr_obj);
    CellInfo c = get_cell(qr, row, col);
    if (!c.view || !c.view->array || ArrowArrayViewIsNull(c.view, c.lr))
        return lean_io_result_mk_ok(lean_int64_to_int(0));
    int64_t val = ArrowArrayViewGetIntUnsafe(c.view, c.lr);
    return lean_io_result_mk_ok(lean_int64_to_int(val));
}

// | Get cell as Float (handles DECIMAL via ArrowDecimal conversion)
lean_obj_res lean_qr_cell_float(b_lean_obj_arg qr_obj, uint64_t row, uint64_t col, lean_obj_arg world) {
    QueryResult* qr = (QueryResult*)lean_get_external_data(qr_obj);
    CellInfo c = get_cell(qr, row, col);
    if (!c.view || !c.view->array || ArrowArrayViewIsNull(c.view, c.lr))
        return lean_io_result_mk_ok(lean_box_float(0.0));
    double val;
    enum ArrowType t = c.sv->type;
    if (t == NANOARROW_TYPE_DECIMAL32 || t == NANOARROW_TYPE_DECIMAL64 ||
        t == NANOARROW_TYPE_DECIMAL128 || t == NANOARROW_TYPE_DECIMAL256) {
        int bw = t == NANOARROW_TYPE_DECIMAL32 ? 32
               : t == NANOARROW_TYPE_DECIMAL64 ? 64
               : t == NANOARROW_TYPE_DECIMAL128 ? 128 : 256;
        struct ArrowDecimal dec;
        ArrowDecimalInit(&dec, bw, c.sv->decimal_precision, c.sv->decimal_scale);
        ArrowArrayViewGetDecimalUnsafe(c.view, c.lr, &dec);
        val = decimal_to_double(&dec);
    } else {
        val = ArrowArrayViewGetDoubleUnsafe(c.view, c.lr);
    }
    return lean_io_result_mk_ok(lean_box_float(val));
}

// | Check if cell is null
lean_obj_res lean_qr_cell_is_null(b_lean_obj_arg qr_obj, uint64_t row, uint64_t col, lean_obj_arg world) {
    QueryResult* qr = (QueryResult*)lean_get_external_data(qr_obj);
    CellInfo c = get_cell(qr, row, col);
    return lean_io_result_mk_ok(lean_box(!c.view || !c.view->array || ArrowArrayViewIsNull(c.view, c.lr) ? 1 : 0));
}
