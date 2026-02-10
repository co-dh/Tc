/*
 * kdb FFI shim for Lean 4
 * Connects to kdb server via IPC using c.o
 */
#include <lean/lean.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "kdb/k.h"

/* === Constants === */
#define CELL_BUF_SIZE 128

/* === Global State === */
static I g_handle = 0;  // kdb connection handle

/* === QueryResult: wraps K object === */
typedef struct {
    K result;       // K object (table)
    K keys;         // column names (symbol list)
    K vals;         // column values (list of lists)
    int64_t nrows;
    int64_t ncols;
} QueryResult;

/* === External Class for Lean === */
static lean_external_class* g_qr_class = NULL;

static void qr_finalize(void* p) {
    QueryResult* qr = (QueryResult*)p;
    if (qr->result) r0(qr->result);  // release K object
    free(qr);
}

static void qr_foreach(void* p, b_lean_obj_arg f) {
    // no nested Lean objects
}

static lean_external_class* get_qr_class(void) {
    if (!g_qr_class) {
        g_qr_class = lean_register_external_class(qr_finalize, qr_foreach);
    }
    return g_qr_class;
}

/* === Connect to kdb server === */
lean_obj_res lean_kdb_connect(b_lean_obj_arg host_obj, uint16_t port, lean_obj_arg world) {
    const char* host = lean_string_cstr(host_obj);
    g_handle = khp((S)host, (I)port);
    return lean_io_result_mk_ok(lean_box(g_handle > 0 ? 1 : 0));
}

/* === Disconnect === */
lean_obj_res lean_kdb_disconnect(lean_obj_arg world) {
    if (g_handle > 0) {
        kclose(g_handle);
        g_handle = 0;
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/* === Check if K is a table === */
static int is_table(K x) { return x && x->t == XT; }

/* === Check if K is a keyed table (dict of tables) === */
static int is_keyed(K x) { return x && x->t == XD && kK(x)[0]->t == XT; }

/* === Check if K is atom (negative type) === */
static int is_atom(K x) { return x && x->t < 0 && x->t > -20; }

/* === Check if K is vector (0 < t < 20) === */
static int is_vec(K x) { return x && x->t >= 0 && x->t < 20; }

/* === Execute q expression, return QueryResult === */
lean_obj_res lean_kdb_query(b_lean_obj_arg expr_obj, lean_obj_arg world) {
    if (g_handle <= 0) {
        return lean_io_result_mk_error(lean_mk_io_user_error(lean_mk_string("kdb not connected")));
    }

    const char* expr = lean_string_cstr(expr_obj);
    K r = k(g_handle, (S)expr, (K)0);

    if (!r) {
        return lean_io_result_mk_error(lean_mk_io_user_error(lean_mk_string("kdb query failed (null)")));
    }

    // Check for error
    if (r->t == -128) {
        char buf[256];
        snprintf(buf, sizeof(buf), "kdb error: %s", r->s);
        r0(r);
        return lean_io_result_mk_error(lean_mk_io_user_error(lean_mk_string(buf)));
    }

    QueryResult* qr = calloc(1, sizeof(QueryResult));

    if (is_keyed(r)) {
        // Keyed table: unkey via ktd (converts to regular table)
        K t = ktd(r);  // ktd consumes r, returns unkeyed table
        qr->result = t;
        K dict = t->k;
        qr->keys = kK(dict)[0];
        qr->vals = kK(dict)[1];
        qr->ncols = qr->keys->n;
        qr->nrows = (qr->ncols > 0) ? kK(qr->vals)[0]->n : 0;
    } else if (is_table(r)) {
        // Table: XT means r->k is dict (XD)
        qr->result = r;
        K dict = r->k;
        qr->keys = kK(dict)[0];
        qr->vals = kK(dict)[1];
        qr->ncols = qr->keys->n;
        qr->nrows = (qr->ncols > 0) ? kK(qr->vals)[0]->n : 0;
    } else if (is_atom(r)) {
        // Atom: wrap as 1x1 pseudo-table (store K directly, special handling)
        qr->result = r;
        qr->keys = NULL;  // signals atom mode
        qr->vals = r;     // the atom itself
        qr->ncols = 1;
        qr->nrows = 1;
    } else if (is_vec(r)) {
        // Vector: treat as single column with n rows
        qr->result = r;
        qr->keys = NULL;  // signals vector mode
        qr->vals = r;
        qr->ncols = 1;
        qr->nrows = r->n;
    } else {
        r0(r);
        free(qr);
        return lean_io_result_mk_error(lean_mk_io_user_error(lean_mk_string("kdb result is not a table/atom/vector")));
    }

    return lean_io_result_mk_ok(lean_alloc_external(get_qr_class(), qr));
}

/* === Get column count === */
lean_obj_res lean_kdb_ncols(b_lean_obj_arg qr_obj, lean_obj_arg world) {
    QueryResult* qr = (QueryResult*)lean_get_external_data(qr_obj);
    return lean_io_result_mk_ok(lean_box_uint64((uint64_t)qr->ncols));
}

/* === Get row count === */
lean_obj_res lean_kdb_nrows(b_lean_obj_arg qr_obj, lean_obj_arg world) {
    QueryResult* qr = (QueryResult*)lean_get_external_data(qr_obj);
    return lean_io_result_mk_ok(lean_box_uint64((uint64_t)qr->nrows));
}

/* === Get column name === */
lean_obj_res lean_kdb_col_name(b_lean_obj_arg qr_obj, uint64_t col, lean_obj_arg world) {
    QueryResult* qr = (QueryResult*)lean_get_external_data(qr_obj);
    if ((int64_t)col >= qr->ncols) {
        return lean_io_result_mk_ok(lean_mk_string(""));
    }
    // atom/vector mode: no column names
    if (!qr->keys) {
        return lean_io_result_mk_ok(lean_mk_string("v"));
    }
    S name = kS(qr->keys)[col];
    return lean_io_result_mk_ok(lean_mk_string(name ? name : ""));
}

/* === Map q type to char === */
static char type_char(signed char t) {
    // atoms have negative type, vectors positive
    int at = t < 0 ? -t : t;
    switch (at) {
        case KB: return 'b';  // boolean
        case KG: return 'x';  // byte
        case KH: return 'h';  // short
        case KI: return 'i';  // int
        case KJ: return 'j';  // long
        case KE: return 'e';  // real
        case KF: return 'f';  // float
        case KC: return 'c';  // char
        case KS: return 's';  // symbol
        case KP: return 'p';  // timestamp
        case KM: return 'm';  // month
        case KD: return 'd';  // date
        case KN: return 'n';  // timespan
        case KU: return 'u';  // minute
        case KV: return 'v';  // second
        case KT: return 't';  // time
        case 0:  return '0';  // mixed
        default: return '?';
    }
}

/* === Get column type char === */
lean_obj_res lean_kdb_col_type(b_lean_obj_arg qr_obj, uint64_t col, lean_obj_arg world) {
    QueryResult* qr = (QueryResult*)lean_get_external_data(qr_obj);
    if ((int64_t)col >= qr->ncols) {
        return lean_io_result_mk_ok(lean_box_uint32(' '));
    }
    // atom/vector mode: type from vals directly
    if (!qr->keys) {
        return lean_io_result_mk_ok(lean_box_uint32((uint32_t)type_char(qr->vals->t)));
    }
    K column = kK(qr->vals)[col];
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)type_char(column->t)));
}

/* === Days in month (handles leap years) === */
static int days_in_month(int y, int m) {
    static const int dm[] = {31,28,31,30,31,30,31,31,30,31,30,31};
    if (m == 2 && (y % 4 == 0 && (y % 100 != 0 || y % 400 == 0))) return 29;
    return dm[m - 1];
}

/* === Convert days since 2000.01.01 to yyyy.mm.dd === */
static void days_to_ymd(I d, int* y, int* m, int* day) {
    *y = 2000; *m = 1; *day = 1;
    d += 1;  /* kdb day 0 = 2000.01.01 */
    while (d > 365 + (*y % 4 == 0 && (*y % 100 != 0 || *y % 400 == 0))) {
        d -= 365 + (*y % 4 == 0 && (*y % 100 != 0 || *y % 400 == 0));
        (*y)++;
    }
    while (d > days_in_month(*y, *m)) { d -= days_in_month(*y, *m); (*m)++; }
    *day = d;
}

/* === Format timestamp (ns from 2000.01.01) === */
static void format_timestamp(J ns, char* buf, size_t sz) {
    J day = ns / 86400000000000LL;
    J rem = ns % 86400000000000LL;
    if (rem < 0) { day--; rem += 86400000000000LL; }
    int y, m, d; days_to_ymd((I)day, &y, &m, &d);
    snprintf(buf, sz, "%04d.%02d.%02dT%02lld:%02lld:%02lld", y, m, d,
             (long long)(rem / 3600000000000LL),
             (long long)((rem / 60000000000LL) % 60),
             (long long)((rem / 1000000000LL) % 60));
}

/* === Format date (days from 2000.01.01) → yyyy.mm.dd === */
static void format_date(I d, char* buf, size_t sz) {
    int y, m, day; days_to_ymd(d, &y, &m, &day);
    snprintf(buf, sz, "%04d.%02d.%02d", y, m, day);
}

/* === Format cell from vector at row === */
static void format_vec_cell(K v, uint64_t row, char* buf, size_t sz) {
    buf[0] = '\0';
    switch (v->t) {
        case KB: snprintf(buf, sz, "%d", (int)kG(v)[row]); break;
        case KG: snprintf(buf, sz, "0x%02x", (unsigned)kG(v)[row]); break;
        case KH: if (kH(v)[row] != nh) snprintf(buf, sz, "%d", (int)kH(v)[row]); break;
        case KI: if (kI(v)[row] != ni) snprintf(buf, sz, "%d", kI(v)[row]); break;
        case KJ: if (kJ(v)[row] != nj) snprintf(buf, sz, "%lld", (long long)kJ(v)[row]); break;
        case KE: snprintf(buf, sz, "%.17g", (double)kE(v)[row]); break;
        case KF: snprintf(buf, sz, "%.17g", kF(v)[row]); break;
        case KC: buf[0] = kC(v)[row]; buf[1] = '\0'; break;
        case KS: snprintf(buf, sz, "%s", kS(v)[row] ? kS(v)[row] : ""); break;
        case KP: format_timestamp(kJ(v)[row], buf, sz); break;
        case KD: format_date(kI(v)[row], buf, sz); break;
        case KT: { I ms = kI(v)[row]; snprintf(buf, sz, "%02d:%02d:%02d.%03d",
                   ms/3600000, (ms/60000)%60, (ms/1000)%60, ms%1000); } break;
        case 0:  snprintf(buf, sz, "[list]"); break;
        default: snprintf(buf, sz, "?t%d", v->t); break;
    }
}

/* === Format atom value === */
static void format_atom(K a, char* buf, size_t sz) {
    buf[0] = '\0';
    int t = -a->t;  // atom type is negative
    switch (t) {
        case KB: snprintf(buf, sz, "%d", (int)a->g); break;
        case KG: snprintf(buf, sz, "0x%02x", (unsigned)a->g); break;
        case KH: if (a->h != nh) snprintf(buf, sz, "%d", (int)a->h); break;
        case KI: if (a->i != ni) snprintf(buf, sz, "%d", a->i); break;
        case KJ: if (a->j != nj) snprintf(buf, sz, "%lld", (long long)a->j); break;
        case KE: snprintf(buf, sz, "%.17g", (double)a->e); break;
        case KF: snprintf(buf, sz, "%.17g", a->f); break;
        case KC: buf[0] = (char)a->g; buf[1] = '\0'; break;
        case KS: snprintf(buf, sz, "%s", a->s ? a->s : ""); break;
        case KP: format_timestamp(a->j, buf, sz); break;
        case KD: format_date(a->i, buf, sz); break;
        case KT: { I ms = a->i; snprintf(buf, sz, "%02d:%02d:%02d.%03d",
                   ms/3600000, (ms/60000)%60, (ms/1000)%60, ms%1000); } break;
        default: snprintf(buf, sz, "?t%d", a->t); break;
    }
}

/* === Get cell as string === */
lean_obj_res lean_kdb_cell_str(b_lean_obj_arg qr_obj, uint64_t row, uint64_t col, lean_obj_arg world) {
    QueryResult* qr = (QueryResult*)lean_get_external_data(qr_obj);
    if ((int64_t)col >= qr->ncols || (int64_t)row >= qr->nrows) {
        return lean_io_result_mk_ok(lean_mk_string(""));
    }
    char buf[CELL_BUF_SIZE];
    // atom/vector mode
    if (!qr->keys) {
        if (is_atom(qr->vals)) {
            format_atom(qr->vals, buf, sizeof(buf));
        } else {
            format_vec_cell(qr->vals, row, buf, sizeof(buf));
        }
        return lean_io_result_mk_ok(lean_mk_string(buf));
    }
    // table mode
    K column = kK(qr->vals)[col];
    format_vec_cell(column, row, buf, sizeof(buf));
    return lean_io_result_mk_ok(lean_mk_string(buf));
}

/* === Check if connected === */
lean_obj_res lean_kdb_connected(lean_obj_arg world) {
    return lean_io_result_mk_ok(lean_box(g_handle > 0 ? 1 : 0));
}
