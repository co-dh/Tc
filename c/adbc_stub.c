/*
 * ADBC Stub - Provides FFI symbols for tc-core (CSV-only build)
 * All ADBC operations return errors or empty results.
 */
#include <lean/lean.h>

/* | Init: always fails (no ADBC support) */
lean_obj_res lean_adbc_init(lean_obj_arg world) {
    return lean_io_result_mk_ok(lean_box(0));
}

/* | Shutdown: no-op */
lean_obj_res lean_adbc_shutdown(lean_obj_arg world) {
    return lean_io_result_mk_ok(lean_box(0));
}

/* | Query: error */
lean_obj_res lean_adbc_query(b_lean_obj_arg sql, lean_obj_arg world) {
    return lean_io_result_mk_error(lean_mk_io_user_error(
        lean_mk_string("ADBC not available (tc-core build)")));
}

/* | ncols: 0 */
lean_obj_res lean_qr_ncols(b_lean_obj_arg qr, lean_obj_arg world) {
    return lean_io_result_mk_ok(lean_box_uint64(0));
}

/* | nrows: 0 */
lean_obj_res lean_qr_nrows(b_lean_obj_arg qr, lean_obj_arg world) {
    return lean_io_result_mk_ok(lean_box_uint64(0));
}

/* | col_name: empty */
lean_obj_res lean_qr_col_name(b_lean_obj_arg qr, uint64_t col, lean_obj_arg world) {
    return lean_io_result_mk_ok(lean_mk_string(""));
}

/* | col_fmt: empty */
lean_obj_res lean_qr_col_fmt(b_lean_obj_arg qr, uint64_t col, lean_obj_arg world) {
    return lean_io_result_mk_ok(lean_mk_string(""));
}

/* | cell_str: empty */
lean_obj_res lean_qr_cell_str(b_lean_obj_arg qr, uint64_t row, uint64_t col, lean_obj_arg world) {
    return lean_io_result_mk_ok(lean_mk_string(""));
}

/* | cell_int: 0 */
lean_obj_res lean_qr_cell_int(b_lean_obj_arg qr, uint64_t row, uint64_t col, lean_obj_arg world) {
    return lean_io_result_mk_ok(lean_int64_to_int(0));
}

/* | cell_float: 0.0 */
lean_obj_res lean_qr_cell_float(b_lean_obj_arg qr, uint64_t row, uint64_t col, lean_obj_arg world) {
    return lean_io_result_mk_ok(lean_box_float(0.0));
}
