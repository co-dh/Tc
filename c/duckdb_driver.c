/*
 * DuckDB Driver - DuckDB-specific ADBC initialization
 * Provides lean_adbc_init for backwards compatibility
 */
#include <lean/lean.h>

// | DuckDB driver paths to try
static const char* DUCKDB_PATHS[] = {
    "/usr/lib/libduckdb.so",
    "/usr/local/lib/libduckdb.so",
    "libduckdb.so",
    NULL
};

// | DuckDB ADBC entrypoint
static const char* DUCKDB_ENTRYPOINT = "duckdb_adbc_init";

// | Forward decl for generic init (from adbc_core.c)
extern lean_obj_res lean_adbc_init_driver(b_lean_obj_arg driver_obj, b_lean_obj_arg entry_obj, lean_obj_arg world);

// | Init ADBC with DuckDB (tries multiple paths)
lean_obj_res lean_adbc_init(lean_obj_arg world) {
    for (int i = 0; DUCKDB_PATHS[i]; i++) {
        lean_object* driver = lean_mk_string(DUCKDB_PATHS[i]);
        lean_object* entry = lean_mk_string(DUCKDB_ENTRYPOINT);
        lean_object* res = lean_adbc_init_driver(driver, entry, world);
        lean_dec_ref(driver);
        lean_dec_ref(entry);

        // Check if init succeeded (result is lean_box(1))
        if (lean_is_scalar(lean_io_result_get_value(res))) {
            if (lean_unbox(lean_io_result_get_value(res)) == 1) {
                return res;  // success
            }
        }
        lean_dec_ref(res);
    }
    return lean_io_result_mk_ok(lean_box(0));  // all paths failed
}
