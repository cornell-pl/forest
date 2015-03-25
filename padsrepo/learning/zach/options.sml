(* set options to control the amount of debugging print-out *)
structure Options = struct
val print_interp_debug = false (* useful for finding where a parse fails *)
val print_pre_reduced_IR = false (* prints the IR as used for parsing *)
val print_tables = false (* prints out the data as it appears in tables *)
val print_functional_deps = true
val print_report = true (* prints out a report summarizing information in Main.main' *)
val reparse_data = false (* try to reparse the data with the simplified IR *)
val print_levels = false (* report each level in TANE *)
end