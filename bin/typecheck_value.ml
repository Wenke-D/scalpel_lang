open Typecheck_symbol_table

let check_value (table : Typecheck_symbol_table.symbol_table) =
  Printexc.backtrace_slots
