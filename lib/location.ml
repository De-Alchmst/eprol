type loc = {
  file: string;
  line: int;
  column: int;
}

type error = {
  message: string;
  loc: loc;
}

let print_loc loc =
  Printf.sprintf "%s:%d:%d" loc.file loc.line loc.column

let found_errors : error list ref = ref []

let report_error loc msg =
  found_errors := { loc = loc; message = msg } :: !found_errors;
  (* Print immediately so the user sees errors in order *)
  Printf.eprintf "%s: error: %s\n" (print_loc loc) msg
