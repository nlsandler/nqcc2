open Context

let is_dump_target ctx =
  match !Settings.debug.dump_fun with
  (* If we didn't specify which fun to dump info about, dump info for all of them *)
  | None -> true
  (* Otherwise, check if the current fun is the target fun *)
  | Some target when ctx.fun_name = target -> true
  | Some _other -> false

(* prefix is some user-friendly label indicating e.g. current optimization, ext is file extension, including . *)
let mk_filename prefix ctx ext =
  let base_filename =
    ctx.filename |> Filename.chop_extension |> Filename.basename
  in
  Unique_ids.make_label
    (Printf.sprintf "%s.%s.%s" prefix ctx.fun_name base_filename)
  ^ ext
