(* a tiny helper function to get the destination of a tacky instruction; useful
   for both copy propagation and dead store elimination *)
let get_dst = function
  | Tacky.Copy { dst; _ } -> Some dst
  | FunCall { dst; _ } -> dst
  | Unary { dst; _ } -> Some dst
  | Binary { dst; _ } -> Some dst
  | SignExtend { dst; _ } -> Some dst
  | ZeroExtend { dst; _ } -> Some dst
  | DoubleToInt { dst; _ } -> Some dst
  | DoubleToUInt { dst; _ } -> Some dst
  | UIntToDouble { dst; _ } -> Some dst
  | IntToDouble { dst; _ } -> Some dst
  | Truncate { dst; _ } -> Some dst
  | GetAddress { dst; _ } -> Some dst
  | Load { dst; _ } -> Some dst
  | AddPtr { dst; _ } -> Some dst
  | CopyToOffset { dst; _ } -> Some (Var dst)
  | CopyFromOffset { dst; _ } -> Some dst
  | Store _ -> None
  | Return _ | Jump _ | JumpIfZero _ | JumpIfNotZero _ | Label _ -> None

let is_static v =
  match (Symbols.get v).attrs with Symbols.StaticAttr _ -> true | _ -> false
