include Set.Make (struct
  type t = Assembly.reg

  let compare = Assembly.compare_reg
end)
