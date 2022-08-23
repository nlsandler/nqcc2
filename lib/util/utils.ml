module ListUtil = struct
  let max cmp l = List.(hd (rev (sort cmp l)))
  let make_list len v = List.init len (fun _ -> v)

  let rec take_drop n = function
    | h :: t when n > 0 ->
        let l1, l2 = take_drop (n - 1) t in
        (h :: l1, l2)
    | l -> ([], l)
end

module StringUtil = struct
  let drop n s = String.sub s n (String.length s - n)
  let chop_suffix ?(n = 1) s = String.sub s 0 (String.length s - n)
end
