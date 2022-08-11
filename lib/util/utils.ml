module ListUtil = struct
  let max cmp l = List.(hd (rev (sort cmp l)))
end

module StringUtil = struct
  let drop n s = String.sub s n (String.length s - n)
end
