(* whitespace characters: space, tab, newline, vertical tab, form feed *)
let is_whitespace c = String.contains " \t\n\x0b\x0c" c
let is_digit c = String.contains "0123456789" c

let is_letter c =
  String.contains "abcdefghijklmnopqrstuvwxzyABCDEFGHIJKLMNOPQRSTUVWXZY" c

let drop n s = String.sub s n (String.length s - n)
let drop_first = drop 1
let chop_suffix ?(n = 1) s = String.sub s 0 (String.length s - n)

(* Get the first n characters of s as a list;
 * if s has n or fewer characters, return the whole string *)
let prefix s n =
  let pref = if String.length s > n then String.sub s 0 n else s in
  pref |> String.to_seq |> List.of_seq
