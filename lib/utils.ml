let is_decimal_digit c = Char.code c >= 48 && Char.code c <= 59

let ctoi c = Char.code c - 48

let rec pow a n =
  match n with
  | 0 -> 1
  | 1 -> a
  | _ -> a * pow a (n - 1)


let token_to_string token =
  match token with
  | Token.Lpar -> "Lpar"
  | Token.Rpar -> "Rpar"
  | Token.Numc(c) -> Printf.sprintf "Numc(%d)" c
  | Token.Pls -> "Pls"
  | Token.Mns -> "Mns"
  | Token.Tms -> "Tms"
  | Token.Dvd -> "Dvd"
  | Token.Unknown -> "Unknown"


let serialize_token_list token_list =
  let rec construct_string tl =
    match tl with
    | [] -> ""
    | x :: s -> Printf.sprintf " %s%s" (token_to_string x) (construct_string s)
  in
  match token_list with
  | [] -> ""
  | x :: s -> Printf.sprintf "[%s%s]" (token_to_string x) (construct_string s)

