exception Syntax_error


let text_to_char_list text =
  let n = String.length text in
  let rec construct_char_list i =
    match i with
    | j when j = n -> []
    | _ -> text.[i] :: construct_char_list (i + 1)
  in
  construct_char_list 0


let rec skip_blank_spaces char_list =
  match char_list with
  | x :: s when x = ' ' -> skip_blank_spaces s
  | _ -> char_list


let rec parse_number char_list =
  match char_list with
  | [] -> [], []
  | x :: s -> 
      if Utils.is_decimal_digit x then
        let number_list, cl = parse_number s in
        Utils.ctoi x :: number_list, cl
      else
        [], char_list


let construct_parsed_number number_list =
  let rec construct nl p =
    match nl with
    | [] -> 0
    | x :: s -> x * (Utils.pow 10 p) + construct s (p - 1)
  in
  construct number_list (List.length number_list - 1)


let recognize_number char_list =
  let parsed_number, char_list_n = parse_number char_list in
  Token.Numc (construct_parsed_number parsed_number), char_list_n


let text_to_token_list text =
  let char_list = text |> text_to_char_list |> skip_blank_spaces in
  let rec tokenize char_list =
    let clean_tok s = tokenize (skip_blank_spaces s) in
    let open Token in
    match char_list with
    | [] -> []
    | '(' :: s -> Lpar :: clean_tok s
    | ')' :: s -> Rpar :: clean_tok s
    | '+' :: s -> Pls :: clean_tok s
    | '-' :: s -> Mns :: clean_tok s
    | '*' :: s -> Tms :: clean_tok s
    | '/' :: s -> Dvd :: clean_tok s
    | c :: _ when Utils.is_decimal_digit c -> 
        let numc, s = recognize_number char_list in
        numc :: clean_tok s
    | _ -> raise Syntax_error
  in
  tokenize char_list
