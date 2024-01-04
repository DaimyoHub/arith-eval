let () =
  let open Arc in
  let r = "((3 * 5) / 3) + 3"
    |> Lexer.text_to_token_list
    |> Parser.token_list_to_ast
    |> Parser.evaluate_ast
  in
  print_int r

  (* [Lpar; Numc(138); Pls; Numc(48); Rpar; Dvd; Numc(9)] *)