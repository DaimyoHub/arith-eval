open Token


type ast =
  | Node of ast * token * ast
  | Leaf of token


let is_well_parenthesized token_list =
  let rec check_paren token_list c =
    match token_list with
    | [] -> if c = 0 then true else false
    | Lpar :: s -> check_paren s (c + 1)
    | Rpar :: s -> check_paren s (c - 1)
    | _ :: s -> check_paren s c
  in
  check_paren token_list 0


let find_first_level_k_operation token_list k =
  let rec find_with_level_counter pre_tl op post_tl level =
    match post_tl with
    | [] -> pre_tl, op, post_tl
    | Lpar :: s -> find_with_level_counter (pre_tl @ [Lpar]) op s (level + 1)
    | Rpar :: s -> find_with_level_counter (pre_tl @ [Rpar]) op s (level - 1)
    | Tms :: s when level = k -> pre_tl, Tms, s
    | Dvd :: s when level = k -> pre_tl, Dvd, s
    | Pls :: s when level = k -> pre_tl, Pls, s
    | Mns :: s when level = k -> pre_tl, Mns, s
    | x :: s -> find_with_level_counter (pre_tl @ [x]) op s level
  in
  find_with_level_counter [] Unknown token_list 0


let rec remove_useless_left_parentheses token_list k =
  match token_list, k with
  | [], _ -> []
  | _, 0 -> token_list
  | Lpar :: s, k -> remove_useless_left_parentheses s (k - 1)
  | x :: s, _ -> x :: remove_useless_left_parentheses s k


let rec remove_useless_right_parentheses token_list k =
  match token_list, k with
  | [], _ -> []
  | _, 0 -> token_list
  | Rpar :: s, k -> remove_useless_right_parentheses s (k - 1)
  | x :: s, _ -> x :: remove_useless_right_parentheses s k


let find_first_operation token_list =
  let rec find_with_level_counter k =
    let a, op, b = find_first_level_k_operation token_list k in
    match op with
    | Unknown -> find_with_level_counter (k + 1)
    | _ -> a, op, b, k
  in
  match token_list with
  | [] -> [], Unknown, []
  | [x] -> [x], Unknown, []
  | _ -> 
      let a, op, b, k = find_with_level_counter 0 in
      remove_useless_left_parentheses a k, op, remove_useless_right_parentheses b k


let rec token_list_to_ast token_list =
  let a, op, b = find_first_operation token_list in
  match a, op, b with
  | [], Unknown, [] -> Leaf(Unknown)
  | [x], Unknown, [] -> Leaf(x)
  | _, _, _ -> 
      let left_partial_ast = token_list_to_ast a
      and right_partial_ast = token_list_to_ast b in
      Node(left_partial_ast, op, right_partial_ast)


(* Check if x of Leaf(x) is a Nu *)
let rec evaluate_ast ast = 
  match ast with
  | Leaf(x) -> begin
      match x with
      | Unknown -> 0
      | Numc(c) -> c
      | _ -> failwith "Invalid expression"
    end
  | Node(l, op, r) -> begin
      let ev_l, ev_r = evaluate_ast l, evaluate_ast r in
      match op with
      | Pls -> ev_l + ev_r
      | Tms -> ev_l * ev_r
      | Dvd -> ev_l / ev_r
      | Mns -> ev_l - ev_r
      | _ -> failwith "Invalid expression"
    end