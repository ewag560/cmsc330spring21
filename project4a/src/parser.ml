open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* Part 2: Parsing expressions *)

let rec parse_expr toks = 
  match (lookahead toks) with 
  | Some Tok_If -> p_if toks
  | Some Tok_Let -> p_let toks
  | Some Tok_Fun -> p_fun toks
  | _ -> p_or toks

and p_if toks =
  match lookahead toks with 
    | Some Tok_If -> 
      let tok_l2 = match_token toks Tok_If in 
      let (tok_l3,exp_l1) = parse_expr tok_l2 in 
      let tok_4 = match_token tok_l3 Tok_Then in 
      let (tok_l5,exp_l3) = parse_expr tok_4 in 
      let tok_l6 = match_token tok_l5 Tok_Else in 
      let (tok_l6,exp_l4) = parse_expr tok_l6 in (tok_l6, If(exp_l1,exp_l3,exp_l4))
    |_ -> raise (InvalidInputException "if")

and p_let toks =
  match lookahead toks with 
  | Some Tok_Let -> begin
    let tok_l = match_token toks Tok_Let in
    match lookahead tok_l with

  |Some Tok_Rec -> begin
    let tok_l2 = match_token tok_l Tok_Rec in
    match lookahead tok_l2 with

  |Some Tok_ID id -> let tok_l3 = match_token tok_l2 (Tok_ID id) in
    let tok_4 = match_token tok_l3 Tok_Equal in
    let (tok_l5, exp_l1) = parse_expr tok_4 in
    let tok_l6 = match_token tok_l5 Tok_In in
    let (tok_l6, exp_l2) = parse_expr tok_l6 in (tok_l6, Let(id, true, exp_l1, exp_l2))
  |_ -> raise (InvalidInputException "let1")
  end
  |Some Tok_ID id -> let tok_l2 = match_token tok_l (Tok_ID id) in
    let tok_l3 = match_token tok_l2 Tok_Equal in
    let (tok_4, exp_l1) = parse_expr tok_l3 in
    let tok_l5 = match_token tok_4 Tok_In in
    let (tok_l6, exp_l2) = parse_expr tok_l5 in (tok_l6, Let(id, false, exp_l1, exp_l2))
  |_ -> raise (InvalidInputException "let2")
  end
  |_ -> raise (InvalidInputException "let3")

and p_fun toks =
  match lookahead toks with
    | Some Tok_Fun -> 
    begin 
      let tok_l2 = match_token toks Tok_Fun in match lookahead tok_l2 with 
    | Some Tok_ID x -> 
      let tok_l3 = match_token tok_l2 (Tok_ID x) in
      let tok_4 = match_token tok_l3 Tok_Arrow in 
      let (tok_l5, exp_l1) = parse_expr tok_4 in (tok_l5, Fun(x, exp_l1))
    | _ -> raise (InvalidInputException("fun1")) 
    end
    | _ -> raise (InvalidInputException "fun2")

and p_or toks =
  let (t1, exp_l1) = p_and toks in
  match lookahead t1 with
  | Some Tok_Or -> let t2 = match_token t1 Tok_Or in
    let (t3, exp_l2) = p_or t2 in (t3, Binop(Or, exp_l1, exp_l2))
  | _ -> (t1, exp_l1)

and p_and toks = 
  let (t1, exp_l1) = p_eq toks in
  match lookahead t1 with
  | Some Tok_And -> 
    let t2 = match_token t1 Tok_And in
    let (tok_l3, exp_l2) = (p_and t2) in (tok_l3, Binop(And, exp_l1, exp_l2))
  | _ -> (t1, exp_l1)

and p_add toks = 
  let (t1, exp_l1) = p_mult toks in
  match lookahead t1 with
  | Some Tok_Add -> 
    let t2 = (match_token t1 Tok_Add) in
    let (t3, exp_l2) = (p_add t2) in (t3, Binop(Add, exp_l1, exp_l2))

  | Some Tok_Sub -> 
    let t2 = (match_token t1 Tok_Sub) in
    let (t3, exp_l2) = (p_add t2) in (t3, Binop(Sub, exp_l1, exp_l2))
  | _ -> (t1, exp_l1)

and p_mult toks = 
  let (tok, exp_l1) = p_concat toks in 
  match lookahead tok with 
  | Some Tok_Mult -> 
    let tok_l2 = match_token tok Tok_Mult in 
    let (tok_l3, exp_l3) = p_mult tok_l2 in (tok_l3, Binop (Mult, exp_l1, exp_l3))

  | Some Tok_Div -> 
    let tok_l2 = match_token tok Tok_Div in 
    let (tok_l3, exp_l3) = p_mult tok_l2 in (tok_l3, Binop (Div, exp_l1, exp_l3))  
  |_ -> (tok, exp_l1)

  and p_concat toks =
  let (tok, exp_l1) = p_uexp toks in
  match lookahead tok with 
  | Some Tok_Concat -> 
    let tok_l2 = match_token tok Tok_Concat in
    let (tok_l3, exp_l3) = p_concat tok_l2 in (tok_l3, Binop(Concat, exp_l1, exp_l3))

  | _ -> (tok, exp_l1)

and p_uexp toks = 
    match lookahead toks with 
    | Some Tok_Not -> 
      let tok_l2 = match_token toks Tok_Not in 
      let (tok_l3, exp_l1) = p_uexp tok_l2 in (tok_l3, Not (exp_l1))
    | _ -> p_fcall toks

and p_fcall toks = 
    let (tok, exp_l1) = p_primadd toks in 
    match lookahead tok with 
    | Some Tok_Bool x -> let (tok_l2, exp_l3) = p_primadd tok in (tok_l2, FunctionCall(exp_l1, exp_l3))

    | Some Tok_Int x -> let (tok_l2, exp_l3) = p_primadd tok in (tok_l2, FunctionCall(exp_l1, exp_l3))

    | Some Tok_ID x -> let (tok_l2, exp_l3) = p_primadd tok in (tok_l2, FunctionCall(exp_l1, exp_l3))

    | Some Tok_String x -> let (tok_l2, exp_l3) = p_primadd tok in (tok_l2, FunctionCall(exp_l1, exp_l3))

    | Some Tok_LParen -> let (tok_l2, exp_l3) = p_primadd tok in (tok_l2, FunctionCall(exp_l1, exp_l3))

    | _ -> (tok, exp_l1)

and p_eq toks = 
  let (t1, exp_l1) = p_releq toks in
  match lookahead t1 with 
  | Some Tok_Equal -> 
    let t2 = match_token t1 Tok_Equal in
    let (t3, exp_l2) = (p_eq t2) in (t3, Binop(Equal, exp_l1, exp_l2))

  | Some Tok_NotEqual -> 
    let t2 = match_token t1 Tok_NotEqual in
    let(t3, exp_l2) = (p_eq t2) in (t3, Binop(NotEqual, exp_l1, exp_l2))
  | _ -> (t1, exp_l1)

and p_releq toks =
  let (t1, exp_l1) = p_add toks in
  match lookahead t1 with
  | Some Tok_Greater -> 
    let t2 = (match_token t1 Tok_Greater) in
    let (t3, exp_l2) = (p_releq t2) in (t3, Binop(Greater, exp_l1, exp_l2))

  | Some Tok_Less -> 
    let t2 = (match_token t1 Tok_Less) in
    let (t3, exp_l2) = (p_releq t2) in
    (t3, Binop(Less, exp_l1, exp_l2))

  | Some Tok_GreaterEqual -> 
    let t2 = (match_token t1 Tok_GreaterEqual) in
    let (t3, exp_l2) = (p_releq t2) in
    (t3, Binop(Less, exp_l1, exp_l2))

  | Some Tok_LessEqual -> 
    let t2 = (match_token t1 Tok_LessEqual) in
    let (t3, exp_l2) = (p_releq t2) in (t3, Binop(LessEqual, exp_l1, exp_l2))
  | _ -> (t1, exp_l1)

and p_primadd toks = 
    match lookahead toks with 
    | Some Tok_Bool x -> let tok_l3 = match_token toks (Tok_Bool x) in (tok_l3, Value(Bool x))

    | Some Tok_Int x -> let tok_l2 = match_token toks (Tok_Int x) in (tok_l2, Value(Int x))

    | Some Tok_ID x -> let tok_l5 = match_token toks (Tok_ID x) in (tok_l5, ID x)

    | Some Tok_String x -> let tok_4 = match_token toks (Tok_String x) in (tok_4, Value(String x))

    | Some Tok_LParen -> 
      let tok_l6 = match_token toks Tok_LParen in 
      let (tok_l6, exp_l1) = parse_expr tok_l6 in
      let tok8 = match_token tok_l6 Tok_RParen in (tok8, exp_l1)
    | _ -> raise (InvalidInputException"primary")


(* Part 3: Parsing mutop *)

let rec parse_mutop toks = 
  match lookahead toks with 
  | Some Tok_Def -> p_cond toks
  | Some Tok_DoubleSemi -> let tok = match_token toks Tok_DoubleSemi in (tok, NoOp)
  | _ -> p_help toks

and p_cond toks = 
  let tok = match_token toks Tok_Def in 
  match lookahead tok with 
  | Some Tok_ID temp -> 
    let tok = match_token tok (Tok_ID temp) in 
    let t2 = match_token tok Tok_Equal in 
    let (t3, exp) = parse_expr t2 in 
    let t4 = match_token t3 Tok_DoubleSemi in (t4, Def(temp, exp))
  | _ -> raise (InvalidInputException "oop")     

and p_help toks =
  let (temp, exp) = (parse_expr toks) in 
  let tok = match_token temp Tok_DoubleSemi in (tok, Expr(exp))
  