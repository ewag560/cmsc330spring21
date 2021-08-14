
open TokenTypes

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)
 
let tokenize input = 
let s_len = String.length input in
    let rec help i =
    if i >= s_len then []
     else
     if (Str.string_match (Str.regexp "(-[0-9]+)") input i) then
        let t1 = Str.matched_string input in
        let t2 = Str.string_after t1 1 in
        let t3 = Str.string_before t2 ((String.length t2 -1))in
        Tok_Int(int_of_string t3)::(help (i + String.length t1 + 1))

    else if (Str.string_match (Str.regexp "[0-9]+" )input i) then
    let mat_int = Str.matched_string input in
    (Tok_Int (int_of_string mat_int)):: (help ( i + String.length mat_int))
    
    else if (Str.string_match (Str.regexp "->") input i) then Tok_Arrow::(help (i + 2))

    else if (Str.string_match (Str.regexp "\\+") input i) then Tok_Add::(help (i + 1))
    
    else if (Str.string_match (Str.regexp "-") input i) then Tok_Sub::(help (i + 1))
    
    else if (Str.string_match (Str.regexp "\\*") input i) then Tok_Mult::(help (i + 1))
    
    else if (Str.string_match (Str.regexp "/") input i) then Tok_Div::(help (i + 1))

    else if (Str.string_match (Str.regexp "\\^") input i) then Tok_Concat::(help (i + 1))
    
    else if (Str.string_match (Str.regexp "[ \n\t]+") input i) then
    (help (i + 1))
    
    else if (Str.string_match (Str.regexp "=") input i) then Tok_Equal::(help (i + 1))
    
    else if (Str.string_match (Str.regexp "<>") input i) then Tok_NotEqual::(help (i + 2))

    else if (Str.string_match (Str.regexp ">=") input i) then Tok_GreaterEqual::(help (i + 2))

    else if (Str.string_match (Str.regexp "<=") input i) then Tok_LessEqual::(help (i + 2))
    
    else if (Str.string_match (Str.regexp ">") input i) then Tok_Greater::(help (i + 1))
    
    else if (Str.string_match (Str.regexp "<") input i) then Tok_Less::(help (i + 1))

    else if (Str.string_match (Str.regexp "not") input i) then Tok_Not::(help (i + 3))

    else if (Str.string_match (Str.regexp "if") input i) then Tok_If::(help (i + 2))

    else if (Str.string_match (Str.regexp "then") input i) then Tok_Then::(help (i + 4))

    else if (Str.string_match (Str.regexp "else") input i) then Tok_Else::(help (i + 4))

    else if (Str.string_match (Str.regexp "let") input i) then Tok_Let::(help (i + 3))

    else if (Str.string_match (Str.regexp "rec") input i) then Tok_Rec::(help (i + 3))

    else if (Str.string_match (Str.regexp "def") input i) then Tok_Def::(help (i + 3))

    else if (Str.string_match (Str.regexp "in") input i) then Tok_In::(help (i + 2))

    else if (Str.string_match (Str.regexp "fun") input i) then Tok_Fun::(help (i + 3))

    else if (Str.string_match (Str.regexp "&&") input i) then Tok_And::(help (i + 2))
   
    else if (Str.string_match (Str.regexp "||") input i) then Tok_Or::(help (i + 2))

    else if (Str.string_match (Str.regexp ";;") input i) then Tok_DoubleSemi::(help (i + 2))

    else if (Str.string_match (Str.regexp "true\\|false" )input i) then
let mat_int = Str.matched_string input in
    Tok_Bool (bool_of_string mat_int):: (help (i + (String.length mat_int)))

    else if (Str.string_match (Str.regexp "\"[^\"]*\"" )input i) then
let mat_int = Str.matched_string input in
    
let st = Str.global_replace (Str.regexp "\"+") "" mat_int in
    if (String.length st = 0) then
    raise (InvalidInputException "string error")
    else
    Tok_String (st):: (help (i + (String.length mat_int)))
    
    else if (Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9]*" )input i) then
    let mat_int = Str.matched_string input in
    Tok_ID (mat_int):: (help (i + (String.length mat_int)))

    else if (Str.string_match (Str.regexp ")") input i) then Tok_RParen::(help (i + 1))
    
    else if (Str.string_match (Str.regexp "(") input i) then Tok_LParen::(help (i + 1))

    else (help (i +1))
in help 0