
open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v)::env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else update t x v

(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
let rec eval_expr env e = match e with
    | Value v -> v
    
    | ID i -> (lookup env i)

    | Not expr -> let b = (eval_expr env expr) in
    (match b with
    | Bool x ->  (if x = true then Bool false else Bool true)
    | _ -> raise (TypeError("noting non-bool")))
    
    
    | Binop (op, e1, e2) ->
( match op with
	        | Add -> let n1 = (eval_expr env e1) in
            let n2 = (eval_expr env e2) in
                (match n1 with
                    | Int x -> (match n2 with
				                              | Int y -> Int (x+y)
                        | _ -> raise (TypeError("adding non-value")))
		                        | _ -> raise (TypeError("adding non-value")))
                    
            | Sub -> let n1 = (eval_expr env e1) in
	    let n2 = (eval_expr env e2) in
	    (match n1 with
		                       | Int x -> (match n2 with
                        | Int y -> Int (x-y)
			                        | _ -> raise (TypeError("subing non-value")))
                    | _ -> raise (TypeError("subing non-value")))

	        | Mult -> let n1 = (eval_expr env e1) in
            let n2 = (eval_expr env e2) in
               ( match n1 with
                    | Int x -> (match n2 with
				                              | Int y -> Int (x*y)
                        | _ -> raise (TypeError("multing non-value")))
		                        | _ -> raise (TypeError("multing non-value")))
            | Div -> let n1 = (eval_expr env e1) in
	    let n2 = (eval_expr env e2) in
	    (match n1 with
		                       | Int x -> (match n2 with
                        | Int y -> if y!=0 then Int (x/y) else raise(DivByZeroError)
			                        | _ -> raise (TypeError("diving non-value")))
                    | _ -> raise (TypeError("diving non-value")))

	        | Concat -> let s1 = (eval_expr env e1) in
            let s2 = (eval_expr env e2) in 
                (match s1 with
                    | String s -> (match s2 with
					                         | String s2 -> String (s ^ s2)
                        | _ -> raise (TypeError("concating non-string")))
		                        | _ -> raise (TypeError("concating non-string")))
                
            | Greater ->  let n1 = (eval_expr env e1) in
	    let n2 = (eval_expr env e2) in
	    (match n1 with
		                       | Int x -> (match n2 with
                        | Int y -> Bool (x>y)
			                        | _ -> raise (TypeError(">ing non-int")))
                    | _ -> raise (TypeError(">ing non-int")))

	        | Less ->  let n1 = (eval_expr env e1) in
            let n2 = (eval_expr env e2) in 
                (match n1 with
                    | Int x -> (match n2 with
				                              | Int y -> Bool (x<y)
                        | _ -> raise (TypeError("<ing non-int")))
		                        | _ -> raise (TypeError("<ing non-int")))
                    
            | LessEqual ->  let n1 = (eval_expr env e1) in
	    let n2 = (eval_expr env e2) in
	    (match n1 with
		                       | Int x -> (match n2 with
                        | Int y -> Bool (x<=y)
			                        | _ -> raise (TypeError("<=ing non-int")))
                    | _ -> raise (TypeError("<=ing non-int")))

	        | GreaterEqual -> let n1 = (eval_expr env e1) in
            let n2 = (eval_expr env e2) in 
                (match n1 with
                    | Int x -> (match n2 with
				                              | Int y -> Bool (x>=y)
                        | _ -> raise (TypeError(">=ing non-int")))
		                        | _ -> raise (TypeError(">=ing non-int")))
                
        | Equal ->  let n1 = (eval_expr env e1) in
	    let n2 = (eval_expr env e2) in
	    (match n1 with
		                       | Int x -> (match n2 with
                        | Int y -> Bool (x=y)
			                        | _ -> raise (TypeError("=ing non-int")))
                    | _ -> raise (TypeError("=ing non-int")))

	                | NotEqual ->  let n1 = (eval_expr env e1) in
            let n2 = (eval_expr env e2) in 
                (match n1 with
                    | Int x -> (match n2 with
				                              | Int y -> Bool (x!=y)
                        | _ -> raise (TypeError("!=ing non-int")))
		                        | _ -> raise (TypeError("!=ing non-int")))
                    
        | Or -> let b1 = (eval_expr env e1) in
	    let b2 = (eval_expr env e2) in
	    (match b1 with
		                       | Bool x -> (match b2 with
                        | Bool y -> Bool (x||y)
			                        | _ -> raise (TypeError("oring non-bool")))
                    | _ -> raise (TypeError("oring non-bool")))

	    | And -> let b1 = (eval_expr env e1) in
            let b2 = (eval_expr env e2) in 
                (match b1 with
                    | Bool x -> (match b2 with
				                               | Bool y -> Bool (x&&y)
                        | _ -> raise (TypeError("anding non-bool")))
		                        | _ -> raise (TypeError("anding non-bool"))) 
                                        )
        | If (ifex, tex, eex) -> let e1 = (eval_expr env ifex) in
	( match e1 with
		                | Bool b -> let e2 = (
                    if b=true then (eval_expr env tex) else (eval_expr env eex)
                    ) in e2
                | _ -> raise(TypeError("ifing non-bool")))

	    | Let (name,recur, iex, bex) ->
            (match recur with
                | false -> let ie = (eval_expr env iex) in
		let env2 = (extend env name ie) in
		(eval_expr env2 bex)
		                | true -> let env2 = (extend_tmp env name) in
                    let ie = (eval_expr env2 iex) in
                    let _ = (update env2 name ie) in
                        (eval_expr env2 bex)
        )
        | Fun (p,b) -> Closure (env, p, b)

	    | FunctionCall (a,b) -> let e1 = (eval_expr env a) in
            let e2 = (eval_expr env b) in
            (match e1 with
                | Closure (aaa,nam,e) -> let env2 = (extend aaa nam e2) in
		(eval_expr env2 e)

		        |_ -> raise (TypeError("Not a function")))
        
	(* Part 2: Evaluating mutop directive *)

	(* Evaluates MicroCaml mutop directive [m] in environment [env],
	   returning a possibly updated environment paired with
	   a value option; throws an exception on error *)
	   let eval_mutop env m = match m with
        | NoOp -> ([], None)
        | Def (a,b)-> let env = extend_tmp env a in
            let eb = (eval_expr env b) in
                let _ = update env a eb in
                (env, Some eb)
        | Expr a -> let e = (eval_expr env a) in
            (env, Some e)


	   
