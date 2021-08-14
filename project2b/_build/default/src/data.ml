open Funs

(*************************************)
(* Part 2: Three-Way Search Tree *)
(*************************************)

type int_tree =
  | IntLeaf
  | IntNode of int * int option * int_tree * int_tree * int_tree 

let empty_int_tree = IntLeaf

let rec int_mem x t =
match t with
    | IntLeaf -> false
    | IntNode (i,io,t1,t2,t3)-> if i = x ||
    (match io with None -> false | Some q -> q = x)
    then true else int_mem x t1 || int_mem x t2 || int_mem x t3
;;


let rec insert x t = match t with
    | IntLeaf -> IntNode (x, None, IntLeaf, IntLeaf, IntLeaf)
    | IntNode (i,io,t1,t2,t3)-> match io with
        | None -> if x > i then IntNode (i,Some x,t1,t2,t3) else
            IntNode (x, Some i, t1, t2, t3)
        | Some io -> if x < i then IntNode (i,Some io, insert x t1, t2, t3)
            else if x > io then IntNode (i,Some io, t1, t2, insert x t3)
                else IntNode (i, Some io, t1, insert x t2, t3)
;;

let rec int_insert x t =
    if ((int_mem x t) = true) then
        t else insert x t
;;


let rec int_size t = match t with
    | IntLeaf -> 0
    | IntNode (i, io, t1, t2, t3) -> match io with
        | None -> 1
        | Some io -> 2 + int_size t1 + int_size t2 + int_size t3
;;

let rec int_max t = match t with
    | IntLeaf -> raise (Invalid_argument("int_max"))
    | IntNode(i,io,t1,t2,t3)-> match t3 with
        | IntNode(a,b,c,d,e) -> int_max t3
	| IntLeaf -> match io with
            | None-> i
            | Some x -> x
;;

(*******************************)
(* Part 3: Three-Way Search Tree-Based Map *)
(*******************************)

type 'a tree_map =
  | MapLeaf
  | MapNode of (int * 'a) * (int * 'a) option * 'a tree_map * 'a tree_map * 'a tree_map

let empty_tree_map = MapLeaf

let rec map_contains k t = match t with
    |MapLeaf -> false
        |MapNode ((k1,v1),op,t1,t2,t3) -> if k = k1 || match op with
        |None-> false
	        |Some (k2,v2) -> k = k2
        then true else map_contains k t1 || map_contains k t2 || map_contains k t3
;;

let rec m_insert k v t= match t with
    | MapLeaf -> MapNode ((k,v), None, MapLeaf,MapLeaf,MapLeaf)
        | MapNode ((k1,v1),o,t1,t2,t3) -> match o with
        | None -> if k > k1 then MapNode ((k,v), Some (k1,v1),t1,t2,t3) else
	MapNode ((k1,v1),Some (k,v), t1,t2,t3)
	        | Some (k2,v2) -> if k < k1 then MapNode ((k1,v1),Some (k2,v2), m_insert k v t1,t2,t3)
          else if k > k2 then MapNode ((k1,v1),Some (k2,v2), t1,t2, m_insert k v t3)
            else MapNode ((k1,v1),Some (k2,v2), t1, m_insert k v t2, t3)
;;

let rec map_put k v t =
  if ((map_contains k t) = true) then
    raise(Invalid_argument("map_put")) else m_insert k v t
;;

let rec map_get k t = match t with
    |MapLeaf -> raise(Invalid_argument("map_get"))
        |MapNode ((k1,v1),op,t1,t2,t3) -> match op with
        |None -> if k1 = k then v1 else raise(Invalid_argument("map_get"))
	        |Some (k2,v2) -> if k1 = k then v1 else if k2 = k then v2 else
            if k < k1 then map_get k t1 else
                if k > k2 then map_get k t3 else
                    map_get k t2
;;

(***************************)
(* Part 4: Variable Lookup *)
(***************************)

type lookup_table = (string * int) list list

let empty_table = []

let push_scope (table:lookup_table) : lookup_table =
  match table with
      | []-> [[]]
            | h::t -> []::h::t
;;

let pop_scope (table:lookup_table) : lookup_table =
  match table with 
      | [] -> raise(failwith "No scopes remain!")
            | h::t -> t
;;

let rec var_exists name scope = match scope with
    | [] -> false
        | h::t -> match h with 
        |(nam,num) -> nam = name || var_exists name t
	;;

	let add_var name value (table:lookup_table) : lookup_table =
	match table with
	      | [] -> failwith "No scopes remain!"
      | h::t -> if var_exists name h then failwith "Duplicate variable binding in scope!"
      else match h with
                  | [] -> [(name,value)]::t
            | lh::lt -> ((name,value)::lh::lt)::t
	    ;;

	    let rec lookup name (table:lookup_table) =
	    match table with
	          | [] -> failwith "Variable not found!"
      | h::t -> match h with
                | [] -> lookup name t
          | lh::lt -> match lh with
	                | (nam,num) -> if nam = name then num 
                else lookup name (lt::t)
;;
