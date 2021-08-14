open List
open Sets

(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q

type ('q, 's) nfa_t = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
}

(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []

(****************)
(* Part 1: NFAs *)
(****************)

let rec addmissing l elem =
match l with
| []-> elem
| h::t -> if List.mem h elem =true then addmissing t elem else addmissing t (h::elem);;

let rec contain x a = match a with
| [] -> false
| s::t -> if s= x then true else contain x t;;


let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list =
match s with
        | None ->
(match nfa with
{sigma=_;qs=_;q0=_;fs=_;delta = de}-> List.fold_right (fun h a ->
match h with
| (x, None, y) ->
if contain x qs=true then y::a else a
| _ -> a) de []
)

  | Some e ->
match nfa with
{sigma=_;qs=_;q0=_;fs=_;delta = de}-> let result = (List.fold_right (fun h a ->
match h with
    | (x,Some op, y)->
if List.mem x qs =true && op= e then y::a else a
    | (x, None, y) -> a)
de []) in addmissing result [];;

let rec ehelptwo s record =
List.fold_right (fun x a-> if List.mem x record=true then a else x::a) s [];;

let rec same l ls =
match l with
| h::t -> if (List.mem h ls) = true then true else (same t ls)
| []-> false
;;

let rec ehelp nfa qll seen elem=
let ql = ehelptwo qll seen in
if ql=[] then elem
else match nfa with
{sigma=_;qs=_;q0=_;fs=_;delta=d} ->
let pla = (List.fold_right (fun h a ->match h with
    | (x,None,y)->
if (List.mem x ql =true && List.mem x elem =false && List.mem y elem =false  && List.mem y ql =false) then y::a else a
    | _ -> a) d []) in
ehelp nfa pla (ql@seen) (ql@elem);;

let e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =
ehelp nfa qs [] [];;

let rec ahelper nfa st slst =
let {sigma=_;qs=_;q0=_;fs= final;delta=de} = nfa in
match slst with
| []-> if same st final =false then false else true 
| h::t -> let newst = e_closure nfa (move nfa (e_closure nfa st) (Some h)) in
if newst = [] then false
else ahelper nfa newst t
;;

let accept (nfa: ('q,char) nfa_t) (s: string) : bool =
match nfa with {sigma=_;qs=_;q0= st;fs=f;delta=_}->
if not (s = "") then
ahelper nfa [st] (explode s)
else same (e_closure nfa [st]) f
;;


(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
let e = e_closure nfa qs in
match nfa with
{sigma=k; qs=_;q0=_;fs=_;delta=d} ->List.map (fun q ->
e_closure nfa (List.fold_right (fun tr b-> match tr with
  | (x,Some op, y) -> if contain x e =true && op =q then y::b else b
  | _ -> b) d [] )) k;;

let rec remdup lst a =
match lst with
| h::t -> if contain h a = true then remdup t a
else remdup t (h::a)
| []-> a
;;

let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
let ls = e_closure nfa qs in
match nfa with
{sigma=k ;qs=_;q0=_;fs=_;delta=de} -> List.map (fun si ->
(qs, Some si, e_closure nfa (remdup (List.fold_right (fun h b-> match h with
      | (x,Some op, y) -> if contain x ls =true && op =si then y::b else b
      | _ -> b) de []) []) )) k
;;

let helpert nfa qs =
let ls = e_closure nfa qs in
match nfa with
{sigma=k ;qs=_;q0=_;fs=_;delta=de} -> List.map (fun si ->
e_closure nfa (remdup (List.fold_right (fun h b-> match h with
| (x,Some op, y) -> if contain x ls =true && op =si then y::b else b
        | _ -> b) de []) []) ) k
;;

let rec containall x a =
match x with 
| h::t -> if contain h a =true then true else containall t a
| []-> false
;;

let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
match nfa with
{sigma=_;qs=_;q0=_;fs=x;delta=_} ->
if (containall x qs) =true then [qs]
else []
;;

let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
(work: 'q list list) : ('q list, 's) nfa_t =
failwith "unimplemented"


let rec lste ls lt =
let e1 = List.sort compare ls in
let e2 = List.sort compare lt in
if e1 = e2 then true else false
;;

let rec searchlst x a=
match a with
| [] -> false   
| h::t -> if lste h x then true else searchlst x t
;;

let rec rls lst a =
match lst with
| []-> a
| h::t -> if searchlst h a = true then rls t a
else rls t (h::a)
;;

let rec removeempt ls result=
match ls with
| h::t -> if h= [] then removeempt t result else removeempt t (h::result)
| []-> result
;;

let rec rmtwo s record =
List.fold_right (fun x a-> if searchlst x record=true then a else x::a) s []
;;


let rec gets nfa states res =
let afrem = rmtwo states res in
if afrem =[] then res
else
gets nfa (List.fold_right (fun x a-> (removeempt (helpert nfa x) [])@ a ) afrem []) (afrem @ res)
;;

let getf nfa lst =
List.fold_right (fun x a -> (new_finals nfa x) @ a) lst []
;;

let getd nfa lst=
List.fold_right (fun x a-> (new_trans nfa x) @ a) lst []
;;

let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
match nfa with
{sigma=h ;qs=_;q0= k ;fs=_;delta=de}->
let start = (e_closure nfa [k]) in
let t = removeempt (helpert nfa start) [] in
let newqs = gets nfa t [start] in
{sigma=h; qs= newqs; q0= start; fs= getf nfa newqs; delta= getd nfa newqs}
;; 




