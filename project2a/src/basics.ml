(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup tup = let (a,b,c) = tup in (c,b,a);;

let abs x = if x>=0 then x else x * (-1);;

let area x y = let (xa,xb) = x in let (ya,yb) = y in
((abs (xa-ya)) * (abs (xb-yb)))
;;

let volume x y = let (xa,xb,xc) = x in let (ya,yb,yc) = y in
((abs(xa-ya))* (abs(xb-yb)) * (abs(xc-yc)))
;;



(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec fibonacci n =
if n < 3 then
if n = 0 then 0 else 1
else
fibonacci(n-1) + fibonacci(n-2)
;;

let rec pow x y =
if y = 0 then 1 else
x * pow x (y-1)
;;

let rec log x y =
if (y/x) <= 1 then (y/x) else
(log x (y/x)) + 1
;;


let rec his_prime x y = if (y = 1) then true else
if (y<1) then false else
if (x mod y = 0) then false else
his_prime x (y-1)
;;

let rec is_prime x = his_prime x (x-1);;


let rec next_prime x = if is_prime x = true then x else
next_prime (x+1)
;;


(*****************)
(* Part 3: Lists *)
(*****************)

let rec get idx lst = match lst with
    | [] -> failwith "Out of bounds"
    | first::rest ->
if idx = 0 then first
else get (idx-1) rest
;;

let rec length lst = match lst with
    | [] -> 0
    | first::rest->
if rest = [] then 1
else (length rest) + 1
;;

let larger lst1 lst2 = if (length lst1) > (length lst2) then lst1 else
if (length lst2) > (length lst1) then lst2 else
[]
;;

let reverse lst =
let rec append acc  = function
      | [] -> acc
      | h::t -> append (h::acc) t in
append [] lst
;;

let rec combine lst1 lst2 = match lst1 with
    | h :: t -> h :: combine t lst2
    | [] -> lst2
;;

let split lst n =
let rec app i acc = function
    | [] -> reverse acc, []
    | h :: t as l -> if i = 0 then reverse acc, l else
app (i-1) (h:: acc) t in app n [] lst
;;

let rec rotate shift lst =
let leng = (length lst) in
let shift = if leng = 0 then 0 else (shift mod leng + leng) mod leng in
if shift = 0 then lst
else let a, b = split lst shift in (combine b a)
;;

let rec is_palindrome lst = if lst = (reverse lst) then true else false;;

