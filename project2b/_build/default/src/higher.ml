open Funs

(********************************)
(* Part 1: High Order Functions *)
(********************************)

let contains_elem lst e = 
    Funs.fold (fun a b -> a || b = e) false lst
;;

let is_present lst x =
    Funs.map (fun a -> if a = x then 1 else 0) lst
;;

let count_occ lst target = 
    Funs.fold (fun a b -> 
        if b = target then a+1 else a)
        0 lst
;;

let uniq lst = 
    Funs.fold (fun a b ->
        if (count_occ lst b) < 2 || (count_occ a b) = 0 then
          b::a else a ) [] lst
;;

let assoc_list lst = 
    let ulst = uniq lst in
        Funs.fold (fun a b ->
            (b,count_occ lst b)::a
            ) [] ulst
;;

let ap fns args =
    Funs.fold (fun a b ->
        a@(List.map b args)
        ) [] fns
;;
