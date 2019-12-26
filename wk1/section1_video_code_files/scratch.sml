
(* There is no assignment in ML, only binding, which cannot be changed *)

(*assuming that y is larger than 0*)

(* val it = [true] *)

(* val it2 = false::true::it *)


fun sum_list(xs: int list) = 
    if null xs 
    then 0
    else hd xs + sum_list(tl xs)


fun countdown(x : int) =
    if x = 0
    then []
    else x :: countdown(x-1)


fun append(xs : int list, ys : int list) = 
    if null xs
    then ys
    else (hd xs) :: append((tl xs), ys)


fun sum_pair_list(xs : (int * int) list) = 
    if null xs
    then 0
    else (#1 (hd xs)) + (#2 (hd xs)) + sum_pair_list(tl xs)


val x = [1, 2, 3]
val y = countdown(7)
val z = append(x, y)