(* Question 1 *)
fun alternate xs = 
    if null xs
    then 0
    else hd xs - alternate (tl xs)

val xs1 = [1, 2, 3, 4, 5]
val r1 = alternate xs1

(* Question 2 *)
fun min_max (xs: int list): int * int = 
    if null xs 
    then 
        (0, 0)
    else 
        let 
            val (min, max) = min_max(tl xs)
        in (
            if hd xs < min 
            then hd xs 
            else max,

            if hd xs > max 
            then hd xs 
            else max
        )
        end

val a = [1, 2, 3, 2, 1]
val (min, max) = min_max a


(* Question 3  *)
fun cumsum (xs)= 
    let 
        fun add_n (xs, n) = 
            if null xs
            then []
            else (hd(xs)+n) :: add_n(tl(xs), n)
    in 
        if null xs 
        then []
        else 
            (hd xs) :: add_n(cumsum(tl(xs)), hd(xs))
    end

val xs3 = [1, 4, 20]
val r3 = cumsum (xs3)


(* Question 4  *)
fun greeting (s: string option) : string = 
    if isSome(s)
    then "Hello there, " ^ valOf(s)
    else "Hello there, you."

val greeting = greeting(SOME("Peilun"))

(* Question 5 *)
fun repeat (xs, ys) = 
    if null xs
    then []
    else 
        if hd ys = 0
        then repeat(tl xs, tl ys)
        else (hd xs) :: repeat(xs, hd ys -1 :: tl ys)

val xs5 = [1, 2, 3]
val ys5 = [4, 0, 3]
val r5 = repeat(xs5, ys5)


(* Question 6 *)
fun addOpt(x: int option, y : int option) = 
    if isSome x andalso isSome y
    then SOME(valOf(x) + valOf(y))
    else NONE

val x6 = SOME(10)
val y6 = SOME(3)
val r6 = addOpt(x6, y6)


(* Question 7 *)
fun addAllOpt(xs: int option list): int option = 
    if null xs 
    then NONE 
    else 
        let 
            val rst = addAllOpt(tl xs)
        in 
            if hd xs = NONE andalso rst = NONE
            then NONE 
            else if isSome(hd xs) andalso rst = NONE 
            then hd xs 
            else if isSome(hd xs) andalso isSome rst 
            then SOME(valOf(hd xs) + valOf(rst))
            else rst 
        end
        
val xs7 = [SOME 1,NONE,SOME 3]
val r7 = addAllOpt(xs7)


(* Question 8 *)
fun any(bs) = 
    if null bs 
    then false
    else hd bs orelse any(tl bs) 
        
val bs = [false, true]
val rs = any(bs)


(* Question 9 *)
fun all bs = 
    if null bs 
    then true
    else hd bs andalso all (tl bs)

val bs9 = [true, true]
val r9 = all bs9  

(* Question 10 *)
fun zip(xs: int list, ys: int list): (int * int) list = 
    if (null xs orelse null ys)
    then []
    else (hd xs, hd ys) :: zip (tl xs, tl ys)

val r10 = zip([1, 2, 3], [4, 6]) 
     
