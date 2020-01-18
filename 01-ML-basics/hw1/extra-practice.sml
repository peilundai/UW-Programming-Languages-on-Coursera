
(* Write a function 
alternate : int list -> int that takes a list of numbers and adds them with alternating sign. For example 
alternate[1,2,3,4]=1-2+3-4=-2. *)

fun alternate xs = 
    if null xs
    then 0
    else hd xs - alternate (tl xs)

val xs1 = [1, 2, 3, 4, 5]
val r1 = alternate xs1

(* Question 2 Write a function 
min_max : int list -> int * int that takes a non-empty list of numbers, and returns a pair 
(min,max) of the minimum and maximum of the numbers in the list.*)
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


(* Question 3  Write a function cumsum:intlist->intlist that takes a list of numbers and returns a 
list of the partial sums of those numbers. For example 
cumsum[1,4,20]=[1,5,25].*)
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


(* Question 4  Write a function 
greeting:stringoption->string that given a string option 
SOME
SOME name returns the string 
"Hello there, ...!"
"Hellothere,...!" where the dots would be replaced by name. Note that the name is given as an option, so if it is 
NONE
NONE then replace the dots with 
"you"
"you".*)
fun greeting (s: string option) : string = 
    if isSome(s)
    then "Hello there, " ^ valOf(s)
    else "Hello there, you."

val greeting = greeting(SOME("Peilun"))

(* Question 5 Write a function 
repeat:intlist*intlist->intlist that given a list of integers and another list of nonnegative integers, repeats the integers in the first list according to the numbers indicated by the second list. For example: 
repeat ([1,2,3], [4,0,3]) = [1,1,1,1,3,3,3]
repeat([1,2,3],[4,0,3])=[1,1,1,1,3,3,3].*)
fun repeat (xs, ys) = 
    if null xs
    then []
    else 
        if hd ys = 0
        then repeat(tl xs, tl ys)
        else (hd xs) :: repeat(xs, (hd ys) -1 :: (tl ys))

val xs5 = [1, 2, 3]
val ys5 = [4, 0, 3]
val r5 = repeat(xs5, ys5)


(* Question 6 Write a function 
addOpt  : int option * int option -> int option that given two "optional" integers, adds them if they are both present (returning 
SOME of their sum), or returns 
NONE if at least one of the two arguments is 
NONE.*)
fun addOpt(x: int option, y : int option) = 
    if isSome x andalso isSome y
    then SOME(valOf(x) + valOf(y))
    else NONE

val x6 = SOME(10)
val y6 = SOME(3)
val r6 = addOpt(x6, y6)


(* Question 7 Write a function 
addAllOpt  : int option list -> int option
that given a list of "optional" integers, adds those integers that are there (i.e. adds all the 
SOME i. For example: 
addAllOpt ([SOME 1, NONE, SOME 3]) = SOME 4
addAllOpt([SOME1,NONE,SOME3])=SOME4. If the list does not contain any 
SOME i
SOMEis in it, i.e. they are all 
NONE or the list is empty, the function should return 
NONE.*)
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


(* Question 8 Write a function 
any : bool list -> bool that given a list of booleans returns true
true if there is at least one of them that is 
true, otherwise returns 
false. (If the list is empty it should return 
false because there is no 
true.)*)
fun any(bs) = 
    if null bs 
    then false
    else hd bs orelse any(tl bs) 
        
val bs = [false, true]
val rs = any(bs)


(* Question 9 Write a function 
all : bool list -> bool
all:boollist->bool that given a list of booleans returns 
true
true if all of them 
true
true, otherwise returns 
false
false. (If the list is empty it should return 
true
true because there is no 
false
false.)*)
fun all bs = 
    if null bs 
    then true
    else hd bs andalso all (tl bs)

val bs9 = [true, true]
val r9 = all bs9  

(* Question 10 Write a function zip:int list * int list->int * int list that given two lists of integers creates consecutive pairs, and stops when one of the lists is empty. For example: 
zip ([1,2,3], [4, 6]) = [(1,4), (2,6)]*)
fun zip(xs: int list, ys: int list): (int * int) list = 
    if (null xs orelse null ys)
    then []
    else (hd xs, hd ys) :: zip (tl xs, tl ys)

val r10 = zip([1, 2, 3], [4, 6]) 
     



