(* functions that return functions *)
(* fun double_or_triple f = 
  if f 7
  then fn x => 2*x
  else fn x => 3*x 

val double = double_or_triple (fn x => x - 3 = 4)
val triple = double_or_triple (fn x => x = 42) 3

val four = double 2 *)

(* higher-order functions over our own datatype bindings *)

(* datatype exp = Constant of int
              | Negate of exp
              | Add of exp * exp 
              | Multiply of exp * exp *)



(* given an exp, is every constant in it an even number? *)

(* fun true_of_all_constants(f, e) = 
  case e of 
    Constant i => f i
    | Negate e1 => true_of_all_constants(f, e1)
    | Add(e1, e2) => true_of_all_constants(f, e2)
                      andalso true_of_all_constants(f, e1)
    | Multiply(e1, e2) => true_of_all_constants(f, e2)
                      andalso true_of_all_constants(f, e1)                     

fun all_even e = true_of_all_constants((fn x => x mod 2 = 0), e) *)

(* Lexical scope *)
(* function bodies can use any bindings in scope
where the functions was defined not where it was called *)
(* Lexical scope *)

(* val x = 1
fun f y = x + y

val x = 2
val y = 3
val z = f (x + y) *)

(* function closures *)
 (* a function value has two parts 
 - the code - the environment that was current when the function was defined *)
(* - the environment that was current when the function was defined *)
(* The pair is called function closure *)

(* Lexical scope vs dynamic scope *)

(* val x =1 
fun f y = 
  let 
    val x = y + 1
  in 
    fn z => x + y + z
  end 

val x = 3
val g = f 4
val y = 5
val z = g 6 *)

(* Filter function *)
fun filter (f, xs) = 
  case xs of 
    [] => []
    | x::xs' => if f x then x::(filter(f, xs')) else (filter(f, xs'))

