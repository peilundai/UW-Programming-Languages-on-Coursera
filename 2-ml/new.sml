

datatype mytype = 
  TwoInts of int * int
  | Str of string 
  | Pizza 

fun f x = 
  case x of 
    Pizza => 3
    | TwoInts(i1, i2) => i1 + i2
    | Str s => String.size s 


(* val a = TwoInts(1, 2)
val b = f a  *)

datatype exp = 
  Constant of int 
  | Negate of exp 
  | Add of exp * exp 
  | Multiply of exp * exp 

val a = Add (Constant (10 + 9), Negate (Constant 4))

fun eval e = 
  case e of 
    Constant i          => i 
    | Negate e2         => ~(eval e2) 
    | Add(e1, e2)       => (eval e1) + (eval e2)
    | Multiply(e1, e2)  => (eval e1) * (eval e2)
  
(* val b = eval a  *)


fun max_const e = 
  let 
    fun max_of_two (e1, e2) = 
      let val v1 = max_const e1 
          val v2 = max_const e2
      in if (v1 > v2) then v1 else v2 
      end 
  in 
    case e of 
      Constant i        => i 
      | Negate e1       => max_const e1 
      | Add(e1, e2)     => max_of_two(e1, e2)
      | Multiply(e1, e2)=> max_of_two(e1, e2)
  end 

val b = max_const(a)


(* init list are datatypes  *)
datatype my_int_list = Empty | Cons of int * my_int_list

val x = Cons(4, Cons(23, Cons(2008, Empty)))

fun append_my_list (xs, ys) = 
  case xs of 
    Empty => ys 
    | Cons(x, xs') => Cons(x, append_my_list(xs', ys))


(* options are datatypes  *)
datatype intoption = NONE | SOME of int 
val c = SOME ~1

fun inc_of_zero_int c = 
  case c of 
    NONE => 0 
    | SOME i => i+1

val d = inc_of_zero_int c 


fun sum_list xs = 
  case xs of 
    [] => 0
    | x::xs' => x + sum_list xs' 

fun append (xs, ys) = 
  case xs of 
    [] => ys 
    | x::xs' => x::append(xs', ys)

(* Nested patterns *)
