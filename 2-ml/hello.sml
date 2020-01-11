
datatype mytype = TwoInts of int * int
                | Str of string 
                | Pizza 

fun f x = 
    case x of 
        Pizza => 3
        | TwoInts(i1, i2) => i1 + i2
        | Str s => String.size s 


(* val a = TwoInts(1, 2)
val b = f a  *)

datatype exp = Constant of int 
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
  
val b = eval a 


fun max_const e = 
    case e of 
        Constant i          => i 
        | Negate e1         => (max_const e1) 
        | Add(e1, e2)       =>  let val maxE1 = max_const e1 
                                   val maxE2 = max_const e2 
                                in 
                                    if (maxE1 > maxE2)
                                    then maxE1 
                                    else maxE2
                                end
        | Multiply(e1, e2)  =>  let 
                                
                            => 


fun max_const2 e = 