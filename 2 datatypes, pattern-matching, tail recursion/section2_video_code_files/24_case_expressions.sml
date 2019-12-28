(* Programming Languages, Dan Grossman *)
(* Section 2: Case Expressions *)

datatype mytype = TwoInts of int * int 
                | Str of string 
                | Pizza

fun f x = 
    case x of 
	Pizza => 3 
      | Str s => 8
      | TwoInts(i1,i2) => i1 + i2

(*    | Pizza => 4; (* redundant case: error *) *)
(*fun g x = case x of Pizza => 3 (* missing cases: warning *) *)

datatype my_info = LastName of string 
                   | FirstName of string 
                   | Age of int 


fun f2(info : my_info): string = 
    case info of 
    LastName last_name => last_name
    | FirstName first_name => first_name
    | Age age => "Hello, world"


val my_information = f2 (Age 29)


datatype exp = 
    Constant of int 
    | Negate of exp 
    | Add of exp * exp 
    | Multiply of exp * exp 


fun eval e = 
    case e of 
          Constant i          => i
        | Negate e1         => ~(eval e1)
        | Add (e1, e2)      => (eval e1) + (eval e2)
        | Multiply (e1, e2) => (eval e1) * (eval e2)


val e =  Add (Constant 19, Negate(Constant 400))
val eval_e = eval e 
