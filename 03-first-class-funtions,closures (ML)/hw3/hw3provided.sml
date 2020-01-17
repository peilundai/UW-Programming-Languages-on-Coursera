(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer



(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)



(* Question 1 *)

fun only_capitals (xs: string list) =
	List.filter (Char.isUpper o (fn (x) => String.sub(x, 0))) xs 

(* val xs = ["Hello", "world", "Today"]
val isupper = only_capitals (xs) *)



(* Question 2 *)
fun longest_string1 (xs: string list): string = 
	foldl (fn (s, accu) => if String.size s > String.size accu then s else accu) "" xs 

(* val xs = ["Hello,dadada", "world", "Today", "Hellll"]
val longest = longest_string1(xs) *)



(* Question 3 *)
fun longest_string2 (xs: string list): string = 
	foldl (fn (s, accu) => if String.size s >= String.size accu then s else accu) "" xs 



(* Quetsion 4 *)
fun longest_string_helper f xs = 
(* (int * int -> bool) -> string list -> string *)
	foldl (fn (s, accu) => if f(String.size s, String.size accu) then s else accu) "" xs 	

val longest_string3 = longest_string_helper (fn (x, y) => x > y)
val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

(* (* val xs = ["xxxxxxxx", "world", "Today", "Hellll", "zzzzzzzz"] *)

val x3 = longest_string3 xs
val x4 = longest_string4 xs *)



(* Question 5 *)
val longest_capitalized = (longest_string1 o only_capitals) 

(* val xs = ["xxxxxxxx", "world", "today", "eellll", "zzzzzzzz"] *)

(* val long_cap = longest_capitalized xs  *)




(* Question 6 *)
val rev_string = String.implode o rev o String.explode 



(* Question 7 *)
fun first_answer f xs = 
	(* (’a -> ’b option) -> ’a list -> ’b  *)
	case xs of 
		[] => raise NoAnswer
		| x::xs' => 
			case f x of 
				NONE => first_answer f xs' 
				| SOME v => v 


(* Question 8	*)
fun all_answers f xs = 
(* (’a -> ’b list option) -> ’a list -> ’b list option *)
	case xs of 
			[] 			=> SOME []
		| x::xs' 	=> 
			let val first_ans = f x  (* type: 'b list option *)
					val tl_ans = all_answers f xs' (* type: 'b list option *)
			in 
				case (first_ans, tl_ans) of 
						(NONE, _) 				=> NONE 
					| (SOME v, NONE) 		=> NONE 
					| (SOME v1, SOME v2) => SOME (v1@v2)
		  end


datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
	let
		val r = g f1 f2
	in
		case p of
				Wildcard          => f1 ()
			| Variable x        => f2 x
			| TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
			| ConstructorP(_,p) => r p
			| _                 => 0
			end


(* Question 9 *)
val count_wildcards = g (fn p => 1) (fn _ => 0)

val count_wild_and_variable_lengths = g (fn p => 1) String.size 

fun count_some_var (s, p) = g (fn x => 0) (fn x => if x = s then 1 else 0) p


(* Question 10 *)
fun check_pat p = 
	let 
		fun col_string p lst = 
			case p of 
					Wildcard						=> lst
				|	Variable x					=> x::lst
				| TupleP ps 					=> List.foldl (fn (p, lst) => (col_string p lst)) lst ps 
				| ConstructorP (_, p)	=> col_string p lst 
				| _ 									=> lst 
    fun check_rpt xs = 
			case xs of 
					[] 			=> true 
				| x::xs' 	=> not (List.exists ((fn s1 => fn s2 => s1 = s2) x) xs')
	in 
		check_rpt (col_string p [])
	end 

(* Question 11 *)
fun match (v : valu, p : pattern) : (string * valu) list option = 
	(* all_answers : (’a -> ’b list option) -> ’a list -> ’b list *)
	(* ListPair.zip *)
  case (p, v) of 
			(Wildcard, _) 				=>	SOME []
		| (Variable s, v) 			=>	SOME [(s, v)]
		| (UnitP, Unit) 				=>	SOME []
		| (ConstP i, Const j)		=>  if i = j then SOME [] else NONE 
		| (TupleP ps, Tuple vs)	=>	
				let 
					val zipped = ListPair.zip (vs, ps)
				in all_answers match zipped
				end 
		| (ConstructorP(s1, p_), Constructor(s2, v_))
														=> if s1 = s2 then match (v_, p_) else NONE 
		| _ 										=> NONE 


(* Question 12 *)
fun first_match v ps = 
	(* first_answer of type (’a -> ’b option) -> ’a list -> ’b  *)
	let 
		fun match_v p = match(v, p)
		val r = first_answer match_v ps
	in SOME r 
	end  



