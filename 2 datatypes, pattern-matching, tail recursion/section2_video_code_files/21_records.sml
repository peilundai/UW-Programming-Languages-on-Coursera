(* Programming Languages, Dan Grossman *)
(* Section 2: Records *)

val x = { bar = (1+2,true andalso true), foo = 3+4, baz = (false,9) }

val my_niece = {name = "Amelia", id = 41123 - 12}

val brain_part = {id = true, ego = false, superego = false}

val my_name = {last_name = "Dai", first_name = "Peilun" }

val first_name = #first_name my_name 

