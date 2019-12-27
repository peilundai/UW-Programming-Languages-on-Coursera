
(* There is no assignment in ML, only binding, which cannot be changed *)



fun max(xs: int list) = 
    if null xs 
        then 0
    else if null (tl xs)
        then hd xs 
    else 
        let val m = max(tl xs)
        in
            if hd xs > m 
                then hd xs 
            else m 
        end


fun count_up(from : int, to : int) = 
    if from = to
        then from :: []
    else from :: count_up(from + 1, to) 


val x = count_up(1, 100)
val y = max(x)

