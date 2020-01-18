
(* Coursera Programming Languages
Homework 1
Author: Peilun Dai, peilun@bu.edu 
Date: 2020-01-06 *)

(* Question 1 *)

fun is_older (day1: int * int * int , day2: int * int * int ): bool = 
  if #1 day2 < #1 day1 
  then false 
  else if #1 day2 = #1 day1 andalso #2 day2 < #2 day1  
  then false 
  else if #1 day2 = #1 day1 andalso #2 day2 = #2 day1 andalso #3 day2 < #3 day1 
  then false  
  else if #1 day2 = #1 day1 andalso #2 day2 = #2 day1 andalso #3 day2 = #3 day1 
  then false 
  else true 
  

(* Question 2 *)
fun number_in_month (xs: (int * int * int ) list, m: int): int = 
  case xs of 
    [] => 0
    | x::xs' => 
      if (#2 x) = m
      then 1 + number_in_month(xs', m)
      else number_in_month(xs', m)


(* Question 3 *)
fun number_in_months (xs: (int * int * int ) list, ms: int list): int = 
  case ms of 
    [] => 0
    | m::ms' => number_in_month(xs, m) + number_in_months(xs, ms')


(* Question 4 *)
fun dates_in_month (ds: (int * int * int ) list, m: int):  (int * int * int ) list =
  case ds of 
    [] => []
    | d::ds' => 
      if (#2 d) = m 
      then d::dates_in_month (ds', m)
      else dates_in_month (ds', m)


(* Question 5 *)
fun dates_in_months (ds: (int * int * int) list, ms: int list): (int * int * int ) list =
  case ms of 
    [] => []
    | m::ms' =>
      dates_in_month(ds, m)@dates_in_months (ds, ms')


(* Question 6 *)
fun get_nth (xs: string list, n: int): string =
  if n = 1
  then hd xs 
  else get_nth (tl xs, n-1)


(* Question 7 *)
fun date_to_string (d: int * int * int ): string = 
  let val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
  in 
    get_nth(months, #2 d)^" "^Int.toString(#3 d)^", "^Int.toString(#1 d)
  end 


(* Question 8 *)
fun number_before_reaching_sum (sum, xs) = 
  let 
    fun cum_fn (sum, xs, cum, n) = 
      case xs of 
        x::[] => if cum < sum andalso cum + x >= sum then n else n+1
        | x::xs' => if (cum < sum andalso cum + x >= sum) then n else cum_fn
        (sum, xs', cum + x, n + 1)
  in 
    case xs of 
      [] => 0
      | _ => cum_fn (sum, xs, 0, 0)
  end 


(* Question 9 *)
fun what_month (d: int): int = 
  let val days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in number_before_reaching_sum(d, days_in_months) + 1
  end 


(* Question 10 *)
fun month_range (day1, day2) = 
  if day1 > day2 
  then []
  else what_month(day1)::month_range(day1+1, day2)


(* Question 11 *)
fun oldest(dates: (int * int * int ) list): (int * int * int ) option = 
(* assuming at least one element *)
  case dates of 
      [] => NONE
    | d::[] =>  SOME d 
    | d::ds =>  let val oldest_tl = valOf(oldest(ds))
                in 
                  if is_older(d, oldest_tl)
                  then SOME d 
                  else SOME oldest_tl
                end

(* Question 12 *)
fun number_in_months_challenge(xs, ms) = 
  let 
    fun ele_in_list (ele, xs) = 
      if null xs 
      then false 
      else (hd xs) = ele orelse ele_in_list(ele, tl xs)
    fun remove_dup (xs) = 
      if null xs 
      then [] 
      else if ele_in_list(hd xs, tl xs) then remove_dup(tl xs) else (hd
      xs)::remove_dup(tl xs)
    val no_dup = remove_dup(ms)
  in number_in_months(xs, no_dup)
  end 
      
fun dates_in_months_challenge(ds, ms) = 
  let 
    fun ele_in_list (ele, xs) = 
      if null xs 
      then false 
      else (hd xs) = ele orelse ele_in_list(ele, tl xs)
    fun remove_dup (xs) = 
      if null xs 
      then [] 
      else if ele_in_list(hd xs, tl xs) then remove_dup(tl xs) else (hd
      xs)::remove_dup(tl xs)
    val no_dup = remove_dup(ms)
  in dates_in_months(ds, no_dup)
  end 


(* Question 13 *)



