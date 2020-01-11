use "hw1.sml";

(* Q1 test *)
val date1 = (1999, 7, 27)
val date2 = (1990, 7, 27)
val older2 = is_older (date1, date2)


(* Q2 test  *)
val date_list = [(1998, 5, 4), (1999, 6, 1), (1990, 6, 6)]
val in_june = number_in_month(date_list, 6)
val in_may = number_in_month(date_list, 5)

(* Q3 test  *)
val m_list = [1, 2, 3, 4]
val m_list2 = [4, 5]
val m_list3 = [5, 6]

val r31 = number_in_months (date_list, m_list)
val r32 = number_in_months (date_list, m_list2)
val r33 = number_in_months (date_list, m_list3)

(* Q4 test  *)
val date_in_june = dates_in_month (date_list, 6)
val date_in_may = dates_in_month (date_list, 5)
val date_in_jan = dates_in_month (date_list, 1)

(* Q5 test  *)
val date_in_ms = dates_in_months(date_list, m_list3)

(* Q6 test  *)
val string_list = ["hello", "world", "peter"]
val string_returned = get_nth(string_list, 1)


(* Q7 test  *)
val date_string = date_to_string((1990, 7, 27))


(* Q8 test  *)
val val_list = [1, 2, 3, 4]
val sum = 7
val n8 = number_before_reaching_sum(sum, val_list)

(* Q9 test  *)
val day_number = 120
val which_month = what_month(day_number)


(* Q10 test  *)
val day1_number = 120
val day2_number = 160
val list_of_months = month_range(120, 160)


(* Q11 test  *)
val dates_oldest = [(1990,01,01), (1999,12,20), (1989,12,31)]
val oldest_date = oldest(dates_oldest)
