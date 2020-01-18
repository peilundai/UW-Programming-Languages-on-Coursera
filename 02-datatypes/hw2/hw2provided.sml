(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option (str: string, str_lst: string list) =
  let 
    fun aux (str_lst_) = 
      case str_lst_ of 
        [] => (false, [])
        | x::xs' => 
          let val (has_string, lst) = aux(xs')
          in
            if x = str 
            then (true, lst)
            else (has_string, x::lst)
          end 
  in 
    let val (has_ele, lst) = aux(str_lst)
    in if has_ele then SOME lst else NONE
    end 
  end 

fun get_substitutions1 (subs, name) = 
  case subs of 
    [] => []
    | x::xs => 
      case all_except_option(name, x) of 
        NONE => get_substitutions1(xs, name)
        | SOME lst => (lst@get_substitutions1(xs, name))


fun get_substitutions2 (subs, name) = 
  let  
    fun aux (subs_remained , so_far): 
    string list =
      case subs_remained of 
        [] => so_far 
        | x::xs => 
          case all_except_option(name, x) of 
            NONE => aux (xs, so_far)
            | SOME lst => aux(xs, lst@so_far)
  in 
    aux (subs, [])
  end 

fun similar_names (subs, full_name) = 
  let 
    val {first=ff, middle=mm, last=ll} = full_name
    fun helper(lst, full_name) = 
      let 
        val {first=f, middle=m, last=l} = full_name
      in 
        case lst of 
          [] => []
          | x::xs => {first=x, middle=m, last=l}::helper(xs, full_name)
      end
  in 
    full_name::helper(get_substitutions1(subs, ff), full_name)
  end 


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color(x: card): color = 
  case x of 
    (Diamonds, _) => Red
    | (Hearts, _) => Red
    | _ => Black
    

fun card_value(x: card): int = 
  case x of 
      (_, Num n) => n
    | (_, Ace) => 11
    | _ => 10


fun remove_card(cs: card list, c: card, e: exn) = 
  let 
    fun remove_card_flag (cs, has_removed) = 
      case cs of 
        [] => if has_removed then [] else raise e
        | x::xs => 
          if x = c andalso (not has_removed) 
          then remove_card_flag(xs, true)
          else x::remove_card_flag(xs, has_removed)
  in 
    remove_card_flag (cs, false)
  end 


fun all_same_color (xs) =
  case xs of 
    [] => true 
    | x::[] => true 
    | x::x'::xs' => 
      if card_color(x) = card_color(x') andalso all_same_color(x'::xs')
      then true
      else false 
 
fun sum_cards (xs) = 
  let fun cum (xs, s) = 
        case xs of 
          [] => s
          | x::xs' => cum(xs', s+card_value(x))
  in cum (xs, 0)
  end 
  
fun score(cs, goal) = 
  let
    val raw = sum_cards(cs)
    val are_same_color = all_same_color(cs)
  in 
    if raw > goal andalso are_same_color 
    then (3 * (raw - goal)) div 2
    else if raw > goal
    then 3  * (raw - goal)
    else if are_same_color
    then (goal - raw) div 2
    else (goal - raw)
  end


fun officiate (card_list, move_list, goal) = 
  let 
    fun transition(card_list, held_cards, move_list, goal) = 
      case move_list of 
        [] => score (held_cards, goal)
        | x::xs' => 
            case x of 
              Draw => 
                if null card_list 
                then score(held_cards, goal)
                else             
                  transition ((tl card_list), (hd(card_list)::held_cards), xs', goal)
            | Discard(i) => transition (card_list, remove_card(held_cards, i, IllegalMove), xs', goal)
  in 
    transition(card_list, [], move_list, goal)
  end 



