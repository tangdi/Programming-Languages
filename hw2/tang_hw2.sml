(* Dan Grossman, CSE341 Winter 2013, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(s1: string, s_l: string list)=
  case s_l of
       [] => NONE
     | s::ss_l => let
                    val rest = all_except_option(s1, ss_l)
                  in
                    case rest of
                         NONE => if same_string(s1, s) then SOME ss_l else NONE
                       | SOME ss_l => if same_string(s1, s) then SOME ss_l else
                           SOME (s::ss_l)
                  end

fun get_substitutions1(sll: string list list, s: string) =
  case sll of
       [] => []
     | sl::sll2 =>
        let
          val sl1 = all_except_option(s, sl)
        in
          case sl1 of
               NONE => get_substitutions1(sll2, s)
             | SOME l => l @ get_substitutions1(sll2, s)
        end

fun get_substitutions2(sll: string list list, s: string) =
let
  fun helper(sll: string list list, s: string, accu: string list)=
    case sll of
         [] => accu
       | sl::sll2 =>
           let
             val sl1 = all_except_option(s, sl)
           in
             case sl1 of
                  NONE => helper(sll2, s, accu)
                | SOME l => helper(sll2, s, accu @ l)
            end
in
  helper(sll, s, [])
end

fun similar_names(sll: string list list, {first=x, last=z, middle=y}) =
    let
      val sl = get_substitutions2(sll, x)
      fun helper(sl: string list, accu: {first:string,middle:string,last:string}
        list)=
        case sl of
             [] => accu
           | s::sl2 => helper(sl2, accu @ [{first=s, middle=y, last=z}])
    in
      helper(x::sl,[])
    end




(* you may assume that Num is always used with values 2, 3, ..., 9
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* put your solutions for problem 2 here *)
fun card_color (c: card) =
  case c of
       (Clubs , _) => Black
     | (Spades , _) => Black
     | (Diamonds , _) => Red
     | (Hearts , _) => Red

fun card_value (c: card) =
  case c of
       (_, Num i) => i
     | (_, Ace) => 11
     | _ => 10

fun remove_card (card_list: card list, c: card, ex) =
  case card_list of
       [] => raise ex
     | c_h::card_list2 => if c_h = c then card_list2 else c_h ::
     remove_card(card_list2, c, ex)

fun all_same_color(card_list: card list) =
  case card_list of
       [] => raise IllegalMove
     | _::[] => true
     | c1::(c2::rest) => if card_color c1 = card_color c2 then
       all_same_color(c2::rest) else false

fun sum_cards(card_list: card list) =
let
  fun helper (card_list: card list, accu: int)=
    case card_list of
         [] => accu
       | card::cl2 => helper(cl2, accu + card_value card)
in
  helper(card_list, 0)
end

fun score(held_cards: card list, goal: int)=
let
  val sum = sum_cards(held_cards)
  val preliminary_score = if sum > goal then 3*(sum-goal) else goal-sum
in
  if all_same_color(held_cards) then preliminary_score div 2 else
    preliminary_score
end

fun officiate(card_list: card list, goal: int, moves: move list) =
let
  fun run (card_list: card list, moves: move list, held_cards: card list)=
    case moves of
         [] => score(held_cards, goal)
       | Discard c ::mvs2 => run(card_list, mvs2, remove_card
         (held_cards, c, IllegalMove))
       | Draw ::mvs2=> case card_list of
                            [] => score(held_cards, goal)
                          | card::cl2 => let
                                            val held_cards = card::held_cards
                                         in
                                           if sum_cards(held_cards) > goal then
                                             score(held_cards, goal)
                                           else run(cl2, mvs2, held_cards)
                                         end
in
  run (card_list, moves, [])
end

fun score_challenge(held_cards: card list, goal: int)=

let
  (*
  fun least_preliminary_score(held_cards: card list, goal: int) =
  case held_cards of
       (_, Ace) :: cl2 => let (sum, score) = least_preliminary_score(cl2, goal)
                          in
                            if sum >= goal then (sum+1, score + 3*1)
                            else if sum + 11 <= goal then (sum +11, score -11)
                            else if 3 * (sum+11 -goal) <= (goal - (sum+1))
                            then (sum+11, 3 * (sum+11-goal))
                            else (sum+1, goal -sum -1)
                          end
*)
  fun preliminary_score(sum: int) =
   if sum > goal then 3*(sum-goal) else goal-sum

  fun least_preliminary_score(held_cards: card list) =
  case held_cards of
       [] => (0, goal)
     | (_, Ace) :: cl2 => let val (sum, score) = least_preliminary_score(cl2)
                          in
                            if sum >= goal then (sum+1, preliminary_score(sum))
                            else if sum + 11 <= goal then (sum +11,
                            preliminary_score(sum + 11))
                            else if preliminary_score(sum+11) <=
                            preliminary_score(sum+1)
                            then (sum+11, preliminary_score(sum+11))
                            else (sum+1, preliminary_score(sum+1))
                          end

     | (_, Num i) :: cl2 => let val (sum, score) = least_preliminary_score(cl2)
                          in
                            (sum + i, preliminary_score(sum +i))
                          end
     | (_, _) :: cl2 => let val (sum, score) = least_preliminary_score(cl2)
                          in
                            (sum + 10, preliminary_score(sum +10))
                          end



  val (sum, pre_score) = least_preliminary_score(held_cards)
in
  if all_same_color(held_cards) then pre_score div 2 else pre_score
end


fun score_challenge2(held_cards: card list, goal: int)=

let
  fun preliminary_score(sum: int) =
   if sum > goal then 3*(sum-goal) else goal-sum

  fun extract_ace(held_cards: card list, accu: int) =
    case held_cards of
         [] => ([], accu)
       | (_, Ace) :: rest => let val (ace_list, sum) = extract_ace(rest,
       accu)
                             in
                               (Ace::ace_list, sum)
                             end
       | (_, Num i) :: rest => extract_ace(rest, accu + i)
       | (_, _) :: rest => extract_ace(rest, accu + 10)

  fun get_least_pre_score(held_cards) =
  let
    fun add_for_each(sum_list: int list, add: int) =
      case sum_list of
           [] => []
         | x::rest => (x+add)::add_for_each(rest, add)
    fun get_possible_sum_list(ace_list: rank list, accu: int) =
      case ace_list of
           [] => [accu]
         | ace::rest_ace => add_for_each(get_possible_sum_list(rest_ace, accu),
           1) @ add_for_each(get_possible_sum_list(rest_ace, accu), 11)

    fun map_to_pre_score(sum_list: int list) =
      case sum_list of
           [] => []
         | x::rest => preliminary_score(x) :: map_to_pre_score(rest)
    fun min (int_list: int list) =
      case int_list of
           [] => NONE
         | x::[] => SOME x
         | x::y::rest => if x<=y then min(x::rest) else min(y::rest)
    val (ace_list, rest_sum) = extract_ace(held_cards, 0)
  in
    case ace_list of
         [] => preliminary_score(rest_sum)
       | ace_list =>
           case min(map_to_pre_score(get_possible_sum_list(ace_list, rest_sum)))
             of
                NONE => 0
              | SOME x => x
  end
  val pre_score = get_least_pre_score(held_cards)
in
  if all_same_color(held_cards) then pre_score div 2 else pre_score
end


