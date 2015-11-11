(* Dan Grossman, CSE341 Winter 2013, HW2 Provided Tests *)

(* These are just two tests for problem 2; you will want more.

   Naturally these tests and your tests will use bindings defined
   in your solution, in particular the officiate function,
   so they will not type-check if officiate is not defined.
 *)
use "tang_hw2.sml";

all_except_option("a", []) = NONE;

all_except_option("a", ["a"]) = SOME [];

all_except_option("a", ["b", "a", "c"]) = SOME ["b", "c"];

get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
"Fred") = ["Fredrick","Freddie","F"];

get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],
"Jeff") = ["Jeffrey","Geoff","Jeffrey"];

get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
"Fred") = ["Fredrick","Freddie","F"];

get_substitutions2([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],
"Jeff") = ["Jeffrey","Geoff","Jeffrey"];

similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
{first="Fred", middle="W", last="Smith"}) =
 [{first="Fred", last="Smith", middle="W"},
{first="Fredrick", last="Smith", middle="W"},
{first="Freddie", last="Smith", middle="W"},
{first="F", last="Smith", middle="W"}] ;

card_color(Clubs, Jack) = Black;
card_color(Hearts, Jack) = Red;
card_color(Diamonds, Jack) = Red;
card_color(Spades, Jack) = Black;

card_value(Clubs, Jack) = 10;
card_value(Hearts, Ace) = 11;
card_value(Hearts, Num 3) = 3;

(remove_card([], (Clubs, Jack), IllegalMove) handle IllegalMove => []) =[];
remove_card([(Clubs, Jack)], (Clubs, Jack), IllegalMove) =[];
remove_card([(Clubs, Jack), (Clubs, Ace)], (Clubs, Jack), IllegalMove) =[(Clubs,
Ace)];

all_same_color([(Clubs, Jack), (Spades, Ace)]) = true;
all_same_color([(Clubs, Jack)]) = true;
all_same_color([(Clubs, Jack), (Hearts, Ace)]) = false;
all_same_color([(Clubs, Jack), (Spades, Ace), (Clubs, Jack), (Spades, Ace),
(Hearts, Ace)]) = false;

sum_cards([(Clubs, Jack), (Spades, Ace)]) = 21;
sum_cards([(Clubs, Jack), (Spades, Ace), (Clubs, Num 9), (Spades, Ace)]) = 41;

score([(Clubs, Jack), (Spades, Ace), (Clubs, Num 9), (Spades, Ace)], 41) = 0;
score([(Clubs, Jack), (Spades, Ace), (Clubs, Num 9), (Spades, Ace)], 40) = 1;
score([(Clubs, Jack), (Spades, Ace), (Clubs, Num 9), (Spades, Ace)], 39) = 3;

(officiate([], 1, [Discard (Spades, Ace)]) handle IllegalMove => ~1) = ~1;
(officiate([(Clubs, Num 1)], 1, [Draw, Draw]) handle IllegalMove => ~1) = 0;

score_challenge2([(Hearts, Ace), (Spades, Num 3), (Clubs, Ace)], 22) = 7;
score_challenge([(Hearts, Ace), (Spades, Num 3), (Clubs, Ace)], 22) = 7;

