(* Dan Grossman, CSE341 Winter 2013, HW3 Provided Code *)

exception NoAnswer

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

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
val only_capitals = List.filter (fn s => Char.isUpper(String.sub (s, 0)) )
val longest_string1 = List.foldl (fn (a, b) => if (String.size b) >= (String.size a) then b else a) "";
val longest_string2 = List.foldl (fn (a, b) => if (String.size b) > (String.size a) then b else a) "";

fun longest_string_helper pre initial_string string_list =
  case string_list of
       [] => initial_string
     | x::rest => if pre (String.size x, String.size initial_string)
                  then longest_string_helper pre x rest
                  else longest_string_helper pre initial_string rest

val longest_string3 = longest_string_helper (fn (a, b) => a>b) ""
val longest_string4 = longest_string_helper (fn (a, b) => a>=b) ""

val longest_capitalized = longest_string3 o only_capitals

val rev_string = String.implode o List.rev o String.explode
