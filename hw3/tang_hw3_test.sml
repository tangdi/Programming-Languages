use "tang_hw3.sml";
only_capitals([]) = [];
only_capitals(["a"]) = [];
only_capitals(["a", "Abc"]) = ["Abc"];

longest_string1([]) = "";
longest_string1(["abc", "cde"]) = "abc";
longest_string1(["abc", "cde", "33333"]) = "33333";

longest_string2([]) = "";
longest_string2(["abc", "cde"]) = "cde";
longest_string2(["abc", "cde", "33333"]) = "33333";

longest_string3([]) = "";
longest_string3(["abc", "cde"]) = "abc";
longest_string3(["abc", "cde", "33333"]) = "33333";

longest_string4([]) = "";
longest_string4(["abc", "cde"]) = "cde";
longest_string4(["abc", "cde", "33333"]) = "33333";

longest_capitalized([]) = "";
longest_capitalized(["abc", "cde"]) = "";
longest_capitalized(["Abc", "Cde", "33333"]) = "Abc";
longest_capitalized(["Abc", "Cde", "D3333"]) = "D3333";

rev_string "" = "" ;
rev_string "abcde" = "edcba" ;

all_answers (fn a => SOME [a]) [] = SOME [];


val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1, 2, 3, 4,
5] = 4;
val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2, 3, 4, 5]
= NONE;

all_answers (fn x => SOME []) [1] = SOME [];


count_wildcards Wildcard = 1;
count_wildcards (TupleP [Wildcard, Wildcard, Wildcard]) =3;
count_wildcards (ConstructorP ("abc", TupleP [Wildcard, Wildcard, Wildcard])) =3;
count_wildcards (Variable "a") = 0;


count_wild_and_variable_lengths(TupleP [Wildcard, Variable "doc"]) = 4;
count_some_var("a", TupleP [Variable "a", UnitP, Variable "a"]) = 2;

check_pat Wildcard = true;
check_pat (Variable "a") = true;
check_pat (TupleP [Variable "a", UnitP, Variable "a"]) = false;
check_pat (TupleP [Variable "a", UnitP, TupleP [Variable "a", UnitP]]) = false;
check_pat (TupleP [Variable "a", UnitP, TupleP [Variable "b", UnitP]]) = true;



match(Const 1, TupleP [ConstP 1]) = NONE;
match(Const 1, Variable "cat") = SOME [("cat", Const 1)];
match(Tuple [Const 1], TupleP [ConstP 1]) = SOME [];
match(Const 1, ConstP 1) = SOME [] ;


first_match Unit [UnitP] = SOME [];
first_match Unit [ConstP 6] = NONE;
