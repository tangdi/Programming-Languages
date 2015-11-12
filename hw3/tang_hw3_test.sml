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

