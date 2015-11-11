use "tang_hw1.sml";

is_older((14, 15, 16), (14, 15, 17)) = true;
is_older((15, 15, 16), (14, 15, 17)) = false;

number_in_month([], 5) = 0;
number_in_month([(1,2,3), (1,4,5), (1,5,6)], 5) = 1;

number_in_months([(1,5,3), (1,5,5), (1,5,6)], [5]) = 3;
number_in_months([(1,5,3), (1,5,5), (1,5,6)], []) = 0;

dates_in_month([], 5) = [];
dates_in_month([(1,2,3), (1,4,5), (1,5,6)], 5) = [(1,5,6)];
dates_in_month([(1,5,3), (1,5,5), (1,5,6)], 5) = [(1,5,3), (1,5,5),(1,5,6)];


dates_in_months([], [5]) = [];
dates_in_months([(1,2,3), (1,4,5), (1,5,6)], [2, 4, 5]) = [(1,2,3), (1,4,5), (1,5,6)];

get_nth(["1", "2"], 2) = "2";
get_nth(["1", "2", "3"], 3) = "3";

date_to_string((2013, 1, 20)) = "January 20, 2013";

number_before_reaching_sum([100, 30, 50], 131) = 2;

what_month(30)=1;
what_month(61)=3;

