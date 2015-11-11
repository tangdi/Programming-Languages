fun is_older(a: int*int*int, b:int*int*int)=
    if #1 a < #1 b
    then true
    else if #1 a > #1 b
        then false
            else if #2 a < #2 b then true
                else if #2 a > #2 b
                    then false
                        else if #3 a < #3 b
                             then true
                                 else false


fun number_in_month(a: (int * int * int) list, b: int) =
  if null a
  then 0
  else let
    val rest = number_in_month(tl a, b)
       in
         if #2 (hd a) = b then rest +1 else rest
       end

fun number_in_months(a: (int * int * int) list, b: int list) =
  if null a orelse null b
  then 0
  else number_in_month(a, hd b) + number_in_months(a, tl b)

fun dates_in_month(a: (int * int * int) list, b: int) =
  if null a
  then a
  else let
    val rest = dates_in_month(tl a, b)
       in
         if #2 (hd a) = b
         then hd a :: rest
         else rest
       end

fun dates_in_months(a: (int * int * int) list, b: int list) =
  if null a orelse null b
  then []
  else dates_in_month (a, hd b) @ dates_in_months(a, tl b)

fun get_nth(strings: string list, n: int) =
    if n =1
    then hd strings
    else get_nth(tl strings, n-1)

fun date_to_string(date: int*int*int) =
    let
      val months = ["January", "February", "March", "April", "May", "June",
      "July", "August", "September", "October", "November", "December"]
    in
      get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

fun number_before_reaching_sum(intList: int list, sum: int)=
  if null intList orelse hd intList >= sum
  (* should throw exception *)
  then 0
  else 1 + number_before_reaching_sum(tl intList, sum - hd intList)

fun what_month (day: int) =
    let
      val months = [30, 30, 30, 30, 30, 30, 30, 30]
    in
      1 + number_before_reaching_sum(months, day)
    end

fun month_range(day1: int, day2:int) =
  if day1 > day2
  then []
  else what_month(day1) :: month_range(day1+1, day2)

fun oldest(dates: (int*int*int) list) =
  if null dates
  then NONE
  else let
        val old = oldest(tl dates)
       in
         if isSome old  andalso is_older(valOf old, hd dates)
         then old
         else SOME (hd dates)
       end

fun reasonable_date(date: int*int*int) =
let
  fun is_leap_year(date: int * int* int) =
    if #3 date mod 400 = 0
    then true
    else if (#3 date mod 4 =0) andelse (not #3 date mod 100 =0)
    then true
    else false
in
  if #1 date <= 0 orelse #3 date <=0
  then false
  else if #2 date < 1 orelse #2 date >12
  then false
  else if is_leap_year date
  then if #3 date > 366 then false else true
  else if #3 date > 365 then false else true
