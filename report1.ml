(* Exercise 2.6 *)

let usdrate = 111.12;;
let yen_of_usdollar usd =
  int_of_float (floor (usd *. usdrate +. 0.5))
;;
let usdollar_of_yen yen =
  (floor ((float_of_int yen) /. usdrate *. 100.0 +. 0.5)) /. 100.0
;;
let yen_of_usdollar_message usd =
  string_of_float usd ^ " dollars are " ^ string_of_int(yen_of_usdollar usd) ^ " yen."
;;
let capitalize moji =
  if (int_of_char moji >= int_of_char 'a' && int_of_char moji <= int_of_char 'z')
      then char_of_int ((int_of_char moji) - 32)
  else moji
;;

(* Exercise 3.7 *)

let rec pow1 (x, n) =
  if n=0 then 1.0
  else x *. pow1 (x, n-1)
;;
let rec pow2 (x, n) =
  if n=0 then 1.0
  else if n mod 2=0 then let powdata = pow2 (x, n/2) in powdata *. powdata
  else let powdata = pow2 (x, n/2) in x *. powdata *. powdata
;;

(*Excercise 3.11*)
let rec gcd (m, n) =
  if m<=n then if m=0 then n
               else gcd (m, n mod m)
  else if n=0 then m
       else gcd (m mod n, n)
;;
let rec comb (n, m) =
  if (m=0||m=n) then 1
  else comb(n-1, m)+comb(n-1,m-1)
;;
let rec fib_iter_pre (num, res) =
  if num=1 then res
  else fib_iter_pre (num-1, res*num)
;;
let fib_iter num = fib_iter_pre (num, 1)
;;
let rec max_ascii_pre (num, max, sent) =
  if num = String.length sent then max
  else if int_of_char sent.[num] > max then max_ascii_pre(num+1, int_of_char sent.[num], sent)
  else max_ascii_pre(num+1, max, sent)
;;
let max_ascii sent = char_of_int (max_ascii_pre (0, 0, sent))
;;
    
(* Exercise 4.1 *)
let rec integral f a b =
  let dx = 0.1e-10 in
  if a>=b then 0.
  else (integral f (a+.dx) b) +. ((f a)+.(f (a+.dx))/.2.)
;;
