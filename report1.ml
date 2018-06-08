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

(*Excercise 3.11 *)
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
let fib_iter num =
  let rec fib_iter_pre (num, res) =
    if num=1 then res
    else fib_iter_pre (num-1, res*num) in
  fib_iter_pre (num, 1)
;;
let max_ascii sent =
  let rec max_ascii_pre (num, max, sent) =
    if num = String.length sent then max
    else if int_of_char sent.[num] > max then max_ascii_pre(num+1, int_of_char sent.[num], sent)
    else max_ascii_pre(num+1, max, sent) in
  char_of_int (max_ascii_pre (0, 0, sent))
;;
    
(* Exercise 4.1 *)
let integral f a b =
  let rec integral_pre f a b n sum=
    let dx = (b -. a) /. (float_of_int n) in
    if n=0 then sum
    else integral_pre f (a+.dx) b (n-1) (sum +. (((f a)+.(f (a+.dx)))*.dx)/.2.) in
  integral_pre f a b (int_of_float 1e6) 0.
;;

(* Exercise 4.4 *)
let uncurry f (a, b) = f a b
;;

(* Exercise 4.5 *)
let rec repeat f n x =
  if n>0 then repeat f (n-1) (f x) else x
;;
let fib_repeat n =
  let (fibn, _) = repeat (fun (x, s) -> (x+s, x)) (n-1) (1, 0) 
  in fibn
;;

(* Exercise 4.7 *)
(*s k k 1 = k 1 (k 1)
          = 1 (because k 1 x = 1 in any x)*)
let k x y = x
;;
let s x y z = x z (y z)
;;
let second x y = k (s k k) x y
;;

(* Exercise 5.3 *)
let hd (x::rest) = x;;
let tl (x::rest) = rest;;
let null = function [] -> true | _ -> false;;
let rec nth n l =
  if n = 1 then hd l else nth (n - 1) (tl l);;
let rec take n l =
  if n = 0 then [] else (hd l) :: (take (n - 1) (tl l));;
let rec drop n l =
  if n = 0 then l else drop (n - 1) (tl l);;

let rec downto0 n =
  if n<0 then []
  else n :: (downto0 (n-1))
;;
let rec roman romlist num =
  if romlist=[] then ""
  else
  let (uni, chara) = hd romlist in
  let times = num / uni in
  let rec charatimes times chara str =
    if times=0 then str
    else chara ^ (charatimes (times-1) chara str) in
  (charatimes times chara "") ^ (roman (tl romlist) (num mod uni))
;;
let rec concat list_list =
  if list_list=[] then []
  else (hd list_list) @ (concat (tl list_list))
;;
let rec zip lista listb =
  match lista with
    [] -> []
  | a :: tla -> match listb with
                  [] -> []
                | b :: tlb -> (a, b) :: (zip tla tlb)
;;
let rec filter f l =
  match l with
    [] -> []
  | x :: t -> if (f x) then x :: (filter f t)
              else filter f t
;;

(* Exercise 5.6 
let rec quicker l sorted =
*)

(* Exercise 6.2 *)
type nat = Zero | OneMoreThan of nat;;
let rec int_of_nat = function
    Zero -> 0
  | OneMoreThan nat2 -> 1 + (int_of_nat nat2)
;;
let rec add m n =
  match m with Zero -> n | OneMoreThan m' -> OneMoreThan (add m' n)
;;
let rec mul m n =
  match m with
    Zero -> Zero
  | OneMoreThan m' -> add (mul m' n) n
;;
let rec monus m n =
  match n with
    Zero -> m
  | OneMoreThan n' -> match m with
                        Zero -> Zero
                      | OneMoreThan m' -> monus m' n'
;;

(* Exercise 6.6 *)
type 'a seq = Cons of 'a * (unit -> 'a seq);;
