(*Ahmed Amine Taleb Bahmed*)
(*130201120*)


(* CSE 130: Programming Assignment 1
 * misc.ml
 *)

(* sumList : int list -> int 
 * (sumList l) is the sum of the integer elements of the list of l
 * e.g. (sumList [1; 2; 3; 4]) is 10
 *      (sumList [1; -2; 3; 5] is 7
 *)

let rec sumList l = 
match l with
|[] -> 0
|a::z -> a + (sumList z)
;;


(* digitsOfInt : int -> int list 
 * (digitsOfInt n) is the list of positive digits of n in the order in which they appear
 * in n
 * e.g. (digitsOfInt 31243) is [3,1,2,4,3]
 *      (digitsOfInt (352663) is [3; 5; 2; 6; 6; 3]
 *)

let rec digitsOfInt n =
if n<=0 then []                             
else digitsOfInt (n/10) @ (n mod 10) :: []
;;

(* digits : int -> int list
 * (digits n) is the list of digits of n in the order in which they appear
 * in n
 * e.g. (digits 31243) is [3,1,2,4,3]
 *      (digits (-23422) is [2,3,4,2,2]
 *)
 
let rec digits n = digitsOfInt (abs n)


(* From http://mathworld.wolfram.com/AdditivePersistence.html
 * Consider the process of taking a number, adding its digits, 
 * then adding the digits of the number derived from it, etc., 
 * until the remaining number has only one digit. 
 * The number of additions required to obtain a single digit from a number n 
 * is called the additive persistence of n, and the digit obtained is called 
 * the digital root of n.
 * For example, the sequence obtained from the starting number 9876 is (9876, 30, 3), so 
 * 9876 has an additive persistence of 2 and a digital root of 3.
 *)


(* ***** PROVIDE COMMENT BLOCKS FOR THE FOLLOWING FUNCTIONS ***** *)

(* additivePersistence: int -> int
 * (additivePersistence n ) takes in a positive n number and give
 * the number of additions required to obtain a single digit from a
 * number n
 * e.g. (additivePersistence 9876) is 2
 *)
let rec additivePersistence n = 
if (n<10) then 0
else (1+ additivePersistence (sumList (digitsOfInt n)))
;;


(* digitalRoot: int -> int
 * (digitalRoot n ) takes in a positive n number and keep adding all
 * the digits of n until n is a single digit.
 * e.g. (digitalRoot 92) is 2
 *     (additivePErsistence 9876) is 3
 *)
let rec digitalRoot n = 
if (n<10) then n 
else digitalRoot (sumList (digitsOfInt n))
;;

(* listReverse: 'a list -> 'a list
 * (listReverse l) reverses all the elements of an inputed list
 * e.g. (listReverse [1;2;3;4]) is [4;3;2;1]
 *       (listReverse ["a","b","d","d"]) is ["d","c","b", "a"]
*)
let rec listReverse l = 
match l with
|[] -> [] 
|a::z -> (listReverse z)@[a]
;;

(* explode : string -> char list 
 * (explode s) is the list of characters in the string s in the order in 
 *   which they appear
 * e.g.  (explode "Hello") is ['H';'e';'l';'l';'o']
 *)
let rec explode s = 
  let rec _exp i = 
    if i >= String.length s then [] else (s.[i])::(_exp (i+1)) in
  _exp 0

(* palindrome: string -> bool
 * (palindrome w) checks whether a given string is a palindrome.
 * The function will return true if the string is indeed a palindrome;
 * otherwise, return false.
 * e.g. (palindrome "malayalam") is true
 *      (palindrome "myxomatosis") is false	
 *)
let rec palindrome w = 
if (explode w = listReverse(explode w)) then true
else false
;;

