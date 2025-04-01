(* Simple value bindings *)
let x = 5 ;;
let pi : float = 3.14 ;;
let greeting = "Hello world!" ;;
let is_active : bool = true ;;
let nada : unit = () ;;
let calculated_val = (x * 2) + 3 ;;

(* Function definition bindings *)

(* Simple function, inferred types *)
let square n = n * n ;;

(* Function with typed parameter *)
let increment (x : int) = x + 1 ;;

(* Function with explicit return type *)
let get_offset : int = 10 ;; (* Simple let, return type specified *)
let add_offset y : int = y + get_offset ;;

(* Function with multiple params, one typed *)
let scale_add (factor : int) val = (factor * val) + increment val ;;

(* Function with multiple typed params and return type *)
let subtract (a : int) (b : int) : int = a - b ;;

(* Using function application *)
let result = add_offset 5 ;;              (* Expected: 15 *)
let scaled = scale_add 3 result ;;        (* Expected: (3 * 15) + 16 = 45 + 16 = 61 *)
let final_val = subtract scaled (square 4); ;; (* Expected: 61 - 16 = 45 *)

(* If-Then-Else Expression *)
let check_val = if final_val > 50 then "Big" else "Small" ;;

(* Function definition using fun keyword *)
let double = fun x -> x * 2 ;;
let apply_twice (f : int -> int) x = f (f x) ;;
let quadrupled = apply_twice double 3 ;; (* Expected: double(double(3)) = double(6) = 12 *)

(* List Literal *)
let my_list = [1; 2; 3; 4] ;;
let empty_list : int = [] ;; (* Type anno needed if list type inference not implemented *)

(* Tuple Literal - CAUTION: Parsing logic might need review *)
(* let my_tuple = (1; "two"; false) ;; *)
let simple_pair = (10; 20) ;;

(* Prefix Expressions *)
let negated = -final_val ;;
let is_not_active = !is_active ;;

(* Infix Expressions involving precedence *)
let complex_calc = 2 + 3 * 4 ;; (* Expected: 14 *)
let comparison = complex_calc > 10 ;; (* Expected: true *)

(* Sequence Expression (using ;) *)
(* let side_effect = (print_string "Calculating..."; 100) ;; *) (* Requires print_string + sequence parsing in tuples/groups *)
let sequence_test = (1; 2; 3) ;; (* Likely parses as Tuple based on current logic *)

(* Top-level expression *)
100 + 200 ;;

(* End of example *)
