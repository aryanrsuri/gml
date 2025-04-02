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


