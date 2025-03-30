let value: int = 4;;
let square n = n * n;;
let exp n, m = n ^ m;;
square 10 ;; 
square exp 10 2 ;;

match xs with
  | hd :: rst -> Some hd
  | None -> None
  ;;

type state = 
  | alive of int
  | dormant
  ;;

type number = int;

type cell = { 
  length: number;
  state: state;
  position: int * int; }
  ;;


let first: cell = { length = 1, state = alive 10, position: (1, 1)}
