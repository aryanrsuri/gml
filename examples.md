let x: int = 4;;
-- int
let x n = 4 * n;;
-- int -> int
let exp n, m = n ^ m;;
-- int -> int -> int

match xs with
  | hd :: rst -> Some hd
  | None -> None
  ;;

type state = 
  | alive of int
  | dormant
  ;;
-- alive | dormant

type cell = { 
  length: int;
  state: state;
  position: tuple * (int * int); }
  ;;
-- { length * state * position }
