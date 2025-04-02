(* GML Type Definition Test File *)
(* Testing Alias, Record, and Union type definitions *)

(* --- ALIAS Definitions --- *)

(* Simple alias for a primitive *)
type number = int ;;
type text = string ;;
type flag = bool ;;

(* Alias for a function type *)
type int_processor = int -> int ;;
type data_handler = string -> bool -> unit ;;

(* Alias for a nested function type (Requires parenthesis parsing in types) *)
type higher_order = (int -> bool) -> string ;;

(* Alias for a named type (Assumes 'user' type is defined elsewhere or parsing allows it) *)
(* type user_ref = user ;; *) (* Uncomment if named type parsing works *)

(* Alias for a tuple type (Requires '*' parsing in types) *)
(* type point = float * float ;; *)
(* type name_age = string * int ;; *)

(* Alias for a generic application (Requires generic type parsing like 'list' or '<>') *)
(* type int_list = int list ;; *)
(* type string_option = string option ;; *)

(* Parameterized alias (Simple) *)
type 'a identity = 'a ;;

(* Parameterized alias (Multiple Params) *)
type ('k, 'v) pair_alias = 'k * 'v ;; (* Requires '*' parsing in types *)

(* Parameterized alias referencing a generic type *)
type 't sequence = 't list ;; (* Requires list parsing *)


(* --- RECORD Definitions --- *)

(* Empty record *)
type empty_record = {} ;;

(* Simple record with primitive fields *)
type person = {
  name: string;
  age: int;
  is_active: bool
} ;;

(* Record with a trailing semicolon *)
type config = {
  host: string;
  port: int;
  enabled: bool; (* Trailing semicolon *)
} ;;

(* Record with a function field *)
type job = {
  id: int;
  execute: unit -> unit;
} ;;

(* Record with a tuple field (Requires '*' parsing in types) *)
(* type rect = { pos: int * int; size: int * int } ;; *)

(* Record with a field using another named type (Requires named type parsing) *)
(* type employee = { details: person; salary: float } ;; *)

(* Parameterized record *)
type 'a box = {
  content: 'a;
  label: string
} ;;

(* Parameterized record (Multiple params) *)
type ('k, 'v) entry = {
  key: 'k;
  value: 'v;
  timestamp: float
} ;;


(* --- UNION Definitions --- *)

(* Simple enum-style union (no data) *)
type status = Pending | Processing | Complete | Failed ;;

(* Simple enum starting with a pipe *)
type signal = | Stop | Go | Wait ;;

(* Union with variants carrying single primitive data *)
type shape_simple =
  | Circle of float
  | Square of float ;; (* Single type after 'of' is assumed *)

(* Union mixing data-carrying and non-data-carrying variants *)
type response =
  | Success of string
  | NotFound
  | Error of int ;;

(* Union with a variant carrying a function type *)
type event_handler =
  | OnClick of (int -> unit)
  | OnKeyPress of (string -> unit)
  | OnLoad ;;

(* Union with variant carrying tuple (Requires '*' parsing after 'of') *)
(* type result_tuple = Ok of (string * int) | Err of string ;; *)

(* Union with variant carrying named type (Requires named type parsing) *)
(* type data_value = IntData of int | PersonData of person ;; *)

(* Parameterized union (classic Option type) *)
type 'a optional =
  | Nothing
  | Just of 'a ;;

(* Parameterized union (classic Result type) *)
type ('ok_type, 'err_type) outcome =
  | Ok of 'ok_type
  | Error of 'err_type ;;

(* Recursive union type (Requires named type parsing within definition) *)
(* type 'a linked_list = Nil | Cons of 'a * 'a linked_list ;; *) (* Also needs '*' *)


(* --- Edge Cases / Syntax Tests --- *)

type 'a single_param_record = { value: 'a } ;;
type single_field_record = { only: int } ;;
type single_variant_union = OnlyVariant ;;
type single_variant_with_data = OnlyVariantData of bool ;;
type single_variant_with_pipe = | OnlyVariantPipe ;;

(* Alias to a parenthesized type *)
type wrapped_int = (int) ;;

(* Alias to a more complex parenthesized type *)
type wrapped_func = (int -> (string -> bool)) ;;

(* End of test file *)