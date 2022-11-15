type location = Lexing.position * Lexing.position

type base_ty =
  | Tbool
  | Tint
  | Treal

type construct = C of string

type ty =  base_ty list

type const =
  | Cbool of bool
  | Cint of int
  | Creal of float

type op =
  | Op_eq | Op_neq | Op_lt | Op_le | Op_gt | Op_ge
  | Op_add | Op_sub | Op_mul | Op_div | Op_mod
  | Op_add_f | Op_sub_f | Op_mul_f | Op_div_f
  | Op_not
  | Op_and | Op_or | Op_impl
  | Op_if



let pp_base_ty out b_ty = 
  Format.fprintf out "%s" (match b_ty with |Tbool ->"bool"|Tint->"int"|Treal->"real")

let pp_construct out =
  function 
  |C(s) -> Format.fprintf out "C %s" s

let rec pp_ty out l= 
  match l with 
  |[]->failwith "empty type"
  |[x]->Format.fprintf out "%a" pp_base_ty x
  |x::q->Format.fprintf out "%a x %a" pp_base_ty x pp_ty q


let pp_const out = 
  function 
  |Cbool b -> Format.fprintf out "%b" b
  |Cint i ->  Format.fprintf out "%i" i
  |Creal r -> Format.fprintf out "%f" r

let pp_op out op = 
  Format.fprintf out "%s" 
  (match op with 
  |Op_eq->"="|Op_neq->"!="|Op_lt->"<"|Op_le->"<="|Op_gt->">"|Op_ge->">="
  |Op_add->"+"
  |_->failwith "unknown operator"
  ) 