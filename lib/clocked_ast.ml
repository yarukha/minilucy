open Asttypes 



type typed_var = {id:Ident.t;ty:ty;clock:clock}

and clock = 
  |Base 
  |Prod of clock list 
  |Ck of (construct* c_expr)

and c_expr =
    { cexpr_desc: c_expr_desc;
      cexpr_type:  ty;
      cexpr_loc: location; 
      cexpr_clock:clock}

and c_expr_desc =
  | CE_const of const
  | CE_ident of Ident.t
  | CE_op of op * c_expr list
  | CE_app of Ident.t * c_expr list
  | CE_prim of Ident.t * c_expr list
  | CE_arrow of c_expr * c_expr
  | CE_pre of c_expr
  | CE_tuple of c_expr list  
  | CE_merge of c_expr list
  | CE_when of c_expr * c_expr

type c_patt =
    { cpatt_desc: Ident.t list;
      cpatt_type: ty;
      cpatt_loc: location; }

type c_equation =
    { ceq_patt: c_patt;
      ceq_expr: c_expr; }

type c_node =
    { cn_name: Ident.t;
      cn_input: typed_var list;
      cn_output: typed_var list;
      cn_local: typed_var list;
      cn_equs: c_equation list;
      cn_loc: location; }
  


type c_file = c_node list