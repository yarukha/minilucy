(* Arbres de syntaxe abstraite *)

open Asttypes

type ident = string

type typ_decl = 
  {typ_name : ident ; typ_ty : ty ; typ_loc : location }

type const_decl = 
  {const_name : ident; const_value :const ; const_loc : location}


type p_expr =
  { pexpr_desc: p_expr_desc;
    pexpr_loc: location; }

and p_expr_desc =
  | PE_const of const
  | PE_ident of ident
  | PE_op of op * p_expr list
  | PE_app of ident * p_expr list
  | PE_arrow of p_expr * p_expr
  | PE_pre of p_expr
  | PE_tuple of p_expr list
  | PE_merge of p_expr list
  | PE_when of p_expr * p_expr
  | PE_reset of p_expr 

type p_patt =
  { ppatt_desc: p_patt_desc;
    ppatt_loc: location; }

and p_patt_desc =
  | PP_ident of ident
  | PP_tuple of ident list

type p_equation =
    { peq_patt: p_patt;
      peq_expr: p_expr; }

type p_node =
    { pn_name: ident;
      pn_input: (ident * ty) list;
      pn_output: (ident * ty) list;
      pn_local: (ident * ty) list;
      pn_equs: p_equation list;
      pn_loc: location; }


type p_file = { const_decl : const_decl list;nodes:p_node list}
