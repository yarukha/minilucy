open Asttypes
open Untyped_ast
open Format

let pp_ident out = 
  fprintf out "%s"


let pp_const_decl out c = 
  fprintf out "const %a = %a;" pp_ident c.const_name pp_const c.const_value



let rec pp_expr out e = 
  pp_expr_desc out e.pexpr_desc
and pp_expr_desc out e = 
  match e with 
  |PE_const c -> pp_const out c
  |PE_ident i -> pp_ident out i
  |PE_op (op,l)-> begin 
    match l with 
    |[]->failwith "empty operand list"
    |[x]->fprintf out "%a %a" pp_op op pp_expr x
    |x::q->fprintf out "%a %a" pp_expr x pp_expr_desc (PE_op (op,q))
    end
  |PE_app(id,l)->fprintf out "%a %a" pp_ident id pp_expr_desc (PE_tuple l)
  |PE_arrow(e1,e2)->fprintf out "%a -> %a" pp_expr e1 pp_expr e2
  |PE_pre e -> fprintf out "pre %a" pp_expr e
  |PE_when (e1,e2) -> fprintf out "%a when %a" pp_expr e1 pp_expr e2
  |PE_tuple l->begin
    match l with 
    |[]-> fprintf out "()"
    |x::q->fprintf out "(%s)" @@ List.fold_left (fun s e -> asprintf "%s,%a" s pp_expr e) (asprintf "%a" pp_expr x) q
  end
  |PE_merge l -> 
    fprintf out "merge ";
    List.iter (fun x -> fprintf out "(%a)" pp_expr x) l


let rec pp_patt out pat = 
  fprintf out "%a" pp_patt_desc pat.ppatt_desc
and pp_patt_desc out = 
  function 
  |PP_ident id -> fprintf out "%s" id
  |PP_tuple l -> begin 
    match l with 
    |[]->fprintf out "()"
    |x::q-> fprintf out "(%s)" @@ List.fold_left (fun s p-> asprintf "%s,%s" s p) x q
  end 

let pp_equation out eq = 
  fprintf out "%a = %a;" pp_patt eq.peq_patt pp_expr eq.peq_expr



let pp_node out n = 
  let rec pp_var_decl out' :(ident*ty) list -> unit= 
    function |[]->()|[id,ty]->fprintf out' "%a: %a" pp_ident id pp_ty ty
    |(id,ty)::q->fprintf out' "%a: %a, %a" pp_ident id pp_ty ty pp_var_decl q
  in
  let rec pp_eq_list out' = 
    function |[]->()|x::q->fprintf out' "\t%a;\n%a" pp_equation x pp_eq_list q
  in 
  fprintf out "node %a (%a) returns (%a);\nvar %a;\n" pp_ident n.pn_name pp_var_decl n.pn_input pp_var_decl n.pn_output pp_var_decl n.pn_local;
  fprintf out "let\n%atel\n" pp_eq_list n.pn_equs

let pp_file out f = 
  List.iter (fprintf out "%a\n" pp_const_decl) f.const_decl;
  List.iter (fprintf out "%a\n" pp_node) f.nodes