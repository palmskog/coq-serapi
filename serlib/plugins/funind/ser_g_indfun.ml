open Recdef_plugin
open Sexplib.Conv

module CAst       = Ser_cAst
module Constrexpr = Ser_constrexpr
module Tactypes   = Ser_tactypes
module Genintern  = Ser_genintern
module EConstr    = Ser_eConstr
module Tacexpr    = Ser_tacexpr

module A1 = struct

type h1 = Constrexpr.constr_expr Tactypes.intro_pattern_expr CAst.t option
[@@deriving sexp]
type h2 = Genintern.glob_constr_and_expr Tactypes.intro_pattern_expr CAst.t option
[@@deriving sexp]
type h3 = Tacexpr.intro_pattern option
[@@deriving sexp]

end

let ser_wit_with_names =
  let open A1 in
  Ser_genarg.{
    raw_ser = sexp_of_h1;
    raw_des = h1_of_sexp;

    glb_ser = sexp_of_h2;
    glb_des = h2_of_sexp;

    top_ser = sexp_of_h3;
    top_des = h3_of_sexp;
  }

module A2 = struct
type h1 = Constrexpr.constr_expr Tactypes.with_bindings option
[@@deriving sexp]
type h2 = Genintern.glob_constr_and_expr Tactypes.with_bindings option
[@@deriving sexp]
type h3 = EConstr.t Tactypes.with_bindings Ser_tacexpr.delayed_open option
[@@deriving sexp]
end

let ser_wit_fun_ind_using :
  (Constrexpr.constr_expr Tactypes.with_bindings option,
   Genintern.glob_constr_and_expr Tactypes.with_bindings option,
   EConstr.t Tactypes.with_bindings Ltac_plugin.Tacexpr.delayed_open option)
    Ser_genarg.gen_ser =
  let open A2 in
  Ser_genarg.{
    raw_ser = sexp_of_h1;
    raw_des = h1_of_sexp;

    glb_ser = sexp_of_h2;
    glb_des = h2_of_sexp;

    top_ser = sexp_of_h3;
    top_des = h3_of_sexp;
  }

let ser_wit_fun_scheme_arg :
  (Names.variable * Libnames.qualid * Sorts.family, unit, unit)
    Ser_genarg.gen_ser =
  Ser_genarg.{
    raw_ser = sexp_of_triple Ser_names.sexp_of_variable Ser_libnames.sexp_of_qualid Ser_sorts.sexp_of_family;
    raw_des = triple_of_sexp Ser_names.variable_of_sexp Ser_libnames.qualid_of_sexp Ser_sorts.family_of_sexp;

    glb_ser = sexp_of_unit;
    glb_des = unit_of_sexp;

    top_ser = sexp_of_unit;
    top_des = unit_of_sexp;
  }

let register () =
  Ser_genarg.register_genser G_indfun.wit_with_names ser_wit_with_names;
  Ser_genarg.register_genser G_indfun.wit_fun_ind_using ser_wit_fun_ind_using;
  Ser_genarg.register_genser G_indfun.wit_fun_scheme_arg ser_wit_fun_scheme_arg;
  ()