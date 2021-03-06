(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *   INRIA, CNRS and contributors - Copyright 1999-2018       *)
(* <O___,, *       (see CREDITS file for the list of authors)           *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

(************************************************************************)
(* Coq serialization API/Plugin                                         *)
(* Copyright 2016-2018 MINES ParisTech -- Dual License LGPL 2.1 / GPL3+ *)
(************************************************************************)
(* Status: Very Experimental                                            *)
(************************************************************************)

(**********************************************************************)
(* Misctypes.mli                                                      *)
(**********************************************************************)

open Sexplib

type lident = Misctypes.lident
val lident_of_sexp : Sexp.t -> lident
val sexp_of_lident : lident -> Sexp.t

type lname = Misctypes.lname
val lname_of_sexp : Sexp.t -> lname
val sexp_of_lname : lname -> Sexp.t

type lstring = Misctypes.lstring
val lstring_of_sexp : Sexp.t -> lstring
val sexp_of_lstring : lstring -> Sexp.t

type patvar = Misctypes.patvar
val patvar_of_sexp : Sexp.t -> patvar
val sexp_of_patvar : patvar -> Sexp.t

type 'constr intro_pattern_expr = 'constr Misctypes.intro_pattern_expr
and intro_pattern_naming_expr   = Misctypes.intro_pattern_naming_expr
and 'constr intro_pattern_action_expr = 'constr Misctypes.intro_pattern_action_expr
and 'constr or_and_intro_pattern_expr = 'constr Misctypes.or_and_intro_pattern_expr

val intro_pattern_expr_of_sexp : (Sexp.t -> 'constr) -> Sexp.t -> 'constr intro_pattern_expr
val sexp_of_intro_pattern_expr : ('constr -> Sexp.t) -> 'constr intro_pattern_expr -> Sexp.t

val intro_pattern_naming_expr_of_sexp : Sexp.t -> intro_pattern_naming_expr
val sexp_of_intro_pattern_naming_expr : intro_pattern_naming_expr -> Sexp.t

val intro_pattern_action_expr_of_sexp : (Sexp.t -> 'constr) -> Sexp.t -> 'constr intro_pattern_action_expr
val sexp_of_intro_pattern_action_expr : ('constr -> Sexp.t) -> 'constr intro_pattern_action_expr -> Sexp.t

val or_and_intro_pattern_expr_of_sexp : (Sexp.t -> 'constr) -> Sexp.t -> 'constr or_and_intro_pattern_expr
val sexp_of_or_and_intro_pattern_expr : ('constr -> Sexp.t) -> 'constr or_and_intro_pattern_expr -> Sexp.t

type 'id move_location = 'id Misctypes.move_location

val move_location_of_sexp : (Sexp.t -> 'id) -> Sexp.t -> 'id move_location
val sexp_of_move_location : ('id -> Sexp.t) -> 'id move_location -> Sexp.t

type 'a glob_sort_gen = 'a Misctypes.glob_sort_gen

val glob_sort_gen_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a glob_sort_gen
val sexp_of_glob_sort_gen : ('a -> Sexp.t) -> 'a glob_sort_gen -> Sexp.t

type sort_info = Misctypes.sort_info
val sort_info_of_sexp : Sexp.t -> sort_info
val sexp_of_sort_info : sort_info -> Sexp.t

type level_info = Misctypes.level_info
val level_info_of_sexp : Sexp.t -> level_info
val sexp_of_level_info : level_info -> Sexp.t

type glob_sort = Misctypes.glob_sort
val glob_sort_of_sexp : Sexp.t -> glob_sort
val sexp_of_glob_sort : glob_sort -> Sexp.t

type glob_level = Misctypes.glob_level
val glob_level_of_sexp : Sexp.t -> glob_level
val sexp_of_glob_level : glob_level -> Sexp.t

type 'a cast_type = 'a Misctypes.cast_type

val cast_type_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a cast_type
val sexp_of_cast_type : ('a -> Sexp.t) -> 'a cast_type -> Sexp.t

type glob_constraint = Misctypes.glob_constraint

val glob_constraint_of_sexp : Sexp.t -> glob_constraint
val sexp_of_glob_constraint : glob_constraint -> Sexp.t

type existential_key = Misctypes.existential_key

val existential_key_of_sexp : Sexp.t -> existential_key
val sexp_of_existential_key : existential_key -> Sexp.t

type quantified_hypothesis = Misctypes.quantified_hypothesis

val quantified_hypothesis_of_sexp : Sexp.t -> quantified_hypothesis
val sexp_of_quantified_hypothesis : quantified_hypothesis -> Sexp.t

type 'a explicit_bindings = 'a Misctypes.explicit_bindings

val explicit_bindings_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a explicit_bindings
val sexp_of_explicit_bindings : ('a -> Sexp.t) -> 'a explicit_bindings -> Sexp.t

type 'a bindings = 'a Misctypes.bindings

val bindings_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a bindings
val sexp_of_bindings : ('a -> Sexp.t) -> 'a bindings -> Sexp.t

type 'a with_bindings = 'a Misctypes.with_bindings

val with_bindings_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a with_bindings
val sexp_of_with_bindings : ('a -> Sexp.t) -> 'a with_bindings -> Sexp.t

type 'a or_var = 'a Misctypes.or_var

val or_var_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a or_var
val sexp_of_or_var : ('a -> Sexp.t) -> 'a or_var -> Sexp.t

type 'a and_short_name = 'a Misctypes.and_short_name

val and_short_name_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a and_short_name
val sexp_of_and_short_name : ('a -> Sexp.t) -> 'a and_short_name -> Sexp.t

type 'a or_by_notation = 'a Misctypes.or_by_notation

val or_by_notation_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a or_by_notation
val sexp_of_or_by_notation : ('a -> Sexp.t) -> 'a or_by_notation -> Sexp.t

type module_kind = Misctypes.module_kind
val module_kind_of_sexp : Sexp.t -> module_kind
val sexp_of_module_kind : module_kind -> Sexp.t

type clear_flag = Misctypes.clear_flag
val clear_flag_of_sexp : Sexp.t -> clear_flag
val sexp_of_clear_flag : clear_flag -> Sexp.t

type multi = Misctypes.multi
val multi_of_sexp : Sexp.t -> multi
val sexp_of_multi : multi -> Sexp.t

type 'a core_destruction_arg = 'a Misctypes.core_destruction_arg
val core_destruction_arg_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a core_destruction_arg
val sexp_of_core_destruction_arg : ('a -> Sexp.t) -> 'a core_destruction_arg -> Sexp.t

type 'a destruction_arg = 'a Misctypes.destruction_arg
val destruction_arg_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a destruction_arg
val sexp_of_destruction_arg : ('a -> Sexp.t) -> 'a destruction_arg -> Sexp.t

type inversion_kind = Misctypes.inversion_kind
val inversion_kind_of_sexp : Sexp.t -> inversion_kind
val sexp_of_inversion_kind : inversion_kind -> Sexp.t

type ('a,'b) gen_universe_decl = ('a,'b) Misctypes.gen_universe_decl
val gen_universe_decl_of_sexp : (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> Sexp.t -> ('a,'b) gen_universe_decl
val sexp_of_gen_universe_decl : ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('a,'b) gen_universe_decl -> Sexp.t
