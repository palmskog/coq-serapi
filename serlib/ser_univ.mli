(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2016     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

(************************************************************************)
(* Coq serialization API/Plugin                                         *)
(* Copyright 2016 MINES ParisTech                                       *)
(************************************************************************)
(* Status: Very Experimental                                            *)
(************************************************************************)

open Sexplib

module Level : SerType.S with type t = Univ.Level.t

type universe_level = Level.t
val universe_level_of_sexp : Sexp.t -> universe_level
val sexp_of_universe_level : universe_level -> Sexp.t

module Universe : SerType.S with type t = Univ.Universe.t

type universe = Universe.t
val universe_of_sexp : Sexp.t -> universe
val sexp_of_universe : universe -> Sexp.t

module Instance : SerType.S with type t = Univ.Instance.t

type constraint_type = Univ.constraint_type

val constraint_type_of_sexp : Sexp.t -> constraint_type
val sexp_of_constraint_type : constraint_type -> Sexp.t

type univ_constraint = Univ.univ_constraint

val univ_constraint_of_sexp : Sexp.t -> univ_constraint
val sexp_of_univ_constraint : univ_constraint -> Sexp.t

type universe_instance = Instance.t

val universe_instance_of_sexp : Sexp.t -> universe_instance
val sexp_of_universe_instance : universe_instance -> Sexp.t

module Constraint : SerType.S with type t = Univ.Constraint.t
module UContext : SerType.S with type t = Univ.UContext.t

type universe_context = UContext.t
val universe_context_of_sexp : Sexp.t -> universe_context
val sexp_of_universe_context : universe_context -> Sexp.t

module AUContext : SerType.S with type t = Univ.AUContext.t

type abstract_universe_context = AUContext.t
val abstract_universe_context_of_sexp : Sexp.t -> abstract_universe_context
val sexp_of_abstract_universe_context : abstract_universe_context -> Sexp.t

module CumulativityInfo : SerType.S with type t = Univ.CumulativityInfo.t

type cumulativity_info = CumulativityInfo.t
val cumulativity_info_of_sexp : Sexp.t -> cumulativity_info
val sexp_of_cumulativity_info : cumulativity_info -> Sexp.t

module ACumulativityInfo : SerType.S with type t = Univ.ACumulativityInfo.t

type abstract_cumulativity_info = ACumulativityInfo.t
val abstract_cumulativity_info_of_sexp : Sexp.t -> abstract_cumulativity_info
val sexp_of_abstract_cumulativity_info : abstract_cumulativity_info -> Sexp.t

module ContextSet : SerType.S with type t = Univ.ContextSet.t

type universe_context_set = ContextSet.t
val universe_context_set_of_sexp : Sexp.t -> universe_context_set
val sexp_of_universe_context_set : universe_context_set -> Sexp.t

(** A value in a universe context (resp. context set). *)
type 'a in_universe_context = 'a * universe_context
val in_universe_context_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a in_universe_context
val sexp_of_in_universe_context : ('a -> Sexp.t) -> 'a in_universe_context -> Sexp.t

type 'a in_universe_context_set = 'a * universe_context_set
val in_universe_context_set_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a in_universe_context_set
val sexp_of_in_universe_context_set : ('a -> Sexp.t) -> 'a in_universe_context_set -> Sexp.t

type 'a puniverses = 'a * universe_instance

val puniverses_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a puniverses
val sexp_of_puniverses : ('a -> Sexp.t) -> 'a puniverses -> Sexp.t

type explanation = Univ.explanation

val explanation_of_sexp : Sexp.t -> explanation
val sexp_of_explanation : explanation -> Sexp.t

type univ_inconsistency = Univ.univ_inconsistency

val univ_inconsistency_of_sexp : Sexp.t -> univ_inconsistency
val sexp_of_univ_inconsistency : univ_inconsistency -> Sexp.t

