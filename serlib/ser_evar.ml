(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2016     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

(************************************************************************)
(* Coq serialization API/Plugin                                         *)
(* Copyright 2016-2017 MINES ParisTech                                  *)
(* Written by: Emilio J. Gallego Arias                                  *)
(************************************************************************)
(* Status: Very Experimental                                            *)
(************************************************************************)

open Sexplib.Std

(* Private *)
module Self = struct
type t = [%import: Evar.t]

type _evar                    = Ser_Evar of int [@@deriving sexp]
let _evar_put  evar           = Ser_Evar (Evar.repr evar)
let _evar_get (Ser_Evar evar) = Evar.unsafe_of_int evar

let t_of_sexp sexp = _evar_get (_evar_of_sexp sexp)
let sexp_of_t evar = sexp_of__evar (_evar_put evar)
end

include Self

module Set = Ser_cSet.Make(Evar.Set)(Self)
