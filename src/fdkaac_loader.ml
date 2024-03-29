(*
 * Copyright 2013 Savonet team
 *
 * This file is part of ocaml-fdkaac.
 *
 * ocaml-fdkaac is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * ocaml-fdkaac is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with ocaml-fdkaac; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(* $Id$ *)

(**
  * Bindings to the fdkaac AAC library.
  * Dynamic loading module.
  *
  * @author Romain Beauxis
  *)

open Fdkaac_dynlink

let () = handler.fdkaac_module <- Some (module Fdkaac : Fdkaac_t)
