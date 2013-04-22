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

  (** OCaml bindings for the libfdk-aac. *)

module Encoder =
struct

  exception Invalid_handle
  exception Unsupported_parameter
  exception Invalid_config
  exception Invalid_data
  exception Error of int
  exception End_of_file
  exception Unknown of int
  
  let () =
    Callback.register_exception "fdkaac_exn_invalid_handle" Invalid_handle;
    Callback.register_exception "fdkaac_exn_unsupported_parameter" Unsupported_parameter;
    Callback.register_exception "fdkaac_exn_invalid_config" Invalid_config;
    Callback.register_exception "fdkaac_exn_error" (Error 0);
    Callback.register_exception "fdkaac_exn_encode_eof" End_of_file;
    Callback.register_exception "aacplus_exn_unknown_error" (Unknown 0)
  
  let string_of_exception = 
    function 
       | Invalid_handle -> Some "Handle passed to function call was invalid"
       | Unsupported_parameter -> Some "Parameter not available"
       | Invalid_config -> Some "Configuration not provided"
       | Invalid_data -> Some "Invalid input data"
       | Error 0 -> Some "General initialization error"
       | Error 1 -> Some "AAC library initialization error"
       | Error 2 -> Some "SBR library initialization error"
       | Error 3 -> Some "Transport library initialization error"
       | Error 4 -> Some "Meta data library initialization error"
       | Error 5 -> Some "The encoding process was interrupted by an unexpected error"
       | End_of_file -> Some "End of file reached"
       | _ -> None
  
  (* Dummy registration function for
   * user compiling with ocaml < 3.11.2 *)
  let register_printer _ = ()
  
  (* Now open Printexc,
   * overriding register_printer
   * if present *)
  open Printexc
  
  let () = register_printer string_of_exception
  
  type enc
  
  type t =
    { enc: enc;
      chans: int }
  
  external create : int -> enc = "ocaml_fdkaac_init_enc"
  
  let create chans =
    { enc = create chans;
      chans = chans }
  
  type mpeg2_aac =
    [
       | `AAC_LC
       | `HE_AAC
       | `HE_AAC_v2
    ]
  
  type mpeg4_aac =
    [
       | mpeg2_aac
       | `AAC_LD
       | `AAC_ELD
    ]
  
  type aot =
    [
       | `Mpeg_4 of mpeg4_aac
       | `Mpeg_2 of mpeg2_aac
    ]
  
  let int_of_aot = function
    | `Mpeg_4 `AAC_LC -> 2
    | `Mpeg_4 `HE_AAC -> 5
    | `Mpeg_4 `HE_AAC_v2 -> 29
    | `Mpeg_4 `AAC_LD -> 23
    | `Mpeg_4 `AAC_ELD -> 39
    | `Mpeg_2 `AAC_LC -> 129
    | `Mpeg_2 `HE_AAC -> 132
    | `Mpeg_2 `HE_AAC_v2 -> 156
  
  let aot_of_int = function
    | 2 -> `Mpeg_4 `AAC_LC
    | 5 -> `Mpeg_4 `HE_AAC
    | 29 -> `Mpeg_4 `HE_AAC_v2
    | 23 -> `Mpeg_4 `AAC_LD
    | 39 -> `Mpeg_4 `AAC_ELD
    | 129 -> `Mpeg_2 `AAC_LC
    | 132 -> `Mpeg_2 `HE_AAC
    | 156 -> `Mpeg_2 `HE_AAC_v2
    | _ -> raise Unsupported_parameter 
  
  type bitrate_mode =
    [
       | `Constant
       | `Full_bitreservoir
    ]
  
  let int_of_bitrate_mode = function
    | `Constant -> 0
    | `Full_bitreservoir -> 8
  
  let bitrate_mode_of_int = function
    | 0 -> `Constant
    | 8 -> `Full_bitreservoir
    | _ -> raise Unsupported_parameter
  
  type transmux =
    [
       | `Raw
       | `Adif
       | `Adts
       | `Latm
       | `Latm_out_of_band
       | `Loas
    ]
  
  let int_of_transmux = function
    | `Raw -> 0
    | `Adif -> 1
    | `Adts -> 2
    | `Latm -> 6
    | `Latm_out_of_band -> 7
    | `Loas -> 10
  
  let transmux_of_int = function
    | 0 -> `Raw
    | 1 -> `Adif
    | 2 -> `Adts
    | 6 -> `Latm 
    | 7 -> `Latm_out_of_band
    | 10 -> `Loas
    | _ -> raise Unsupported_parameter
  
  type param = 
    [
      | `Aot of aot 
      | `Bitrate of int
      | `Bitrate_mode of bitrate_mode
      | `Samplerate of int
      | `Sbr_mode of bool
      | `Granule_length of int
      | `Afterburner of bool
      | `Bandwidth of bool
      | `Transmux of transmux 
    ]
  
  type param_name =
    [
      | `Aot
      | `Bitrate
      | `Bitrate_mode
      | `Samplerate
      | `Sbr_mode
      | `Granule_length
      | `Afterburner
      | `Bandwidth
      | `Transmux
    ]
  
  let extract_param = function
    | `Aot x -> `Aot, (int_of_aot x)
    | `Bitrate x -> `Bitrate, x
    | `Bitrate_mode x -> `Bitrate_mode, (int_of_bitrate_mode x)
    | `Samplerate x -> `Samplerate, x
    | `Sbr_mode x -> `Sbr_mode, (if x then 1 else 0)
    | `Granule_length x -> `Granule_length, x
    | `Afterburner x -> `Afterburner, (if x then 1 else 0)
    | `Bandwidth x -> `Bandwidth, (if x then 1 else 0)
    | `Transmux x -> `Transmux, (int_of_transmux x)
  
  external set : enc -> param_name -> int -> unit = "ocaml_fdkaac_set_param"
  
  let set enc param =
    let p, v = extract_param param in
    set enc.enc p v
  
  let pack_param = function
    | `Aot, x -> `Aot (aot_of_int x)
    | `Bitrate, x -> `Bitrate x
    | `Bitrate_mode, x -> `Bitrate_mode (bitrate_mode_of_int x)
    | `Samplerate, x -> `Samplerate x
    | `Sbr_mode, x -> `Sbr_mode (x == 1)
    | `Granule_length, x -> `Granule_length x
    | `Afterburner, x -> `Afterburner (x == 1)
    | `Bandwidth, x -> `Bandwidth (x == 1)
    | `Transmux, x -> `Transmux (transmux_of_int x)
  
  external get : enc -> param_name -> int = "ocaml_fdkaac_set_param"
  
  let get enc param =
    let x = get enc.enc param in
    pack_param (param, x)
    
  external encode : enc -> string -> int -> int -> string = "ocaml_fdkaac_encode"
  
  let encode enc buf ofs len =
    if String.length buf - ofs < len then
      raise Invalid_data;
    encode enc.enc buf ofs len
  
  external flush : enc -> string = "ocaml_fdkaac_flush"
  
  let flush enc =
    let buf = Buffer.create 1024 in
    try
      while true do
        Buffer.add_string buf (flush enc.enc)
      done;
      assert false
    with
      | End_of_file -> Buffer.contents buf 
end
