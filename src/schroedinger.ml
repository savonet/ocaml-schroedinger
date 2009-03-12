(*
 * Copyright 2003-2009 Savonet team
 *
 * This file is part of Ocaml-schroedinger.
 *
 * Ocaml-schroedinger is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Ocaml-schroedinger is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Ocaml-schroedinger; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

  (** OCaml API for the schroedinger video encoding/decoding library
      implementing the Dirac video codec. *)

external init : unit -> unit = "caml_schroedinger_init"

let () = init ()

type plane = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

(* Only planar formats for now.. *)
type format = 
   | Yuv_422_p    (** Planar YCbCr 4:2:2. Each component is an uint8_t *)
   | Yuv_444_p    (** Planar YCbCr 4:4:4. Each component is an uint8_t *)
   | Yuv_420_p   (** Planar YCbCr 4:2:0. Each component is an uint8_t,
                   * luma and chroma values are full range (0x00 .. 0xff) *)

external int_of_define : string -> int = "ocaml_schroedinger_int_of_define"

let int_of_format f = 
  match f with
   | Yuv_422_p -> int_of_define "SCHRO_FRAME_FORMAT_U8_422" 
   | Yuv_444_p -> int_of_define "SCHRO_FRAME_FORMAT_U8_444"
   | Yuv_420_p -> int_of_define "SCHRO_FRAME_FORMAT_U8_420"

type frame = 
  { 
    planes : (plane*int) array;
    width  : int;
    height : int;
    format : format
  }

type internal_frame = 
  {
    int_planes : (plane*int) array;
    int_width  : int;
    int_height : int;
    int_format : int
  }

let internal_frame_of_frame f = 
  if (Array.length f.planes <> 3) then
    failwith "Frame does not have 3 planes. \
              Only planar formats are supported for now..";
  { int_planes = f.planes;
    int_width  = f.width;
    int_height = f.height;
    int_format = int_of_format f.format
  }
  

module Encoder = 
struct

  type t

  external create : unit -> t = "ocaml_schroedinger_create_enc"

  external end_of_stream : t -> unit = "ocaml_schroedinger_enc_eos"

  external encode_frame : t -> internal_frame -> Ogg.Stream.t -> unit = "ocaml_schroedinger_encode_frame" 

  let encode_frame t f = encode_frame t (internal_frame_of_frame f)

end

