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

type plane = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

(* Only planar formats for now.. *)
type format =
   | Yuv_422_p    (** Planar YCbCr 4:2:2. Each component is an uint8_t *)
   | Yuv_444_p    (** Planar YCbCr 4:4:4. Each component is an uint8_t *)
   | Yuv_420_p   (** Planar YCbCr 4:2:0. Each component is an uint8_t,
                   * luma and chroma values are full range (0x00 .. 0xff) *)

type frame =
  {
    (** The integer is the stride for the plane. *)
    planes : (plane*int) array;
    width  : int;
    height : int;
    format : format
  }

module Encoder :
sig

  type t

  val create : unit -> t

  val encode_frame : t -> frame -> Ogg.Stream.t -> unit

  val end_of_stream : t -> unit

end

