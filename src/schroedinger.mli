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

type video_type = 
  | CUSTOM
  | QSIF
  | QCIF
  | SIF
  | CIF
  | SIF_4
  | CIF_4
  | SD480I_60
  | SD576I_50
  | HD720P_60
  | HD720P_50
  | HD1080I_60
  | HD1080I_50
  | HD1080P_60
  | HD1080P_50
  | DC2K_24
  | DC4K_24

type chroma = 
  | Chroma_422
  | Chroma_444
  | Chroma_420

type colour_primaries = 
  | HDTV
  | SDTV_525
  | SDTV_625
  | CINEMA

type colour_matrix = 
  | HDTV
  | SDTV
  | REVERSIBLE

type transfer_function = 
  | TV_GAMMA
  | EXTENDED_GAMMUT
  | LINEAR
  | DCI_GAMMA

type video_format = 
 {
  video_type : video_type;
  width : int;
  height : int;
  chroma_format : chroma; 

  interlaced : bool;
  top_field_first : bool;

  frame_rate_numerator : int;
  frame_rate_denominator : int;
  aspect_ratio_numerator : int;
  aspect_ratio_denominator : int;

  clean_width : int;
  clean_height : int;
  left_offset : int;
  top_offset : int;

  luma_offset : int;
  luma_excursion : int;
  chroma_offset : int;
  chroma_excursion : int;

  colour_primaries: colour_primaries;
  colour_matrix : colour_matrix;
  transfer_function : transfer_function;

  interlaced_coding : bool;
 }

val get_default_video_format : video_type -> video_format

type frame =
  {
    (** The integer is the stride for the plane. *)
    planes : (plane*int) array;
    frame_width  : int;
    frame_height : int;
    format : format
  }

module Encoder :
sig

  type t

  val create : video_format -> t

  val get_video_format : t -> video_format

  val encode_header : t -> Ogg.Stream.t -> unit

  val encode_frame : t -> frame -> Ogg.Stream.t -> unit

  val frames_of_granulepos : Int64.t -> t -> Int64.t

  val eos : t -> Ogg.Stream.t -> unit

end

