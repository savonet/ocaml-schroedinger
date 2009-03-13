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

external int_of_define : string -> int = "ocaml_schroedinger_int_of_define"

(* Only planar formats for now.. *)
type format = 
  | Yuv_422_p    (** Planar YCbCr 4:2:2. Each component is an uint8_t *)
  | Yuv_444_p    (** Planar YCbCr 4:4:4. Each component is an uint8_t *)
  | Yuv_420_p    (** Planar YCbCr 4:2:0. Each component is an uint8_t,
                   * luma and chroma values are full range (0x00 .. 0xff) *)

let int_of_format f =
  match f with
   | Yuv_422_p -> int_of_define "SCHRO_FRAME_FORMAT_U8_422"
   | Yuv_444_p -> int_of_define "SCHRO_FRAME_FORMAT_U8_444"
   | Yuv_420_p -> int_of_define "SCHRO_FRAME_FORMAT_U8_420"

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

let int_of_video_type x = 
  let f = int_of_define in
  match x with
    | CUSTOM -> f "SCHRO_VIDEO_FORMAT_CUSTOM"
    | QSIF -> f "SCHRO_VIDEO_FORMAT_QSIF"
    | QCIF -> f "SCHRO_VIDEO_FORMAT_QCIF"
    | SIF -> f "SCHRO_VIDEO_FORMAT_SIF"
    | CIF -> f "SCHRO_VIDEO_FORMAT_CIF"
    | SIF_4 -> f "SCHRO_VIDEO_FORMAT_4SIF"
    | CIF_4 -> f "SCHRO_VIDEO_FORMAT_4CIF"
    | SD480I_60 -> f "SCHRO_VIDEO_FORMAT_SD480I_60"
    | SD576I_50 -> f "SCHRO_VIDEO_FORMAT_SD576I_50"
    | HD720P_60 -> f "SCHRO_VIDEO_FORMAT_HD720P_60"
    | HD720P_50 -> f "SCHRO_VIDEO_FORMAT_HD720P_50"
    | HD1080I_60 -> f "SCHRO_VIDEO_FORMAT_HD1080I_60"
    | HD1080I_50 -> f "SCHRO_VIDEO_FORMAT_HD1080I_50"
    | HD1080P_60 -> f "SCHRO_VIDEO_FORMAT_HD1080P_60"
    | HD1080P_50 -> f "SCHRO_VIDEO_FORMAT_HD1080P_50"
    | DC2K_24 -> f "SCHRO_VIDEO_FORMAT_DC2K_24"
    | DC4K_24 -> f "SCHRO_VIDEO_FORMAT_DC4K_24"

type chroma = 
  | Chroma_422
  | Chroma_444
  | Chroma_420

let int_of_chroma x = 
  match x with
    | Chroma_422 -> int_of_define "SCHRO_CHROMA_422"
    | Chroma_444 -> int_of_define "SCHRO_CHROMA_444"
    | Chroma_420 -> int_of_define "SCHRO_CHROMA_420"

type colour_primaries = 
  | HDTV
  | SDTV_525
  | SDTV_625
  | CINEMA

let int_of_colour_primaries x = 
  match x with
    | HDTV -> int_of_define "SCHRO_COLOUR_PRIMARY_HDTV"
    | SDTV_525 -> int_of_define "SCHRO_COLOUR_PRIMARY_SDTV_525"
    | SDTV_625 -> int_of_define "SCHRO_COLOUR_PRIMARY_SDTV_625"
    | CINEMA -> int_of_define "SCHRO_COLOUR_PRIMARY_CINEMA"

type colour_matrix = 
  | HDTV
  | SDTV
  | REVERSIBLE

let int_of_colour_matrix x = 
  match x with
    | HDTV -> int_of_define "SCHRO_COLOUR_MATRIX_HDTV"
    | SDTV -> int_of_define "SCHRO_COLOUR_MATRIX_SDTV"
    | REVERSIBLE -> int_of_define "SCHRO_COLOUR_MATRIX_REVERSIBLE"

type transfer_function = 
  | TV_GAMMA
  | EXTENDED_GAMMUT
  | LINEAR
  | DCI_GAMMA

let int_of_transfer_function x =
  match x with 
    | TV_GAMMA -> int_of_define "SCHRO_TRANSFER_CHAR_TV_GAMMA"
    | EXTENDED_GAMMUT -> int_of_define "SCHRO_TRANSFER_CHAR_EXTENDED_GAMMUT"
    | LINEAR -> int_of_define "SCHRO_TRANSFER_CHAR_LINEAR"
    | DCI_GAMMA -> int_of_define "SCHRO_TRANSFER_CHAR_DCI_GAMMA"

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

type internal_video_format = 
 {
  int_video_type : int;
  int_width : int;
  int_height : int;
  int_chroma_format : int; 

  int_interlaced : bool;
  int_top_field_first : bool;

  int_frame_rate_numerator : int;
  int_frame_rate_denominator : int;
  int_aspect_ratio_numerator : int;
  int_aspect_ratio_denominator : int;

  int_clean_width : int;
  int_clean_height : int;
  int_left_offset : int;
  int_top_offset : int;

  int_luma_offset : int;
  int_luma_excursion : int;
  int_chroma_offset : int;
  int_chroma_excursion : int;

  int_colour_primaries: int;
  int_colour_matrix : int;
  int_transfer_function : int;

  int_interlaced_coding : bool;
 }

let internal_video_format_of_video_format x = 
 {
  int_video_type = int_of_video_type x.video_type;
  int_width = x.width;
  int_height = x.height;
  int_chroma_format = int_of_chroma x.chroma_format;

  int_interlaced = x.interlaced;
  int_top_field_first = x.top_field_first;

  int_frame_rate_numerator = x.frame_rate_numerator;
  int_frame_rate_denominator = x.frame_rate_denominator;
  int_aspect_ratio_numerator = x.aspect_ratio_numerator;
  int_aspect_ratio_denominator = x.aspect_ratio_denominator;

  int_clean_width = x.clean_width;
  int_clean_height = x.clean_height;
  int_left_offset = x.left_offset;
  int_top_offset = x.top_offset;

  int_luma_offset = x.luma_offset;
  int_luma_excursion = x.luma_excursion;
  int_chroma_offset = x.chroma_offset;
  int_chroma_excursion = x.chroma_excursion;

  int_colour_primaries = int_of_colour_primaries x.colour_primaries;
  int_colour_matrix = int_of_colour_matrix x.colour_matrix;
  int_transfer_function = int_of_transfer_function x.transfer_function;

  int_interlaced_coding = x.interlaced_coding
 }

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

