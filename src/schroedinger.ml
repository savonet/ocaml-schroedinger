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

let video_type_of_int x =
  let f = int_of_define in
  match x with
    | x when x = f "SCHRO_VIDEO_FORMAT_CUSTOM" -> CUSTOM
    | x when x = f "SCHRO_VIDEO_FORMAT_QSIF" -> QSIF
    | x when x = f "SCHRO_VIDEO_FORMAT_QCIF" -> QCIF
    | x when x = f "SCHRO_VIDEO_FORMAT_SIF" -> SIF
    | x when x = f "SCHRO_VIDEO_FORMAT_CIF" -> CIF
    | x when x = f "SCHRO_VIDEO_FORMAT_4SIF" -> SIF_4
    | x when x = f "SCHRO_VIDEO_FORMAT_4CIF" -> CIF_4
    | x when x = f "SCHRO_VIDEO_FORMAT_SD480I_60" -> SD480I_60
    | x when x = f "SCHRO_VIDEO_FORMAT_SD576I_50" -> SD576I_50
    | x when x = f "SCHRO_VIDEO_FORMAT_HD720P_60" -> HD720P_60
    | x when x = f "SCHRO_VIDEO_FORMAT_HD720P_50" -> HD720P_50
    | x when x = f "SCHRO_VIDEO_FORMAT_HD1080I_60" -> HD1080I_60
    | x when x = f "SCHRO_VIDEO_FORMAT_HD1080I_50" -> HD1080I_50
    | x when x = f "SCHRO_VIDEO_FORMAT_HD1080P_60" -> HD1080P_60
    | x when x = f "SCHRO_VIDEO_FORMAT_HD1080P_50" -> HD1080P_50
    | x when x = f "SCHRO_VIDEO_FORMAT_DC2K_24" -> DC2K_24
    | x when x = f "SCHRO_VIDEO_FORMAT_DC4K_24" -> DC4K_24
    | _ -> assert false

type chroma = 
  | Chroma_422
  | Chroma_444
  | Chroma_420

let int_of_chroma x = 
  match x with
    | Chroma_422 -> int_of_define "SCHRO_CHROMA_422"
    | Chroma_444 -> int_of_define "SCHRO_CHROMA_444"
    | Chroma_420 -> int_of_define "SCHRO_CHROMA_420"

let chroma_of_int x =
  match x with
    | x when x = int_of_define "SCHRO_CHROMA_422" -> Chroma_422
    | x when x = int_of_define "SCHRO_CHROMA_444" -> Chroma_444
    | x when x = int_of_define "SCHRO_CHROMA_420" -> Chroma_420
    | _ -> assert false

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

let colour_primaries_of_int x =
  match x with
    | x when x = int_of_define "SCHRO_COLOUR_PRIMARY_HDTV" -> HDTV
    | x when x = int_of_define "SCHRO_COLOUR_PRIMARY_SDTV_525" -> SDTV_525
    | x when x = int_of_define "SCHRO_COLOUR_PRIMARY_SDTV_625" -> SDTV_625
    | x when x = int_of_define "SCHRO_COLOUR_PRIMARY_CINEMA" -> CINEMA
    | _ -> assert false

type colour_matrix = 
  | HDTV
  | SDTV
  | REVERSIBLE

let int_of_colour_matrix x = 
  match x with
    | HDTV -> int_of_define "SCHRO_COLOUR_MATRIX_HDTV"
    | SDTV -> int_of_define "SCHRO_COLOUR_MATRIX_SDTV"
    | REVERSIBLE -> int_of_define "SCHRO_COLOUR_MATRIX_REVERSIBLE"

let colour_matrix_of_int x =
  match x with
    | x when x = int_of_define "SCHRO_COLOUR_MATRIX_HDTV" -> HDTV
    | x when x = int_of_define "SCHRO_COLOUR_MATRIX_SDTV" -> SDTV
    | x when x = int_of_define "SCHRO_COLOUR_MATRIX_REVERSIBLE" -> REVERSIBLE
    | _ -> assert false

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

let transfer_function_of_int x =
  match x with
    | x when x = int_of_define "SCHRO_TRANSFER_CHAR_TV_GAMMA" -> TV_GAMMA
    | x when x = int_of_define "SCHRO_TRANSFER_CHAR_EXTENDED_GAMMUT" -> EXTENDED_GAMMUT
    | x when x = int_of_define "SCHRO_TRANSFER_CHAR_LINEAR" -> LINEAR
    | x when x = int_of_define "SCHRO_TRANSFER_CHAR_DCI_GAMMA" -> DCI_GAMMA
    | _ -> assert false

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

let video_format_of_internal_video_format x =
 {
  video_type = video_type_of_int x.int_video_type;
  width = x.int_width;
  height = x.int_height;
  chroma_format = chroma_of_int x.int_chroma_format;

  interlaced = x.int_interlaced;
  top_field_first = x.int_top_field_first;

  frame_rate_numerator = x.int_frame_rate_numerator;
  frame_rate_denominator = x.int_frame_rate_denominator;
  aspect_ratio_numerator = x.int_aspect_ratio_numerator;
  aspect_ratio_denominator = x.int_aspect_ratio_denominator;

  clean_width = x.int_clean_width;
  clean_height = x.int_clean_height;
  left_offset = x.int_left_offset;
  top_offset = x.int_top_offset;

  luma_offset = x.int_luma_offset;
  luma_excursion = x.int_luma_excursion;
  chroma_offset = x.int_chroma_offset;
  chroma_excursion = x.int_chroma_excursion;

  colour_primaries = colour_primaries_of_int x.int_colour_primaries;
  colour_matrix = colour_matrix_of_int x.int_colour_matrix;
  transfer_function = transfer_function_of_int x.int_transfer_function;

  interlaced_coding = x.int_interlaced_coding
 }

external get_default_internal_video_format : int -> internal_video_format = "ocaml_schroedinger_get_default_video_format"

let get_default_video_format x =
  video_format_of_internal_video_format
    (get_default_internal_video_format (int_of_video_type x))

type frame = 
  { 
    planes : (plane*int) array;
    frame_width  : int;
    frame_height : int;
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
    int_width  = f.frame_width;
    int_height = f.frame_height;
    int_format = int_of_format f.format
  }
  
module Encoder = 
struct

  type t

  external create : internal_video_format -> t = "ocaml_schroedinger_create_enc"

  let create f = 
    create (internal_video_format_of_video_format f)

  external get_video_format : t -> internal_video_format = "ocaml_schroedinger_enc_video_format"

  let get_video_format x = 
    video_format_of_internal_video_format (get_video_format x)

  external eos : t -> Ogg.Stream.t -> unit = "ocaml_schroedinger_enc_eos"

  external encode_header : t -> Ogg.Stream.t -> unit = "ocaml_schroedinger_encode_header"

  external encode_frame : t -> internal_frame -> Ogg.Stream.t -> unit = "ocaml_schroedinger_encode_frame" 

  let encode_frame t f = encode_frame t (internal_frame_of_frame f)

  external frames_of_granulepos : Int64.t -> t -> Int64.t = "ocaml_schroedinger_frames_of_granulepos"

end

module Skeleton =
struct

  external fisbone : Nativeint.t -> internal_video_format -> 
                     Int64.t -> string -> Ogg.Stream.packet = "ocaml_schroedinger_skeleton_fisbone"

  let fisbone ?(start_granule=Int64.zero)
              ?(headers=["Content-type","video/dirac"])
              ~serialno ~format () =
    let concat s (h,v) =
      Printf.sprintf "%s%s: %s\r\n" s h v
    in
    let s =
      List.fold_left concat "" headers
    in
    fisbone serialno (internal_video_format_of_video_format format) 
            start_granule s

end
