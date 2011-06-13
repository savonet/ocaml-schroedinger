(*
 * Copyright 2003-2011 Savonet team
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

let format_of_int x = 
  let f = int_of_define in
  match x with
    | x when x = f "SCHRO_FRAME_FORMAT_U8_422" -> Yuv_422_p
    | x when x = f "SCHRO_FRAME_FORMAT_U8_444" -> Yuv_444_p
    | x when x = f "SCHRO_FRAME_FORMAT_U8_420" -> Yuv_420_p
    | _ -> assert false

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

type signal_range = 
  | RANGE_CUSTOM
  | RANGE_8BIT_FULL
  | RANGE_8BIT_VIDEO
  | RANGE_10BIT_VIDEO
  | RANGE_12BIT_VIDEO

let int_of_signal_range x =
  match x with
  | RANGE_CUSTOM -> int_of_define "SCHRO_SIGNAL_RANGE_CUSTOM"
  | RANGE_8BIT_FULL -> int_of_define "SCHRO_SIGNAL_RANGE_8BIT_FULL"
  | RANGE_8BIT_VIDEO -> int_of_define "SCHRO_SIGNAL_RANGE_8BIT_VIDEO"
  | RANGE_10BIT_VIDEO -> int_of_define "SCHRO_SIGNAL_RANGE_10BIT_VIDEO"
  | RANGE_12BIT_VIDEO -> int_of_define "SCHRO_SIGNAL_RANGE_12BIT_VIDEO"

let signal_range_of_int x = 
  match x with
    | x when x = int_of_define "SCHRO_SIGNAL_RANGE_CUSTOM" -> RANGE_CUSTOM
    | x when x = int_of_define "SCHRO_SIGNAL_RANGE_8BIT_FULL" -> RANGE_8BIT_FULL
    | x when x = int_of_define "SCHRO_SIGNAL_RANGE_8BIT_VIDEO" -> RANGE_8BIT_VIDEO
    | x when x = int_of_define "SCHRO_SIGNAL_RANGE_10BIT_VIDEO" -> RANGE_10BIT_VIDEO
    | x when x = int_of_define "SCHRO_SIGNAL_RANGE_12BIT_VIDEO" -> RANGE_12BIT_VIDEO
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

  signal_range : signal_range;
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

  int_signal_range : int;
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

  int_interlaced_coding = x.interlaced_coding;

  int_signal_range = int_of_signal_range x.signal_range
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

  interlaced_coding = x.int_interlaced_coding;

  signal_range = signal_range_of_int x.int_signal_range
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

let frame_of_internal_frame f = 
  if (Array.length f.int_planes <> 3) then
    failwith "Frame does not have 3 planes. \
              Only planar formats are supported for now..";
  { planes = f.int_planes;
    frame_width  = f.int_width;
    frame_height = f.int_height;
    format = format_of_int f.int_format
  }

external frames_of_granulepos : Int64.t -> bool -> Int64.t = "ocaml_schroedinger_frames_of_granulepos"

let frames_of_granulepos ~interlaced pos = 
  frames_of_granulepos pos interlaced

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

  external encoded_of_granulepos : Int64.t -> t -> Int64.t = "ocaml_schroedinger_encoded_of_granulepos"

  type rate_control = 
    | Constant_noise_threshold
    | Constant_bitrate
    | Low_delay
    | Lossless
    | Constant_lambda
    | Constant_error
  
  type gop_structure = 
    | Adaptive
    | Intra_only
    | Backref
    | Chained_backref
    | Biref
    | Chained_biref 
  
  type perceptual_weighting = 
    | None
    | Ccir959
    | Moo
    | Manos_sakrison
  
  type filtering = 
    | None
    | Center_weighted_median
    | Gaussian
    | Add_noise
    | Adaptive_gaussian 
  
  type wavelet = 
    | Desl_dubuc_9_7
    | Le_gall_5_3
    | Desl_dubuc_13_7
    | Haar_0
    | Haar_1
    | Fidelity
    | Daub_9_7
  
  type block_size = 
    | Automatic
    | Small
    | Medium
    | Large
  
  type block_overlap = 
    | Automatic
    | None
    | Partial
    | Full 

  type settings = 
  {
    rate_control: rate_control;
    bitrate: int;
    max_bitrate: int;
    min_bitrate: int;
    buffer_size: int;
    buffer_level: int;
    noise_threshold: float;
    gop_structure: gop_structure;
    queue_depth: int;
    perceptual_weighting: perceptual_weighting;
    perceptual_distance: float;
    filtering: filtering;
    filter_value: float;
    profile: int;
    level: int;
    au_distance: int;
    enable_psnr: bool;
    enable_ssim: bool;
    ref_distance: int;
    transform_depth: int;
    intra_wavelet: wavelet;
    inter_wavelet: wavelet;
    mv_precision: int;
    motion_block_size: block_size;
    motion_block_overlap: block_overlap;
    interlaced_coding: bool;
    enable_internal_testing: bool;
    enable_noarith: bool;
    enable_md5: bool;
    enable_fullscan_estimation: bool;
    enable_hierarchical_estimation: bool;
    enable_zero_estimation: bool;
    enable_phasecorr_estimation: bool;
    enable_bigblock_estimation: bool;
    horiz_slices: int;
    vert_slices: int;
    magic_dc_metric_offset: float;
    magic_subband0_lambda_scale: float;
    magic_chroma_lambda_scale: float;
    magic_nonref_lambda_scale: float;
    magic_allocation_scale: float;
    magic_keyframe_weight: float;
    magic_scene_change_threshold: float;
    magic_inter_p_weight: float;
    magic_inter_b_weight: float;
    magic_mc_bailout_limit: float;
    magic_bailout_weight: float;
    magic_error_power: float;
    magic_mc_lambda: float;
    magic_subgroup_length: float;
    magic_lambda: float
  }

  external set_setting : t -> string -> 'a -> unit = "ocaml_schroedinger_set_setting"

  let set_settings enc x = 
    set_setting enc "rate_control" x.rate_control;
    set_setting enc "bitrate" x.bitrate;
    set_setting enc "max_bitrate" x.max_bitrate;
    set_setting enc "min_bitrate" x.min_bitrate;
    set_setting enc "buffer_size" x.buffer_size;
    set_setting enc "buffer_level" x.buffer_level;
    set_setting enc "noise_threshold" x.noise_threshold;
    set_setting enc "gop_structure" x.gop_structure;
    set_setting enc "queue_depth" x.queue_depth;
    set_setting enc "perceptual_weighting" x.perceptual_weighting;
    set_setting enc "perceptual_distance" x.perceptual_distance;
    set_setting enc "filtering" x.filtering;
    set_setting enc "filter_value" x.filter_value;
    set_setting enc "profile" x.profile;
    set_setting enc "level" x.level;
    set_setting enc "au_distance" x.au_distance;
    set_setting enc "enable_psnr" x.enable_psnr;
    set_setting enc "enable_ssim" x.enable_ssim;
    set_setting enc "ref_distance" x.ref_distance;
    set_setting enc "transform_depth" x.transform_depth;
    set_setting enc "intra_wavelet" x.intra_wavelet;
    set_setting enc "inter_wavelet" x.inter_wavelet;
    set_setting enc "mv_precision" x.mv_precision;
    set_setting enc "motion_block_size" x.motion_block_size;
    set_setting enc "motion_block_overlap" x.motion_block_overlap;
    set_setting enc "interlaced_coding" x.interlaced_coding;
    set_setting enc "enable_internal_testing" x.enable_internal_testing;
    set_setting enc "enable_noarith" x.enable_noarith;
    set_setting enc "enable_md5" x.enable_md5;
    set_setting enc "enable_fullscan_estimation" x.enable_fullscan_estimation;
    set_setting enc "enable_hierarchical_estimation" x.enable_hierarchical_estimation;
    set_setting enc "enable_zero_estimation" x.enable_zero_estimation;
    set_setting enc "enable_phasecorr_estimation" x.enable_phasecorr_estimation;
    set_setting enc "enable_bigblock_estimation" x.enable_bigblock_estimation;
    set_setting enc "horiz_slices" x.horiz_slices;
    set_setting enc "vert_slices" x.vert_slices;
    set_setting enc "magic_dc_metric_offset" x.magic_dc_metric_offset;
    set_setting enc "magic_subband0_lambda_scale" x.magic_subband0_lambda_scale;
    set_setting enc "magic_chroma_lambda_scale" x.magic_chroma_lambda_scale;
    set_setting enc "magic_nonref_lambda_scale" x.magic_nonref_lambda_scale;
    set_setting enc "magic_allocation_scale" x.magic_allocation_scale;
    set_setting enc "magic_keyframe_weight" x.magic_keyframe_weight;
    set_setting enc "magic_scene_change_threshold" x.magic_scene_change_threshold;
    set_setting enc "magic_inter_p_weight" x.magic_inter_p_weight;
    set_setting enc "magic_inter_b_weight" x.magic_inter_b_weight;
    set_setting enc "magic_mc_bailout_limit" x.magic_mc_bailout_limit;
    set_setting enc "magic_bailout_weight" x.magic_bailout_weight;
    set_setting enc "magic_error_power" x.magic_error_power;
    set_setting enc "magic_mc_lambda" x.magic_mc_lambda;
    set_setting enc "magic_subgroup_length" x.magic_subgroup_length;
    set_setting enc "magic_lambda" x.magic_lambda

  external get_setting : t -> string -> 'a = "ocaml_schroedinger_get_setting"

  let get_settings enc = 
   {
    rate_control = get_setting enc "rate_control";
    bitrate = get_setting enc "bitrate";
    max_bitrate = get_setting enc "max_bitrate";
    min_bitrate = get_setting enc "min_bitrate";
    buffer_size = get_setting enc "buffer_size";
    buffer_level = get_setting enc "buffer_level";
    noise_threshold = get_setting enc "noise_threshold";
    gop_structure = get_setting enc "gop_structure";
    queue_depth = get_setting enc "queue_depth";
    perceptual_weighting = get_setting enc "perceptual_weighting";
    perceptual_distance = get_setting enc "perceptual_distance";
    filtering = get_setting enc "filtering";
    filter_value = get_setting enc "filter_value";
    profile = get_setting enc "profile";
    level = get_setting enc "level";
    au_distance = get_setting enc "au_distance";
    enable_psnr = get_setting enc "enable_psnr";
    enable_ssim = get_setting enc "enable_ssim";
    ref_distance = get_setting enc "ref_distance";
    transform_depth = get_setting enc "transform_depth";
    intra_wavelet = get_setting enc "intra_wavelet";
    inter_wavelet = get_setting enc "inter_wavelet";
    mv_precision = get_setting enc "mv_precision";
    motion_block_size = get_setting enc "motion_block_size";
    motion_block_overlap = get_setting enc "motion_block_overlap";
    interlaced_coding = get_setting enc "interlaced_coding";
    enable_internal_testing = get_setting enc "enable_internal_testing";
    enable_noarith = get_setting enc "enable_noarith";
    enable_md5 = get_setting enc "enable_md5";
    enable_fullscan_estimation = get_setting enc "enable_fullscan_estimation";
    enable_hierarchical_estimation = get_setting enc "enable_hierarchical_estimation";
    enable_zero_estimation = get_setting enc "enable_zero_estimation";
    enable_phasecorr_estimation = get_setting enc "enable_phasecorr_estimation";
    enable_bigblock_estimation = get_setting enc "enable_bigblock_estimation";
    horiz_slices = get_setting enc "horiz_slices";
    vert_slices = get_setting enc "vert_slices";
    magic_dc_metric_offset = get_setting enc "magic_dc_metric_offset";
    magic_subband0_lambda_scale = get_setting enc "magic_subband0_lambda_scale";
    magic_chroma_lambda_scale = get_setting enc "magic_chroma_lambda_scale";
    magic_nonref_lambda_scale = get_setting enc "magic_nonref_lambda_scale";
    magic_allocation_scale = get_setting enc "magic_allocation_scale";
    magic_keyframe_weight = get_setting enc "magic_keyframe_weight";
    magic_scene_change_threshold = get_setting enc "magic_scene_change_threshold";
    magic_inter_p_weight = get_setting enc "magic_inter_p_weight";
    magic_inter_b_weight = get_setting enc "magic_inter_b_weight";
    magic_mc_bailout_limit = get_setting enc "magic_mc_bailout_limit";
    magic_bailout_weight = get_setting enc "magic_bailout_weight";
    magic_error_power = get_setting enc "magic_error_power";
    magic_mc_lambda = get_setting enc "magic_mc_lambda";
    magic_subgroup_length = get_setting enc "magic_subgroup_length";
    magic_lambda = get_setting enc "magic_lambda"
   }
end

module Decoder = 
struct

  exception Invalid_header
  exception Skipped_frame
  exception Error

  let _ =
    Callback.register_exception "schro_exn_invalid_header" Invalid_header ;
    Callback.register_exception "schro_exn_skip" Skipped_frame ;
    Callback.register_exception "schro_exn_error" Error

  type t

  external create : Ogg.Stream.packet -> t = "ocaml_schroedinger_create_dec"

  let check p = 
    try
      ignore(create p);
      true
    with
       | Invalid_header -> false

  let create p1 p2 = create p2

  external get_video_format : t -> video_format = "ocaml_schroedinger_decoder_get_format"

  external get_picture_number : t -> int = "ocaml_schroedinger_decoder_get_picture_number"

  external decode_frame : t -> Ogg.Stream.t -> internal_frame = "ocaml_schroedinger_decoder_decode_frame"

  let decode_frame dec os = 
    frame_of_internal_frame (decode_frame dec os)    

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
