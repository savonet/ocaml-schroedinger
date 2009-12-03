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

type signal_range =
  | RANGE_CUSTOM
  | RANGE_8BIT_FULL
  | RANGE_8BIT_VIDEO
  | RANGE_10BIT_VIDEO
  | RANGE_12BIT_VIDEO

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

  signal_range : signal_range
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

  val encoded_of_granulepos : Int64.t -> t -> Int64.t

  val eos : t -> Ogg.Stream.t -> unit

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

  val get_settings : t -> settings

  val set_settings : t -> settings -> unit

end

module Decoder :
sig

  exception Invalid_header
  exception Skipped_frame
  exception Error

  type t

  val create : Ogg.Stream.packet -> Ogg.Stream.packet -> t

  val check : Ogg.Stream.packet -> bool

  val get_video_format : t -> video_format

  val get_picture_number : t -> int

  val decode_frame : t -> Ogg.Stream.t -> frame

end

module Skeleton :
sig

  (** Generate a vorbis fisbone packet with
    * these parameters, to use in an ogg skeleton.
    * Default value for [start_granule] is [Int64.zero],
    * Default value for [headers] is ["Content-type","video/dirac"]
    *
    * See: http://xiph.org/ogg/doc/skeleton.html. *)
  val fisbone :
    ?start_granule:Int64.t ->
    ?headers:(string * string) list ->
    serialno:Nativeint.t -> format:video_format -> 
    unit -> Ogg.Stream.packet

end

