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

let check = Schroedinger.Decoder.check

let decoder os =
  let decoder = ref None in
  let packet1 = ref None in
  let packet2 = ref None in
  let latest_yuv = ref None in
  let os = ref os in
  let init () = 
    match !decoder with
      | Some x -> x
      | None ->
          let get_packet packet = 
            match !packet with
              | Some x -> x
              | None   -> 
                    let p = Ogg.Stream.get_packet !os in
                    packet := Some p;
                    p
          in
          let packet1 = get_packet packet1 in
          let packet2 = get_packet packet2 in
          let dec = Schroedinger.Decoder.create packet1 packet2 in
          let video_format = Schroedinger.Decoder.get_video_format dec in
          decoder := Some (dec,video_format);
          dec,video_format
  in
  let decode feed = 
    let (decoder,_) = init () in
    let ret = 
     try
      let yuv = Schroedinger.Decoder.decode_frame decoder !os in
      latest_yuv := Some yuv ;
      yuv
     with
       | Schroedinger.Decoder.Skipped_frame -> 
          begin
            match !latest_yuv with
              | Some yuv -> yuv
              | None     -> assert false
          end
    in
    let format =
      match ret.Schroedinger.format with
        | Schroedinger.Yuv_422_p -> Ogg_demuxer.Yuvj_422
        | Schroedinger.Yuv_444_p -> Ogg_demuxer.Yuvj_444
        | Schroedinger.Yuv_420_p -> Ogg_demuxer.Yuvj_420
    in
    let ret =
      {
        Ogg_demuxer.
          format = format;
          frame_width = ret.Schroedinger.frame_width;
          frame_height = ret.Schroedinger.frame_height;
          y_stride  = snd ret.Schroedinger.planes.(0);
          uv_stride = snd ret.Schroedinger.planes.(1);
          y = fst ret.Schroedinger.planes.(0);
          u = fst ret.Schroedinger.planes.(1);
          v = fst ret.Schroedinger.planes.(2)
      }
    in
    feed ret
  in
  let info () =
    let _,info = init () in
    { Ogg_demuxer.
       fps_numerator = info.Schroedinger.frame_rate_numerator;
       fps_denominator = info.Schroedinger.frame_rate_numerator;
       width = info.Schroedinger.width;
       height = info.Schroedinger.height
    },("ocaml-schroedinger",[])
  in
  let restart new_os =
    os := new_os
  in
  let samples_of_granulepos pos =
    let (decoder,info) = init () in
    Schroedinger.frames_of_granulepos
        ~interlaced:info.Schroedinger.interlaced pos
  in
  Ogg_demuxer.Video 
    { Ogg_demuxer.
       name = "dirac (schroedinger implementation)";
       info = info;
       decode = decode;
       restart = restart;
       samples_of_granulepos = samples_of_granulepos }

let register () =
  Hashtbl.add Ogg_demuxer.ogg_decoders "dirac (schroedinger implementation)" (check,decoder)
