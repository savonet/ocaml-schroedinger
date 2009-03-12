/*
  Copyright 2003-2009 Savonet team

  This file is part of Ocaml-schroedinger.

  Ocaml-schroedinger is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  Ocaml-schroedinger is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Ocaml-schroedinger; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/bigarray.h>

#include <ogg/ogg.h>
#include <ocaml-ogg.h>

#include <string.h>
#include <schroedinger/schro.h>
#include <schroedinger/schroencoder.h>

#define ROUND_UP_SHIFT(x,y) (((x) + (1<<(y)) - 1)>>(y))

/* Common */

void frame_planar_free(SchroFrame *frame, void *private)
{
  free(frame->components[0].data);
  free(frame->components[1].data);
  free(frame->components[2].data);
}

static SchroFrame *schro_frame_of_val(value v)
{
  SchroFrame *frame = schro_frame_new();
  if (frame == NULL)
    caml_failwith("malloc");
  int i = 0;
  int j = 0;
  int h_shift;
  int v_shift;
  int len;
  int stride;
  value plane;
  value planes;
  struct caml_ba_array *data;
  void *tmp;

  planes = Field(v, i++);
  /* Get params */
  frame->width = Int_val(Field(v, i++));
  frame->height = Int_val(Field(v, i++));
  frame->format = Int_val(Field(v, i++));

  /* Get data */

  h_shift = SCHRO_FRAME_FORMAT_H_SHIFT(frame->format);
  v_shift = SCHRO_FRAME_FORMAT_V_SHIFT(frame->format);
  
  /* First plane */
  plane = Field(planes, j++);
  data = Caml_ba_array_val(Field(plane,0));
  stride = Int_val(Field(plane,1));
  len = stride*frame->height;
  if (stride < frame->width || 
      (int)data->dim[0] != len)
    caml_failwith("invalid frame dimension");
  tmp = malloc(sizeof(char)*len);
  if (tmp == NULL)
    caml_failwith("malloc");
  memcpy(tmp,data->data,len);
  frame->components[0].data = tmp;
  frame->components[0].stride = stride;
  frame->components[0].width = frame->width;
  frame->components[0].height = frame->height;
  frame->components[0].length = len;
  frame->components[0].h_shift = 0;
  frame->components[0].v_shift = 0;

  /* Secondary planes */
  plane = Field(planes, j++);
  data = Caml_ba_array_val(Field(plane,0));
  stride = Int_val(Field(plane,1));
  len = stride*(ROUND_UP_SHIFT(frame->height, v_shift));
  if (stride < ROUND_UP_SHIFT(frame->width, h_shift) ||
      (int)data->dim[0] != len)
    caml_failwith("invalid frame dimension");
  tmp = malloc(sizeof(char)*len);
  if (tmp == NULL)
    caml_failwith("malloc");
  memcpy(tmp,data->data,len);
  frame->components[1].data = tmp;
  frame->components[1].stride = stride;
  frame->components[1].width = ROUND_UP_SHIFT(frame->width, h_shift);
  frame->components[1].height = ROUND_UP_SHIFT(frame->height, v_shift);
  frame->components[1].length = len;
  frame->components[1].h_shift = h_shift;
  frame->components[1].v_shift = v_shift;

  plane = Field(planes, j++);
  data = Caml_ba_array_val(Field(plane,0));
  stride = Int_val(Field(plane,1));
  len = stride*(ROUND_UP_SHIFT(frame->height, v_shift));
  if (stride < (ROUND_UP_SHIFT(frame->width, h_shift)) ||
      (int)data->dim[0] != len)
    caml_failwith("invalid frame dimension");
  tmp = malloc(sizeof(char)*len);
  if (tmp == NULL)
    caml_failwith("malloc");
  memcpy(tmp,data->data,len);
  frame->components[2].data = tmp;
  frame->components[2].stride = stride;
  frame->components[2].width = ROUND_UP_SHIFT(frame->width, h_shift);
  frame->components[2].height = ROUND_UP_SHIFT(frame->height, v_shift);
  frame->components[2].length = len;
  frame->components[2].h_shift = h_shift;
  frame->components[2].v_shift = v_shift;

  schro_frame_set_free_callback(frame,frame_planar_free,NULL);

  return frame;
}

CAMLprim value caml_schroedinger_init(value unit)
{
  CAMLparam0();
  schro_init();
  CAMLreturn(Val_unit);
}

CAMLprim value caml_schroedinger_int_of_define(value v)
{
  CAMLparam1(v);
  char *s = String_val(v);
  if (!strcmp(s,"SCHRO_FRAME_FORMAT_U8_444"))
    CAMLreturn(Val_int(SCHRO_FRAME_FORMAT_U8_444)) ;
  if (!strcmp(s,"SCHRO_FRAME_FORMAT_U8_422"))
    CAMLreturn(Val_int(SCHRO_FRAME_FORMAT_U8_422)) ;
  if (!strcmp(s,"SCHRO_FRAME_FORMAT_U8_420"))
    CAMLreturn(Val_int(SCHRO_FRAME_FORMAT_U8_420)) ;

  caml_failwith("unknown value");
}

/* Encoding */

typedef struct {
  SchroEncoder *encoder;
  int is_sync_point;
  int distance_from_sync;
  ogg_int64_t decode_frame_number;
  ogg_int64_t presentation_frame_number;
  ogg_int64_t packet_no;
  int end_of_stream;
} encoder_t;

#define Schro_enc_val(v) (*((encoder_t**)Data_custom_val(v)))

static void finalize_schro_enc(value v)
{
  encoder_t *enc = Schro_enc_val(v);
  schro_encoder_free(enc->encoder);
  free(enc);
}

static struct custom_operations schro_enc_ops =
{
  "ocaml_gavl_schro_enc",
  finalize_schro_enc,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

static void calculate_granulepos (encoder_t *dd, ogg_packet *op)
{
    ogg_int64_t granulepos_hi;
    ogg_int64_t granulepos_low;
    int pt, dt, dist, delay;
    if (dd->is_sync_point)
        dd->distance_from_sync = 0;
    else
        dd->distance_from_sync++;

    pt = dd->presentation_frame_number * 2;
    dt = dd->decode_frame_number * 2;
    delay = pt - dt;
    dist = dd->distance_from_sync;

    granulepos_hi = ((pt - delay)<<9) | ((dist>>8));
    granulepos_low = (delay << 9) | (dist & 0xff);

    op->granulepos = (granulepos_hi << 22) | (granulepos_low);
    op->packetno = dd->packet_no++;
    dd->decode_frame_number++;
}

CAMLprim value ocaml_schroedinger_create_enc(value unit)
{
  CAMLparam0();
  CAMLlocal1(ret);
  encoder_t *enc = malloc(sizeof(encoder_t));
  if (enc == NULL)
    caml_failwith("malloc"); 

  enc->decode_frame_number = -1;
  enc->presentation_frame_number = 0;
  enc->distance_from_sync = 0;
  enc->packet_no = 0;
  enc->end_of_stream = 0;
  enc->is_sync_point = 1;
 
  SchroEncoder *encoder = schro_encoder_new();
  if (encoder == NULL)
    caml_failwith("schro_encoder_new");

  enc->encoder = encoder;

  ret = caml_alloc_custom(&schro_enc_ops, sizeof(encoder_t*), 1, 0);
  Schro_enc_val(ret) = enc;

  CAMLreturn(ret);
}

CAMLprim value ocaml_schroedinger_enc_eos(value enc)
{
  CAMLparam1(enc);
  encoder_t *p = Schro_enc_val(enc);

  schro_encoder_end_of_stream(p->encoder);
  p->end_of_stream = 1;

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_schroedinger_enc_encode_frame(value _enc, value frame, value _os)
{
  CAMLparam2(_enc,frame);
  encoder_t *enc = Schro_enc_val(_enc);
  ogg_stream_state *os = Stream_state_val(_os);
  /* You'll go to Hell for using static variables */
  ogg_packet op;
  SchroStateEnum state;
  SchroBuffer *enc_buf;
  SchroFrame *f = schro_frame_of_val(frame);
  
  /* Put the frame into the encoder. */
  schro_encoder_push_frame(enc->encoder, f);
    
  /* Add a new ogg packet */


  CAMLreturn(Val_unit);
}

