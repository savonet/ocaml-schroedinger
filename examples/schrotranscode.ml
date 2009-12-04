exception No_dirac

open Schroedinger

let infile = ref "input.ogg"
let outfile = ref "output.ogg"
let debug = ref false

let quality = ref 35.

let () =
  Arg.parse
    [
      "-d", Arg.Set debug, "Show debugging messages";
      "-o", Arg.Set_string outfile, "Output file";
      "-q", Arg.Set_float quality, "Quality of the compression";
      "-i", Arg.Set_string infile, "Input file";
    ]
    ignore
    "schrotranscode [options]"

let in_init () =
  let sync,fd = Ogg.Sync.create_from_file !infile in
  let rec fill os =
    let page = Ogg.Sync.read sync in
    try
      (* We drop pages which are not for us.. *)
      if Ogg.Page.serialno page = 
         Ogg.Stream.serialno os
      then
        Ogg.Stream.put_page os page ;
    with
       | Ogg.Bad_data -> fill os (* Do not care about page that are not for us.. *)
  in
  (** Test wether the stream is dirac *)
  let test_dirac () = 
    (** Get First page *)
    let page = Ogg.Sync.read sync in
    (** Check wether this is a b_o_s *)
    if not (Ogg.Page.bos page) then raise No_dirac ;
    (** Create a stream with this ID *)
    let serial = Ogg.Page.serialno page in
    Printf.printf "Testing stream %nx\n" serial ;
    let os = Ogg.Stream.create ~serial () in
    Ogg.Stream.put_page os page ;
    let packet = Ogg.Stream.get_packet os in
    (** Test header. Do not catch anything, first page should be sufficient *)
    if not (Decoder.check packet) then 
      raise Not_found;
    Printf.printf "Got a dirac stream !\n" ;
    (** Get second packet *)
    let rec f () = 
     try
      Ogg.Stream.get_packet os
     with
       | Ogg.Not_enough_data -> (fill os; f ())
    in 
    let packet2 = f () in     
    let dec = Decoder.create packet packet2 in
    let video_format = Decoder.get_video_format dec in
    serial,os,dec,video_format
  in
  (** Now find a theora stream *)
  let rec init () = 
    try 
      test_dirac ()
    with
      | Not_found -> 
         ( Printf.printf "This stream was not dirac..\n";
           init () )
      | No_dirac ->
         ( Printf.printf "No dirac stream was found..\n%!";
           raise No_dirac )
  in
  let serial,os,t,video_format = init () in
  Printf.printf 
     "Ogg logical stream %nx is Dirac %dx%d %.02f fps video\n"
     serial video_format.width video_format.height
     ((float_of_int video_format.frame_rate_numerator) /. 
      (float_of_int video_format.frame_rate_denominator)) ;
  flush_all ();
  t,os,fill,video_format,fd

let out_init video_format =
  let oc = open_out !outfile in
  let out s = output_string oc s; flush oc in
  let os = Ogg.Stream.create () in
  let enc = Encoder.create video_format in
  Encoder.set_settings enc
    {  (Encoder.get_settings enc) with
          Schroedinger.Encoder.
           rate_control = Encoder.Constant_noise_threshold;
           noise_threshold = !quality;
    };
    out (Ogg.Stream.flush os);
    enc,os,out

let () = 
  let dec,is,fill,video_format,fd = in_init () in
  let enc,os,out = out_init video_format in
  let latest_frame = ref None in
  let rec get_frame () = 
    try
      let frame = Decoder.decode_frame dec is in
      latest_frame := Some frame;
      frame
    with
      | Decoder.Skipped_frame -> 
          begin
            match !latest_frame with
              | Some f -> f
              | None   -> assert false
          end
      | Ogg.Not_enough_data when not (Ogg.Stream.eos is) -> 
           (fill is; get_frame ())
  in
  let rec generator () =
    try
      Encoder.encode_frame enc (get_frame ()) os;
      Ogg.Stream.get_page os
    with 
      | Ogg.Not_enough_data when not (Ogg.Stream.eos is) -> 
           generator ()
  in
  Printf.printf "Starting transcoding loop !\n%!";
  begin
   try
    while true do
      let op = generator () in
      let s_o_p (h,b) = h ^ b in
      let op = s_o_p op in
        out op;
    done;
   with
     | Ogg.Not_enough_data -> ()
  end ;
  Encoder.eos enc os;
  out (Ogg.Stream.flush os);
  Unix.close fd;
  Printf.printf "Transcoding is finished..\n"

let () = Gc.full_major ()
