[@@@ocaml.warning "-6"]
open Wall
open Wall_text
open Pres_state
;;

let default_font ?(size=1.0) () =
  Font.make ~size:(64.0 *. size) font_sans
    ~placement:`Subpixel

let banner_height = 128.0
let title_banner = Path.make (fun ctx ->
    Path.rect ctx ~x:0.0 ~y:0.0 ~w:Slideshow.screen_width ~h:banner_height
  )

let b2 x y w h = Gg.Box2.v (Gg.P2.v x y) (Gg.Size2.v w h)
(* let body_box = b2 0.0 128.0 1024.0 640.0 *)
let body_box = b2 0.0 banner_height Slideshow.screen_width (Slideshow.screen_height -. banner_height)

let light_blue = Color.lerp_rgba 0.2 Color.white Color.blue
let light_yellow = Color.v 1.0 1.0 0.0 1.0

let title =
  let color = Paint.rgba 0.0 0.0 0.0 1.0 in
  let title_fg = Font.make ~size:96.0 font_sans in
  let title_bg = Font.make ~blur:6.0 ~size:96.0 font_sans in
  fun text content->
    [ Image.paint Paint.white (Image.fill mediabox);
      Image.paint background (Image.fill title_banner);
      Image.paint color (
        Image.seq [
          simple_text title_bg text
            ~x:514.0 ~y:18.0 ~halign:`CENTER ~valign:`TOP;
          simple_text title_fg text
            ~x:512.0 ~y:16.0 ~halign:`CENTER ~valign:`TOP;
        ]
      );
      Image.scissor body_box (Image.seq content)
    ]

let draw_arrow ~x ~y ~size color =
  Image.paint (Paint.color color)
    (Image.fill_path @@ fun t ->
     Path.move_to t x y;
     Path.line_to t (x-.size) (y+.size);
     Path.line_to t (x-.size) (y-.size);
     Path.close t;
    )

let text ?(halign=`LEFT) ?size ~x ~y str =
  simple_text ~x ~y ~halign ~valign:`BASELINE (default_font ?size ()) str

let text_arrow ~x ~y str =
  Image.seq [
    draw_arrow ~x ~y ~size:16.0 Color.blue;
    text ~x:(x +. 20.0) ~y:(y +. 14.0) str
  ]

let make_outlines a_title steps step =
  let render_step i (step', title) =
    let text = text_arrow ~x:220.0 ~y:(250.0 +. 100.0 *. float i) title in
    if step = step' then
      text
    else
      Image.alpha 0.5 text
  in
  let steps = List.mapi render_step steps in
  title a_title steps

let code fmt =
  let size = 36.0 in
  let interline = size +. 4.0 in
  let font = Font.make ~size font_mono in
  Printf.ksprintf
    (fun source ->
       let rec lines i =
         match String.index_from source i '\n' with
         | j -> String.sub source i (j - i) :: lines (j + 1)
         | exception Not_found -> [String.sub source i (String.length source - i)]
       in
       let lines = lines 0 in
       let h = 768.0 -. interline *. float (List.length lines) in
       Image.seq (
         List.mapi (fun i line ->
             Wall_text.simple_text font line
               ~halign:`LEFT ~valign:`BASELINE
               ~x:10.0 ~y:(h +. float i *. interline)
           ) lines
       ))
    fmt

let pf () f =
  if f < 0.0 then
    Printf.sprintf "(%.02f)" f
  else
    Printf.sprintf "%.02f" f

type outline = [
  | `Problem_solved
  | `Model
  | `API
  | `Execution
  | `Conclusion
]

type api_outline = [
  | `Path
  | `Shape
  | `Paint
  | `Transformation
  | `Composition
]

let outline : outline -> _ =
  make_outlines "Outline" [
    `Problem_solved , "The problem solved";
    `Model          , "Model";
    `API            , "API";
    `Execution      , "Execution";
    `Conclusion     , "Conclusion";
  ]

let api_outline : api_outline -> _ =
  make_outlines "API" [
    `Path           , "Path ([0,1] -> Point)";
    `Shape          , "Shape (Point -> Bool)";
    `Paint          , "Paint (Point -> Color)";
    `Transformation , "Transformation (Point -> Point)";
    `Composition    , "Composition (Image* -> Image)";
  ]

let shape_slide =
  let square = Path.make (fun ctx ->
      Path.rect ctx (-150.0) (-50.0) 100.0 100.0
    ) in
  let circle = Path.make (fun ctx ->
      Path.circle ctx 0.0 0.0 100.0
    ) in
  title "Shapes" [
    Image.transform (Transform.translation 512.0 450.0)
      (Image.seq [ Image.fill square; Image.fill circle ]);
    code "Point = R * R\n\
          Shape ~= Point -> Bool"
  ]
;;

let sample_path = Path.make (fun ctx ->
    Path.move_to ctx 0.0 0.0;
    Path.line_to ctx 50.0 0.0;
    Path.bezier_to ctx 100.0 (-30.0) 100.0 (-90.0) 50.0 (-60.0);
    Path.bezier_to ctx 60.0 (-120.0) (-20.0) (-120.0) 0.0 0.0;
  )
;;
open Slideshow;;







(* let modify stbimg =  *)
  (* let data = stbimg.Stb_image.data in
  { stbimg with Stb_image.data } *)

  (* stbimg *)
  (* {
    stbimg with
    data = 
      let data = stbimg.data in
      let data = Array.map (fun x -> x * 2) data in
  } *)

let scene _time (x,y) =
  (* (x,y,0.) *)
  if x <= 0.01 || x >= 0.99 || y <= 0.01 || y >= 0.99 then
    (0.,0.,0.)
  else if x < 0.1 || x > 0.9 || y < 0.1 || y>0.9 then
    (1.,0.,1.)
  else
    (x,y,0.)
  




let map_triple f (x,y,z) =
  (f x, f y, f z)
(* https://github.com/let-def/stb_image/blob/master/stb_image.mli *)
let create width height st =
    (* int8 buffer 
    int8 = (int, int8_unsigned_elt) kind 
    'k buffer = ('a, 'b, c_layout) Array1.t
    where 'k = ('a, 'b) kind 
    *)
  (* let width = 200 in
  let height = 200 in *)
  let scale = 2.0 in
  let awidth = int_of_float (float_of_int width /. scale) in
  let aheight = int_of_float (float_of_int height /. scale) in
  let render = 
    Array.init aheight (fun y ->
      let yf = float_of_int y /. float_of_int aheight in
      let yf = 1. -. yf in
      Array.init awidth (fun x ->
        let xf = float_of_int x /. float_of_int awidth in
        scene st (xf,yf)
        |> map_triple (fun a -> int_of_float (a *. 255.0))
      )
    )
  in
  let buffer =
    Bigarray.Array1.init
      Bigarray.int8_unsigned
      Bigarray.c_layout
      (width * height * 4)
      (fun i -> 
        let pos = i / 4 in
        let channel = i mod 4 in
        let x = pos / width in
        let y = pos mod width in
        let x = int_of_float (float_of_int x /. scale) in
        let y = int_of_float (float_of_int y /. scale) in
        let (r,g,b) = render.(x).(y) in
        match channel with
        | 0 -> r
        | 1 -> g
        | 2 -> b
        | 3 -> 255 (* alpha *)
        | _ -> assert false
      )
  in
  (* Stb_image.decode buffer *)
  Stb_image.image ~width ~height ~channels:4
    (* ~offset:0 ~stride:1 *)
    buffer

let img width height st =
  let last = ref None in
  fun t ->
  match !last with
    | Some (t', tex) when t == t' -> tex
    | _ ->
      let result = 
        (* let img = Stb_image.load "nyan_cat.png" in *)
        let img = create width height st in
        match img with
        | Result.Error (`Msg x) ->
          Printf.ksprintf prerr_endline "loading nyan_cat: %s" x;
          failwith "No image"
        | Result.Ok img -> 
          Printf.printf "created the image\n%!";
          (* let img = modify img in *)
          Wall.Texture.from_image t ~name:"render" img
      in
      last := Some (t, result);
      result
    ;;

Slideshow.set_slides Slideshow.window ([
  (
    (* let width,height = 512.,512. in *)
    let width,height = 800.,800. in
    let start = Sys.time () in
    (* move inside for animation *)
    let time = Sys.time () -. start in
    let img = img (int_of_float width) (int_of_float height) time in
    fun st ->
     let rect =
       Path.make (fun ctx -> Path.rect ctx 0. 0. width height)
     in
     let px,py = 500.,200. in
     title "Transformation: translation" [
       Image.paint
         (Paint.transform
            (Paint.image_pattern
               (* (Gg.V2.v (-1146.0/.4.0) (-700.0/.4.0)) *)
               (Gg.V2.v (0.0) (0.0))
               (* (Gg.V2.v (1146.0 /. 2.0) (696.0 /. 2.0)) *)
               (Gg.V2.v width height)
               ~angle:0.0
               ~alpha:1.0
               (img st.wall))
            (Transform.translation px py)
         ) 
        (Image.transform
          (Transform.translation px py)
                (Image.fill rect))
     ]);

  (fun _st ->
     let rect =
       Path.make (fun ctx -> Path.rect ctx (-200.0) (-200.0) 200.0 200.0)
     in
     title "Transformation: translation" [
       Image.transform
         (Transform.translation 512.0 444.0)
              (Image.fill rect)
        ;
     ]);
  (fun _st ->
     let rect =
       Path.make (fun ctx -> Path.rect ctx (-60.0) (-20.0) 120.0 40.0)
     in
     title "Transformation: translation" [
       Image.transform
         (Transform.translation 512.0 444.0)
              (Image.fill rect)
        ;
     ]);
  (fun _st ->
     let rect =
       Path.make (fun ctx -> Path.rect ctx (-60.0) (-20.0) 120.0 40.0)
     in
     title "Transformation: translation" [
       Image.transform
         (Transform.translation 512.0 444.0)
            (Image.paint
              (Paint.linear_gradient 0.0 0.0 30.0 30.0
                light_yellow light_blue)
              (Image.fill rect)
            ) ;
     ]);
  (fun st ->
    print_endline ("  " ^ string_of_float st.time);
     let rect =
       Path.make (fun ctx -> Path.rect ctx (-60.0) (-20.0) 120.0 40.0)
     in
     let sx = sin (st.time *. 2.0) *. 200.0 in
     let sy = cos (st.time *. 3.0) *. 200.0 in
     title "Transformation: translation" [
       Image.transform
         (Transform.translation 512.0 444.0)
         (Image.transform
            (Transform.translation sx sy)
            (Image.paint
              (Paint.linear_gradient 0.0 0.0 30.0 30.0
                light_yellow light_blue)
              (Image.fill rect)
            )
          );
       code "Image.transform (translation %a %a) rect" pf sx pf sy;
     ]);
])
;;








(* Slideshow.set_slides Slideshow.window ([
  (fun _ -> title "The Wall library"
      [
        text ~halign:`CENTER ~x:512.0 ~y:250.0 "Frédéric Bour";
        text ~halign:`CENTER ~x:512.0 ~y:350.0 "OCaml Workshop 2018";
        text ~halign:`CENTER ~x:512.0 ~y:450.0 "Thursday, September 27";
        text_arrow ~x:152.0 ~y:600.0 "Graphics";
        text_arrow ~x:412.0 ~y:600.0 "in OCaml";
        text_arrow ~x:672.0 ~y:600.0 "with fun";
      ]
  );
  (fun _ -> outline `Problem_solved);
  (fun _ -> title "Key points"
      [
        text ~x:80.0 ~y:280.0 "1) Declarative vector graphics";
        text ~x:80.0 ~y:450.0 "2) Rendered with OpenGL";
        text ~x:80.0 ~y:620.0 "3) Independent from a windowing system";
      ]
  );
  (fun _ -> title "Tradeoffs"
      [
        text_arrow ~x:100.0 ~y:200.0 "Made for user interfaces";
        text_arrow ~x:100.0 ~y:300.0 "Goals:";
        text ~x:140.0 ~y:380.0 "(1) High output quality";
        text ~x:140.0 ~y:440.0 "(2) Performance";
        text ~x:140.0 ~y:500.0 "(3) Simplicity";
        text_arrow ~x:100.0 ~y:580.0 "Non-goal: expressivity";
      ]
  );
  (fun _ -> outline `Model);
  (fun _ -> shape_slide);
  (fun st ->
     let paint_slide =
       let rect = Path.make (fun ctx ->
           Path.rect ctx 0.0 128.0 1024.0 640.0
         ) in
       title "Paint" [
         Image.paint (Paint.color (Color.v_srgb 1.0 1.0 0.0))
           (Image.fill rect);
         code "Color = [0,1]^4\n\
               Paint ~= Point -> Color"
       ]
     in
     if st.time > 0.25
     then paint_slide
     else
       let progress = st.time /. 0.25 in
       [
         Image.transform
           (Transform.translation (-. progress *. 1024.0) 0.0)
           (Image.seq shape_slide);
         Image.transform
           (Transform.translation ((1.0 -. progress) *. 1024.0) 0.0)
           (Image.seq paint_slide);
       ]
     );
  (fun st ->
     let circle = Path.make (fun ctx ->
         Path.circle ctx 0.0 0.0 100.0
       ) in
     let paint = Paint.color (Color.v_srgb 1.0 1.0 0.0) in
     title "Painted shapes" [
       Image.transform (Transform.translation 512.0 444.0)
         (Image.transform
            (let f = max (10.0 *. (1.0 -. st.time)) 1.0 in Transform.scale f f)
            (Image.paint paint (Image.fill circle)));
       code "Image ~= Point -> Color\n\
             primitive(shape, paint) ~=\n\
            \  fun point -> if shape point then paint point\n\
            \                              else zero"
     ]);
  (fun st ->
     let circle = Path.make (fun ctx ->
         Path.circle ctx 0.0 0.0 100.0
       ) in
     let paint = Paint.color (Color.v_srgb 1.0 1.0 0.0) in
     title "Transform" [
       Image.transform (Transform.translation 512.0 444.0)
         (Image.transform
            (let f = (1.0 +. min st.time 1.0) in
             Transform.rescale ~sx:(1.0 +. f /. 4.0) ~sy:f
               (Transform.translation ~x:0.0 ~y:(f *. -30.0)))
            (Image.paint paint (Image.fill circle)));
       code "Transformation ~= Point -> Point\n\
             transform(image, t) ~=\n\
            \  fun point -> image(t(point))"
     ]);
  (fun _st ->
     let outline = Outline.make ~cap:`ROUND ~width:10.0 () in
     let circle = Path.make (fun ctx ->
         Path.circle ctx 0.0 0.0 100.0
       ) in
     let eye =
       let contour = Path.make (fun ctx ->
           Path.circle ctx 0.0 0.0 30.0
         ) in
       let dot = Path.make (fun ctx ->
           Path.circle ctx 0.0 0.0 10.0
         ) in
       Image.seq [
         Image.paint Paint.white (Image.fill contour);
         Image.stroke outline contour;
         Image.fill dot;
       ]
     in
     let smile =
       let path = Path.make (fun ctx ->
           Path.move_to ctx (-50.0) 0.0;
           Path.bezier_to ctx (-30.0) 30.0 30.0 30.0 50.0 0.0;
         )
       in
       Image.stroke outline path
     in
     let base = Color.v_srgb 1.0 1.0 0.0 in
     let paint =
       Paint.linear_gradient (-100.0) 0.0 350.0 300.0 base Color.black
     in
     title "Repeat!" [
       Image.transform (Transform.translation 512.0 384.0) (Image.seq [
           Image.transform
             (Transform.scale 1.5 2.0)
             (Image.paint paint (Image.fill circle));
           Image.transform (Transform.translation (-60.0) (-90.0)) eye;
           Image.transform (Transform.translation (60.0) (-90.0)) eye;
           Image.transform (Transform.translation 0.0 10.0) smile;
         ]);
       code "blend(image_0, image_1, ...)"
     ]);
  (fun _ -> outline `API);
  (fun _ -> api_outline `Path);
  (fun _ ->
     let p = Path.make (fun ctx ->
         Path.move_to ctx 0.0 0.0;
       )
     in
     title "Path" [
       Image.transform
         (Transform.rescale 2.0 2.0 (Transform.translation 512.0 444.0))
         (Image.stroke Outline.default p);
       code "let path = Path.make (fun ctx ->\n\
            \  \n\
            \  \n\
            \  \n\
            \  \n\
             )"
     ]);
  (fun _ ->
     let p = Path.make (fun ctx ->
         Path.move_to ctx 0.0 0.0;
       )
     in
     title "Path" [
       Image.transform
         (Transform.rescale 2.0 2.0 (Transform.translation 512.0 444.0))
         (Image.stroke Outline.default p);
       code "let path = Path.make (fun ctx ->\n\
            \  Path.move_to ctx 0. 0.;\n\
            \  \n\
            \  \n\
            \  \n\
             )"
     ]);
  (fun _ ->
     let p = Path.make (fun ctx ->
         Path.move_to ctx 0.0 0.0;
         Path.line_to ctx 50.0 0.0;
       )
     in
     title "Path" [
       Image.transform
         (Transform.rescale 2.0 2.0 (Transform.translation 512.0 444.0))
         (Image.stroke Outline.default p);
       code "let path = Path.make (fun ctx ->\n\
            \  Path.move_to ctx 0. 0.;\n\
            \  Path.line_to ctx 50. 0.;\n\
            \  \n\
            \  \n\
             )"
     ]);
  (fun _ ->
     let p = Path.make (fun ctx ->
         Path.move_to ctx 0.0 0.0;
         Path.line_to ctx 50.0 0.0;
         Path.bezier_to ctx 100.0 (-30.0) 100.0 (-90.0) 50.0 (-60.0);
       )
     in
     title "Path" [
       Image.transform
         (Transform.rescale 2.0 2.0 (Transform.translation 512.0 444.0))
         (Image.stroke Outline.default p);
       code "let path = Path.make (fun ctx ->\n\
            \  Path.move_to ctx 0. 0.;\n\
            \  Path.line_to ctx 50. 0.;\n\
            \  Path.bezier_to ctx 10. (-3.) 10. (-9.) 5. (-6.);\n\
            \  \n\
             )"
     ]);
  (fun _ ->
     title "Path" [
       Image.transform
         (Transform.rescale 2.0 2.0 (Transform.translation 512.0 444.0))
         (Image.stroke Outline.default sample_path);
       code "let path = Path.make (fun ctx ->\n\
            \  Path.move_to ctx 0. 0.;\n\
            \  Path.line_to ctx 50. 0.;\n\
            \  Path.bezier_to ctx 10. (-3.) 10. (-9.) 5. (-6.);\n\
            \  Path.bezier_to ctx 6. (-12.) (-2.) (-12.) 0. 0.;\n\
             )"
     ]);
  (fun _ -> api_outline `Shape);
  (fun _ ->
     title "Filling path" [
       Image.transform
         (Transform.rescale 2.0 2.0 (Transform.translation 512.0 444.0))
         (Image.fill sample_path);
       code "Image.fill path";
     ]);
  (fun st ->
     let width = (sin st.time *. sin st.time) *. 10.0 in
     title "Stroking path" [
       Image.transform
         (Transform.rescale 2.0 2.0 (Transform.translation 512.0 444.0))
         (Image.stroke (Outline.make ~width ()) sample_path);
       code "Image.stroke (Outline.make ~width:%a ()) path" pf width;
     ]);
  (fun _ ->
     title "Glyphes & Masks" [
       text_arrow ~x:100.0 ~y:200.0 "For complex or external shapes:";
       text ~x:150.0 ~y:280.0 "use a bitmap as a discrete";
       text ~x:150.0 ~y:340.0 "approximation of (Point -> Bool)";
       text ~x:150.0 ~y:400.0 "([0..n] * [0..m] -> [0..255])";
       text_arrow ~x:100.0 ~y:460.0 "Adjust resolution dynamically";
       text_arrow ~x:100.0 ~y:540.0 "Abstraction for laying out glyphes";
       code "type 'a typesetter\n\
             Image.typeset : 'a typesetter -> 'a -> image";
     ]);
  (fun _ -> api_outline `Paint);
  (fun _ ->
     title "Paint: color" [
       Image.paint (Paint.color light_yellow)
         (Image.fill mediabox);
       code "Paint.color light_yellowred"
     ]);
  (fun st ->
     let t = st.time /. 4.0 in
     let s = sin (t *. 2.0) and c = cos (t *. 3.0) in
     let sx = 24.0 +. abs_float s *. 200.0 in
     let ex = 1000.0 -. abs_float c *. 200.0 in
     title "Paint: linear" [
       Image.paint (Paint.linear_gradient sx 0.0 ex 0.0
                      light_yellow light_blue)
         (Image.fill mediabox);
       code "Paint.linear_gradient %a 0.0 %a 0.0" pf sx pf ex
     ]);
  (fun st ->
     let t = st.time /. 4.0 in
     let s = sin (t *. 2.0) and c = cos (t *. 3.0) in
     let r = abs_float (s *. 100.0) in
     let f = abs_float (c *. 50.0) in
     title "Paint: box" [
       Image.paint (Paint.box_gradient 160.0 228.0 704.0 360.0 r f
                      light_yellow light_blue)
         (Image.fill mediabox);
       code "Paint.box_gradient ~x ~y ~w ~h\n\
            \  ~r:%a ~f:%a light_yellow light_blue" pf r pf f
     ]);
  (fun st ->
     let t = st.time /. 4.0 in
     let s = sin (t *. 2.0) and c = cos (t *. 3.0) in
     let inner = s *. s *. 200.0 in
     let outer = inner +. c *. c *. 100.0 in
     title "Paint: radial" [
       Image.paint (Paint.radial_gradient 512.0 444.0 inner outer
                      light_yellow light_blue)
         (Image.fill mediabox);
       code "Paint.radial_gradient ~cx ~cy\n\
            \  ~inner:%a ~outer:%a" pf inner pf outer;
     ]);
  (fun st ->
     let angle = sin st.time /. 2.0 in
     let alpha = 0.5 +. abs_float (sin (st.time *. 4.0)) /. 2.0 in
     title "Paint: pixmaps" [
       Image.paint
         (Paint.transform
            (Paint.image_pattern
               (Gg.V2.v (-1146.0/.4.0) (-700.0/.4.0))
               (Gg.V2.v (1146.0 /. 2.0) (696.0 /. 2.0))
               ~angle
               ~alpha
               (Pres_state.nyan_cat st.wall))
            (Transform.translation 500.0 400.0)
         ) (Image.fill mediabox);
       code "Paint.image_pattern (x,y) (w,h) \n\
            \   ~angle:%.02f ~alpha:%a nyan_cat"
            (mod_float angle (pi *. 2.0)) pf alpha;
     ]);
  (fun _ -> api_outline `Transformation);
  (fun st ->
     let rect =
       Path.make (fun ctx -> Path.rect ctx (-60.0) (-20.0) 120.0 40.0)
     in
     let sx = sin (st.time *. 2.0) *. 200.0 in
     let sy = cos (st.time *. 3.0) *. 200.0 in
     title "Transformation: translation" [
       Image.transform
         (Transform.translation 512.0 444.0)
         (Image.transform
            (Transform.translation sx sy)
            (Image.fill rect));
       code "Image.transform (translation %a %a) rect" pf sx pf sy;
     ]);
  (fun st ->
     let rect =
       Path.make (fun ctx -> Path.rect ctx (-60.0) (-20.0) 120.0 40.0)
     in
     let a = st.time *. 2.0 in
     title "Transformation: rotation" [
       Image.transform
         (Transform.translation 512.0 444.0)
         (Image.transform
            (Transform.rotation a)
            (Image.fill rect));
       code "Image.transform (rotation %a) rect" pf a;
     ]);
  (fun st ->
     let rect =
       Path.make (fun ctx -> Path.rect ctx (-60.0) (-20.0) 120.0 40.0)
     in
     let sx = 1.0 +. sin (st.time *. 2.0) *. 2.0 in
     let sy = 1.0 +. cos (st.time *. 3.0) *. 4.0 in
     title "Transformation: scaling" [
       Image.transform
         (Transform.translation 512.0 444.0)
         (Image.transform (Transform.scale sx sy) (Image.fill rect));
       code "Image.transform (scale %a %a) rect" pf sx pf sy;
     ]);
  (fun st ->
     let rect =
       Path.make (fun ctx -> Path.rect ctx (-60.0) (-20.0) 120.0 40.0)
     in
     let sx = sin (st.time *. 2.0) in
     let sy = cos (st.time *. 3.0) in
     title "Transformation: skewing" [
       Image.transform
         (Transform.translation 512.0 444.0)
         (Image.transform (Transform.skew sx sy) (Image.fill rect));
       code "Image.transform (skew %a %a) rect" pf sx pf sy;
     ]);
  (fun _ -> api_outline `Composition);
  (fun _st ->
     let rect =
       Path.make (fun ctx -> Path.rect ctx (-120.0) (-120.0) 240.0 240.0)
     in
     title "Composition: paint" [
       Image.transform
         (Transform.translation 512.0 444.0)
         (Image.paint
            (Paint.linear_gradient (-120.0) (-120.0) 120.0 120.0
                      light_yellow light_blue)
            (Image.fill rect));
       code "Image.paint linear_gradient square";
     ]);
  (fun st ->
     let rect =
       Path.make (fun ctx -> Path.rect ctx (-120.0) (-120.0) 240.0 240.0)
     in
     let circle =
       Path.make (fun ctx -> Path.circle ctx 0.0 0.0 120.0)
     in
     let t = st.time *. 3.0 in
     let y = 200.0 *. sin t in
     let a = t /. 10.0 in
     title "Composition: superposition" [
       Image.transform
         (Transform.translation 512.0 444.0)
         (Image.seq [
             Image.paint
               (Paint.linear_gradient (-120.0) (-120.0) 120.0 120.0
                  light_yellow light_blue)
               (Image.fill rect);
             Image.transform
               (Transform.translate 0.0 y (Transform.rotation a))
               (Image.paint
                  (Paint.radial_gradient 0.0 0.0 20.0 180.0
                     light_yellow light_blue)
                  (Image.fill circle));
           ]);
       code
         "Image.superpose square\n\
         \  (moving_circle ~offset:%a ~angle:%a)"
         pf y pf a
     ]);
  (fun st ->
     let circle =
       Path.make (fun ctx -> Path.circle ctx 0.0 0.0 120.0)
     in
     let t = st.time *. 3.0 in
     let y = 200.0 *. sin t in
     let a = t /. 10.0 in
     title "Composition: scissor" [
       Image.transform
         (Transform.translation 512.0 444.0)
         (Image.seq [
             Image.scissor (b2 (-120.0) (-120.0) 240.0 240.0)
               (Image.transform
                  (Transform.translate 0.0 y
                     (Transform.rotation a))
                  (Image.paint
                     (Paint.radial_gradient 0.0 0.0 20.0 180.0
                        light_yellow light_blue)
                     (Image.fill circle)))
           ]);
       code
         "Image.scissor box\n\
         \  (moving_circle ~offset:%a ~angle:%a)"
         pf y pf a
     ]);
  (fun st ->
     let rect =
       Path.make (fun ctx -> Path.rect ctx (-120.0) (-120.0) 240.0 240.0)
     in
     let circle =
       Path.make (fun ctx -> Path.circle ctx 0.0 0.0 120.0)
     in
     let v_alpha = (0.5 +. sin st.time /. 2.0) in
     title "Composition: alpha" [
       Image.transform
         (Transform.translation 512.0 444.0)
         (Image.seq [
             Image.paint
               (Paint.linear_gradient (-120.0) (-120.0) 120.0 120.0
                  light_yellow light_blue)
               (Image.fill rect);
             Image.alpha v_alpha
               (Image.paint
                  (Paint.radial_gradient 0.0 0.0 20.0 180.0
                     light_yellow light_blue)
                  (Image.fill circle));
           ]);
       code "Image.superpose square (Image.alpha %a circle)" pf v_alpha
     ]);
  (fun _ -> outline `Execution)
] @ Tree_slides.slides @ [
  (fun _ -> outline `Conclusion);
  (fun _ -> title "Current state"
      [
        text_arrow ~x:100.0 ~y:200.0 "Versatile vector graphics renderer";
        text_arrow ~x:100.0 ~y:300.0 "Declarative API";
        text_arrow ~x:100.0 ~y:400.0 "Attention to performance and portability";
        text_arrow ~x:100.0 ~y:500.0 "Codebase understandable by 1 person";
      ]
  );
  (fun _ -> title "Future work"
      [
        text_arrow ~x:100.0 ~y:200.0 "(maybe) a video game rendering path";
        text_arrow ~x:100.0 ~y:300.0 "Cleanup, tweak performance...";
        text_arrow ~x:100.0 ~y:400.0 "Build the rest of a graphic stack";
      ]
  );
  (fun _ -> title "Acknowledgements"
      [
        text ~x:100.0 ~y:250.0 "Thanks to Pierre Chambart,";
        text ~x:110.0 ~y:350.0 "Sébastien Mondet and Jordan Walke.";
        text ~x:100.0 ~y:550.0 "Thanks to Mikko Mononen for NanoVG,";
        text ~x:110.0 ~y:650.0 "the original engine behind Wall.";
      ]
  );
  (fun st -> title "Acknowledgements"
      [
        text ~x:100.0 ~y:450.0 "And one last thing...";
        Image.transform (Transform.rotate st.time (Transform.translation 100.0 250.0)) (
        Image.seq (
          if true then [
            text ~x:0.0 ~y:(-20.0) "Thanks for your attention";
            text ~x:0.0 ~y:(20.0) "Live coding is cool";
          ] else []
        ));
        text ~x:100.0 ~y:650.0 "Ph'nglui mglw'nafh";
        text ~x:100.0 ~y:710.0 "Cthulhu R'lyeh wgah'nagl fhtagn!";
      ]
  )]
  )
;;

let () = print_endline "foo" *)
