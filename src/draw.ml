open Roomdiv

let create_canvas (x, y) =
  let surface = Cairo.Image.create Cairo.Image.ARGB32 ~w:x ~h:y in
  surface

let draw_canvas cr =
  Cairo.set_source_rgb cr 1. 1. 1.;
  Cairo.paint cr;
  cr

(** [draw_line cr (x,y) (a,b) lineweight] draws a line from (x,y) to (a,b) 
with the line of [lineweight] width in the [cr]. Used as helper for draw_rooms *)
let draw_line cr (x, y) (a, b) lineweight =
  Cairo.set_source_rgb cr 0. 0. 0.;
  Cairo.set_line_width cr lineweight;
  Cairo.move_to cr x y;
  Cairo.line_to cr a b;
  Cairo.stroke cr;
  cr

let draw_rectangle cr (x, y) (w, h) lineweight =
  Cairo.set_source_rgba cr 0. 0. 0. 1.;
  Cairo.set_line_width cr lineweight;
  Cairo.rectangle cr x y ~w ~h;
  Cairo.stroke cr;
  cr

let draw_door_down cr (x, y) =
  Cairo.set_source_rgb cr 1. 1. 1.;
  for _ = 1 to 5 do
    Cairo.move_to cr x y;
    Cairo.rel_line_to cr 20. 0.;
    Cairo.set_line_width cr 4.;
    Cairo.stroke cr
  done;
  Cairo.set_source_rgb cr 0. 0. 0.;
  Cairo.set_dash cr [| 0.1 |];
  let pi = 4.0 *. atan 1.0 in
  Cairo.arc cr x y ~r:20. ~a1:0. ~a2:(pi /. 2.);
  Cairo.set_line_width cr 0.7;
  Cairo.stroke cr;
  Cairo.set_dash cr [||];
  Cairo.move_to cr x (y +. 20.);
  Cairo.rel_line_to cr 0. (-20.);
  Cairo.stroke cr;
  cr

let draw_door_right cr (x, y) =
  Cairo.set_source_rgb cr 1. 1. 1.;
  for _ = 1 to 5 do
    Cairo.move_to cr x y;
    Cairo.rel_line_to cr 0. 20.;
    Cairo.set_line_width cr 4.;
    Cairo.stroke cr
  done;
  Cairo.set_source_rgb cr 0. 0. 0.;
  Cairo.set_dash cr [| 0.1 |];
  Cairo.move_to cr (x +. 20.) y;
  let pi = 4.0 *. atan 1.0 in
  Cairo.arc cr x y ~r:20. ~a1:0. ~a2:(pi /. 2.);
  Cairo.set_line_width cr 0.7;
  Cairo.stroke cr;
  Cairo.set_dash cr [||];
  Cairo.move_to cr (x +. 20.) y;
  Cairo.rel_line_to cr (-20.) 0.;
  Cairo.stroke cr;
  cr

(** [realign roomcoor] takes a int point and increases its x and y coordinates by 200, and returns a float point.
    Used as helper for draw_rooms and draw_columns *)
let realign (roomcoor : int * int) =
  match roomcoor with x, y -> (float_of_int x +. 200., float_of_int y +. 200.)

let rec draw_rooms (rmlst : room list) cr lineweight =
  match rmlst with
  | [] -> cr
  | rm :: t -> (
      match (rm.grline, rm.dir) with
      | ((a, b), (a', b')), str ->
          let _ = draw_line cr (realign (a, b)) (realign (a', b')) lineweight in
          let _ =
            if str = "h" then draw_door_down cr (realign (a + 15, b))
            else draw_door_right cr (realign (a, b + 15))
          in
          draw_rooms t cr lineweight)

let rec draw_columns (columnlst : (int * int) list) cr lineweight =
  match columnlst with
  | [] -> cr
  | (a, b) :: t ->
      let draw_current =
        draw_rectangle cr (realign (a, b)) (3., 3.) lineweight
      in
      draw_columns t draw_current lineweight

let cover_white cr (x, y) (a, b) =
  Cairo.set_source_rgb cr 1. 1. 1.;
  for _ = 1 to 5 do
    Cairo.set_line_width cr 2.;
    Cairo.move_to cr x y;
    Cairo.line_to cr a b;
    Cairo.stroke cr
  done;
  cr

let draw_windows_rooms cr (house : wholeroom) =
  let width = house.width in
  let height = house.height in
  let numw = width / 100 in
  let numh = height / 100 in
  for i = 1 to numh do
    let _ =
      cover_white cr
        (realign (0, (100 * (i - 1)) + 10))
        (realign (0, (100 * (i - 1)) + 40))
    in
    let _ =
      cover_white cr
        (realign (width, (100 * (i - 1)) + 10))
        (realign (width, (100 * (i - 1)) + 40))
    in
    ()
  done;
  for i = 2 to numw do
    let _ =
      cover_white cr
        (realign ((100 * (i - 1)) + 10, 0))
        (realign ((100 * (i - 1)) + 40, 0))
    in
    let _ =
      cover_white cr
        (realign ((100 * (i - 1)) + 10, height))
        (realign ((100 * (i - 1)) + 40, height))
    in
    ()
  done;
  cr

let draw_structural_gird cr wholeroom =
  Cairo.set_source_rgb cr 0.5 0.5 0.5;
  Cairo.set_line_width cr 1.;
  Cairo.set_dash cr [| 3. |];
  let width = wholeroom.width in
  let height = wholeroom.height in
  let w = (width / 100) - 1 in
  let h = (height / 100) - 1 in
  for i = 1 to w do
    Cairo.move_to cr ((float_of_int i +. 2.01) *. 100.) 100.;
    Cairo.rel_line_to cr 0. (float_of_int (height + 200));
    Cairo.stroke cr
  done;
  for i = 1 to h do
    Cairo.move_to cr 100. ((float_of_int i +. 2.01) *. 100.);
    Cairo.rel_line_to cr (float_of_int (width + 200)) 0.;
    Cairo.stroke cr
  done;
  Cairo.set_dash cr [||];
  cr

let dim cr wr lst h =
  Cairo.set_source_rgb cr 0. 0. 0.;
  Cairo.set_line_width cr 1.;

  Cairo.move_to cr 190. (float_of_int (wr.height + 250));
  Cairo.rel_line_to cr (float_of_int (wr.width + 20)) 0.;
  Cairo.stroke cr;

  Cairo.move_to cr (float_of_int (wr.width + 250)) 190.;
  Cairo.rel_line_to cr 0. (float_of_int (wr.height + 20));
  Cairo.stroke cr;

  Cairo.move_to cr (float_of_int (wr.width + 243)) 200.;
  Cairo.rel_line_to cr 15. 0.;
  Cairo.stroke cr;

  Cairo.move_to cr 200. (float_of_int (wr.height + 243));
  Cairo.rel_line_to cr 0. 15.;
  Cairo.stroke cr;

  Cairo.select_font_face cr "Sans" ~weight:Normal ~slant:Upright;
  Cairo.set_font_size cr 8.;

  let rec help lst =
    match lst with
    | [] -> ()
    | { tr = _, y2; bl = x2, _; br = x3, y3; dir; _ } :: t ->
        if dir = "w" then (
          Cairo.move_to cr (float_of_int (x3 + 200)) (float_of_int (y3 + 243));
          Cairo.rel_line_to cr 0. 15.;
          Cairo.stroke cr;
          Cairo.move_to cr
            (float_of_int (((x3 - x2) / 2) + x2 + 195))
            (float_of_int (y3 + 243));
          Cairo.show_text cr (string_of_int (x3 - x2));
          help t)
        else (
          Cairo.move_to cr (float_of_int (x3 + 243)) (float_of_int (y3 + 200));
          Cairo.rel_line_to cr 15. 0.;
          Cairo.stroke cr;

          Cairo.move_to cr
            (float_of_int (x3 + 235))
            (float_of_int (((y3 - y2) / 2) + y2 + 195));

          Cairo.show_text cr (string_of_int (y3 - y2));

          help t)
  in
  help lst;
  Cairo.move_to cr
    (200. +. float_of_int wr.width)
    (float_of_int (wr.height + 243));
  Cairo.rel_line_to cr 0. 15.;
  Cairo.stroke cr;
  Cairo.move_to cr
    (float_of_int (wr.width + 243))
    (float_of_int (wr.height + 200));
  Cairo.rel_line_to cr 15. 0.;
  Cairo.stroke cr;

  (match h with
  | { tr = _, y1; bl = x1, _; br = x2, y2; _ } ->
      Cairo.move_to cr
        (float_of_int (((x2 - x1) / 2) + x1 + 195))
        (float_of_int (y2 + 243));
      Cairo.show_text cr (string_of_int (x2 - x1));
      Cairo.move_to cr
        (float_of_int (x2 + 235))
        (float_of_int (((y2 - y1) / 2) + y1 + 195));
      Cairo.show_text cr (string_of_int (y2 - y1)));

  Cairo.move_to cr
    (float_of_int ((wr.width / 2) + 200))
    (float_of_int (wr.height + 270));
  Cairo.show_text cr (string_of_int wr.width);

  Cairo.move_to cr
    (float_of_int (wr.width + 260))
    (float_of_int ((wr.height / 2) + 200));
  Cairo.show_text cr (string_of_int wr.height);
  cr

let label_structure cr wholeroom =
  Cairo.set_source_rgb cr 0.5 0.5 0.5;

  let width = wholeroom.width in
  let height = wholeroom.height in
  let w = (width / 100) - 1 in
  let h = (height / 100) - 1 in
  let pi = 4.0 *. atan 1.0 in

  for i = 1 to w do
    Cairo.Path.sub cr;

    Cairo.arc cr
      ((float_of_int i +. 2.01) *. 100.)
      (float_of_int (height + 320))
      ~r:10. ~a1:0. ~a2:(2. *. pi);
    Cairo.set_line_width cr 1.;
    Cairo.stroke cr
  done;

  for i = 1 to h do
    Cairo.Path.sub cr;

    Cairo.arc cr
      (float_of_int (width + 320))
      ((float_of_int i +. 2.01) *. 100.)
      ~r:10. ~a1:0. ~a2:(2. *. pi);
    Cairo.set_line_width cr 1.;
    Cairo.stroke cr
  done;

  Cairo.select_font_face cr "Sans" ~weight:Normal ~slant:Upright;
  Cairo.set_font_size cr 8.;
  for i = 1 to w do
    Cairo.move_to cr
      ((float_of_int i +. 2.) *. 100.)
      (float_of_int (height + 323));
    Cairo.show_text cr (string_of_int i)
  done;
  let s = "ABCDEFGHIJK" in
  for i = 1 to h do
    Cairo.move_to cr
      (float_of_int (width + 318))
      ((float_of_int i +. 2.02) *. 100.);
    Cairo.show_text cr (String.make 1 s.[i - 1])
  done;
  cr

let draw_couch cr pos_x pos_y =
  let width = 100.0 in
  let height = 40.0 in
  let lineweight = 2.0 in
  let updated_cr =
    draw_rectangle cr (pos_x, pos_y) (width, height) lineweight
  in
  let armrest_width = 10.0 in
  let armrest_height = height /. 2.0 in
  let armrest_x = pos_x -. armrest_width in
  let armrest_y = pos_y +. (height /. 4.0) in
  let updated_cr =
    draw_rectangle updated_cr (armrest_x, armrest_y)
      (armrest_width, armrest_height)
      lineweight
  in
  let armrest_x = pos_x +. width in
  let updated_cr =
    draw_rectangle updated_cr (armrest_x, armrest_y)
      (armrest_width, armrest_height)
      lineweight
  in
  Cairo.set_source_rgb updated_cr 0.5 0.5 0.5;
  Cairo.fill updated_cr;
  Cairo.set_source_rgb updated_cr 0.0 0.0 0.0;
  Cairo.set_font_size updated_cr 12.0;
  Cairo.move_to updated_cr (pos_x +. 20.0) (pos_y +. height +. 20.0);
  Cairo.show_text updated_cr "Couch";
  Cairo.stroke updated_cr;
  updated_cr

let draw_bed cr pos_x pos_y =
  let width = 150.0 in
  let height = 90.0 in
  let bed_width = width *. 0.7 in
  let bed_x = pos_x +. ((width -. bed_width) /. 2.0) in
  let lineweight = 2.0 in
  let updated_cr =
    draw_rectangle cr (bed_x, pos_y) (bed_width, height) lineweight
  in
  let headboard_height = 30.0 in
  let headboard_y = pos_y -. headboard_height in
  let _ =
    draw_rectangle updated_cr (bed_x, headboard_y)
      (bed_width, headboard_height)
      lineweight
  in
  let mattress_height = height -. headboard_height in
  let mattress_y = pos_y +. headboard_height in
  let _ =
    draw_rectangle updated_cr (bed_x, mattress_y)
      (bed_width, mattress_height)
      lineweight
  in
  Cairo.set_source_rgb updated_cr 0.0 0.0 0.0;
  Cairo.set_font_size updated_cr 12.0;
  Cairo.move_to updated_cr (bed_x +. 20.0) (pos_y +. height +. 20.0);
  Cairo.show_text updated_cr "Bed";
  Cairo.stroke updated_cr;
  updated_cr

let draw_table cr pos_x pos_y =
  let width = 100.0 in
  let height = 60.0 in
  let lineweight = 2.0 in
  let updated_cr =
    draw_rectangle cr (pos_x, pos_y) (width, height) lineweight
  in
  Cairo.set_source_rgb updated_cr 0.5 0.5 0.5;
  Cairo.fill updated_cr;
  Cairo.set_source_rgb updated_cr 0.0 0.0 0.0;
  Cairo.set_font_size updated_cr 12.0;
  Cairo.move_to updated_cr (pos_x +. 20.0) (pos_y +. height +. 20.0);
  Cairo.show_text updated_cr "Table";
  Cairo.stroke updated_cr;
  updated_cr

let draw_sink cr x y =
  let width = 50.0 in
  let height = 20.0 in
  let lineweight = 2.0 in
  let updated_cr = draw_rectangle cr (x, y) (width, height) lineweight in
  Cairo.set_source_rgb updated_cr 0.8 0.8 0.8;
  Cairo.fill updated_cr;
  Cairo.set_source_rgb updated_cr 0.0 0.0 0.0;
  Cairo.set_font_size updated_cr 12.0;
  Cairo.move_to updated_cr (x +. 20.0) (y +. height +. 20.0);
  Cairo.show_text updated_cr "Sink";
  Cairo.stroke updated_cr;
  let smaller_width = width *. 0.7 in
  let smaller_height = height *. 0.7 in
  let smaller_x = x +. ((width -. smaller_width) /. 2.0) in
  let smaller_y = y +. ((height -. smaller_height) /. 2.0) in
  let _ =
    draw_rectangle updated_cr (smaller_x, smaller_y)
      (smaller_width, smaller_height)
      lineweight
  in
  let knob_size = 3.0 in
  let knob_x1 = x +. ((width -. knob_size) /. 2.0) -. (knob_size +. 5.0) in
  let knob_x2 = x +. ((width -. knob_size) /. 2.0) +. 5.0 in
  let knob_y = y +. 10.0 in
  let _ =
    draw_rectangle updated_cr (knob_x1, knob_y) (knob_size, knob_size)
      lineweight
  in
  let _ =
    draw_rectangle updated_cr (knob_x2, knob_y) (knob_size, knob_size)
      lineweight
  in
  updated_cr

let draw_three_seated_couch cr x y =
  let couch_width = 100. in
  let couch_height = 55. in
  let lineweight = 2.0 in

  let updated_cr =
    draw_rectangle cr (x, y) (couch_width, couch_height) lineweight
  in

  let seat_width = couch_width /. 3.0 in
  let seat_height = couch_height -. 20.0 in
  let seat1_x = x in
  let seat1_y = y +. 10.0 in
  let _ =
    draw_rectangle updated_cr (seat1_x, seat1_y) (seat_width, seat_height)
      lineweight
  in
  let seat2_x = x +. seat_width in
  let seat2_y = y +. 10.0 in
  let _ =
    draw_rectangle updated_cr (seat2_x, seat2_y) (seat_width, seat_height)
      lineweight
  in
  let seat3_x = x +. (2.0 *. seat_width) in
  let seat3_y = y +. 10.0 in
  let _ =
    draw_rectangle updated_cr (seat3_x, seat3_y) (seat_width, seat_height)
      lineweight
  in

  let armrest_width = 10.0 in
  let armrest_height = couch_height -. 20.0 in
  let armrest1_x = x -. armrest_width in
  let armrest1_y = y in
  let _ =
    draw_rectangle updated_cr (armrest1_x, armrest1_y)
      (armrest_width, armrest_height)
      lineweight
  in
  let armrest2_x = x +. couch_width in
  let armrest2_y = y in
  let _ =
    draw_rectangle updated_cr (armrest2_x, armrest2_y)
      (armrest_width, armrest_height)
      lineweight
  in

  Cairo.set_source_rgb updated_cr 0.5 0.5 0.5;
  Cairo.fill updated_cr;

  Cairo.set_source_rgb updated_cr 0.0 0.0 0.0;
  Cairo.set_font_size updated_cr 12.0;
  Cairo.move_to updated_cr (x +. 20.0) (y +. couch_height +. 20.0);
  Cairo.show_text updated_cr "Three-Seated Couch";
  Cairo.stroke updated_cr;

  updated_cr

let draw_round_table_with_chairs ctx x y =
  Cairo.set_source_rgb ctx 0. 0. 0.;
  Cairo.set_line_width ctx 2.;

  let table_radius = 50. in
  let num_sides = 100. in

  let angle = 2. *. 3.14159 /. num_sides in

  let center_x = x in
  let center_y = y in

  let rec draw_table_sides i =
    if i <= num_sides then (
      let start_x = center_x +. (table_radius *. cos (i *. angle)) in
      let start_y = center_y +. (table_radius *. sin (i *. angle)) in
      let end_x = center_x +. (table_radius *. cos ((i +. 1.) *. angle)) in
      let end_y = center_y +. (table_radius *. sin ((i +. 1.) *. angle)) in

      Cairo.move_to ctx start_x start_y;
      Cairo.line_to ctx end_x end_y;

      draw_table_sides (i +. 1.))
  in

  draw_table_sides 0.;
  Cairo.stroke ctx;

  let chair_radius = 15. in
  let num_sides = 100 in

  let angle = 2. *. 3.14159 /. float num_sides in

  let center_x = x +. 65. in
  let center_y = y +. 65. in

  let rec chair_table i =
    if i <= num_sides then (
      let start_x = center_x +. (chair_radius *. cos (float i *. angle)) in
      let start_y = center_y +. (chair_radius *. sin (float i *. angle)) in
      let end_x = center_x +. (chair_radius *. cos (float (i + 1) *. angle)) in
      let end_y = center_y +. (chair_radius *. sin (float (i + 1) *. angle)) in

      Cairo.move_to ctx start_x start_y;
      Cairo.line_to ctx end_x end_y;

      chair_table (i + 1))
  in

  chair_table 0;
  Cairo.stroke ctx;

  let chair_radius = 15. in
  let num_sides = 100 in

  let angle = 2. *. 3.14159 /. float num_sides in

  let center_x = x -. 65. in
  let center_y = y -. 65. in

  let rec chair_table i =
    if i <= num_sides then (
      let start_x = center_x +. (chair_radius *. cos (float i *. angle)) in
      let start_y = center_y +. (chair_radius *. sin (float i *. angle)) in
      let end_x = center_x +. (chair_radius *. cos (float (i + 1) *. angle)) in
      let end_y = center_y +. (chair_radius *. sin (float (i + 1) *. angle)) in

      Cairo.move_to ctx start_x start_y;
      Cairo.line_to ctx end_x end_y;

      chair_table (i + 1))
  in

  chair_table 0;
  Cairo.stroke ctx;

  let chair_radius = 15. in
  let num_sides = 100 in

  let angle = 2. *. 3.14159 /. float num_sides in

  let center_x = x -. 65. in
  let center_y = y +. 65. in

  let rec chair_table i =
    if i <= num_sides then (
      let start_x = center_x +. (chair_radius *. cos (float i *. angle)) in
      let start_y = center_y +. (chair_radius *. sin (float i *. angle)) in
      let end_x = center_x +. (chair_radius *. cos (float (i + 1) *. angle)) in
      let end_y = center_y +. (chair_radius *. sin (float (i + 1) *. angle)) in

      Cairo.move_to ctx start_x start_y;
      Cairo.line_to ctx end_x end_y;

      chair_table (i + 1))
  in

  chair_table 0;
  Cairo.stroke ctx;

  let chair_radius = 15. in
  let num_sides = 100 in

  let angle = 2. *. 3.14159 /. float num_sides in

  let center_x = x +. 65. in
  let center_y = y -. 65. in

  let rec chair_table i =
    if i <= num_sides then (
      let start_x = center_x +. (chair_radius *. cos (float i *. angle)) in
      let start_y = center_y +. (chair_radius *. sin (float i *. angle)) in
      let end_x = center_x +. (chair_radius *. cos (float (i + 1) *. angle)) in
      let end_y = center_y +. (chair_radius *. sin (float (i + 1) *. angle)) in

      Cairo.move_to ctx start_x start_y;
      Cairo.line_to ctx end_x end_y;

      chair_table (i + 1))
  in

  chair_table 0;
  Cairo.stroke ctx;
  ctx
