(**
    Test Plan:
    - We test the data types that we construct for the plan generation using OUnit
      tests, since most of them are functional and we have different properties
      that we can use for testing. However, the drawing part is hard to validate 
      by textual tests, so we decide to test the drawing part by making the images
      and do the visual check. Also, since the library that we depend on Cairo,
      does not provide good functions to inquire properties so it would be better
      for us to do visual tests.text
    - We use both black-box and glass-box testing, tests on usual and edge cases, 
      and try to do different input that will achieve coverage of our code. 
    - By testing as we develop our program and maintain a test suite to ensure
      our system do not regress while developing, we believe we can demonstrate 
      the correctness of our system as we progressed. Also since, different parts
      are first tested separately, then the system are tested as a whole, we believe
      this way of testing should validate the functionality of our system. 
    - OUnit Tests: the basic room elements generations, including the coordinates
      of the rooms, the columns, the whole space, and the sizes are tested using
      the OUnit tests. 
    - Test Suites for Drawings: As we develop our drawing functions, we keep and
      continously builds up a set of tests that could make several image outputs 
      for us to check the validity of the drawing functions.
    - Manual Tests: the whole system are tested through terminal, where we run
      our program and make user input to test the functionality of the whole system.
*)

open OUnit2
open Makeroom.Roomdiv
open Makeroom.Draw
open Makeroom.Labels

let make_room_test (name : string) (w : int) (h : int) (exp_r : wholeroom) =
  name >:: fun _ ->
  assert_equal exp_r (make_room w h) ~printer:wholeroom_tostring

let column_test (name : string) (wr : wholeroom) (exp_out : int) =
  name >:: fun _ ->
  assert_equal exp_out
    (generate_columngrid wr |> List.flatten |> List.length)
    ~printer:string_of_int

let div_room_length_test (name : string) (wr : wholeroom) (n : int) =
  name >:: fun _ ->
  assert_equal n (divroom wr n |> List.length) ~printer:string_of_int

let two_div_room_test (name : string) (wr : wholeroom) =
  name >:: fun _ ->
  let r = divroom wr 2 |> List.tl |> List.hd in
  if r.dir = "h" then assert_equal (0, wr.width) (r.bl |> fst, r.br |> fst)
  else assert_equal (0, wr.height) (r.tr |> snd, r.br |> snd)

let last_size_test (name : string) (wr : wholeroom) (n : int) =
  name >:: fun _ ->
  let r = divroom wr n |> List.hd in
  if r.dir = "h" then
    assert (
      fst r.tr - fst r.tl >= 50
      || wr.height - snd r.br >= 50
      || snd r.bl - snd r.tl >= 50)
  else
    assert (
      fst r.tr - fst r.tl >= 50
      || wr.width - snd r.br >= 50
      || snd r.bl - snd r.tl >= 50)

let size_noless_50 r =
  assert (fst r.tr - fst r.tl >= 50 || snd r.br - snd r.tr >= 50)

let div_size_test (name : string) (wr : wholeroom) (n : int) =
  name >:: fun _ ->
  let r = divroom wr n in
  List.iter (fun x -> size_noless_50 x) r

let rec div_room (lst : room list) : unit =
  match List.tl lst with
  | h1 :: h2 :: t ->
      if h1.dir = "w" && h2.dir = "w" then (
        assert (snd h1.tl = snd h2.tl && snd h1.br = snd h2.br);
        div_room (h2 :: t))
      else if h1.dir = "w" && h2.dir = "h" then (
        assert (fst h1.tl = fst h2.bl && snd h1.tr = snd h2.br);
        div_room (h2 :: t))
      else if h1.dir = "h" && h2.dir = "w" then (
        assert (snd h1.tr = snd h2.tr && fst h1.tl = fst h2.br);
        div_room (h2 :: t))
      else (
        assert (fst h1.tl = fst h2.tl && fst h1.tr = fst h2.tr);
        div_room (h2 :: t))
  | _ -> ()

let div_room_test (name : string) (wr : wholeroom) (n : int) =
  name >:: fun _ ->
  let r = divroom wr n in
  div_room r

let rec numb_labels (lst : room list) (lst_label : label list) : unit =
  match (lst, lst_label) with
  | h1 :: t1, h2 :: t2 ->
      let tl_x = float_of_int (fst h1.tl) in
      let br_x = float_of_int (fst h1.br) in
      assert ((tl_x +. br_x) /. 2. = h2.x);
      numb_labels t1 t2
  | _ -> ()

let num_labels_test (name : string) (wr : wholeroom) (n : int) =
  name >:: fun _ ->
  let r = divroom wr n in
  let n = num_labels_to_room r in
  numb_labels r n

let suite =
  "suites"
  >::: [
         make_room_test "basic space 1" 500 1000
           { width = 500; height = 1000; interval = 5 };
         make_room_test "basic space 2" 600 700
           { width = 600; height = 700; interval = 5 };
         make_room_test "basic space 3" 900 700
           { width = 900; height = 700; interval = 5 };
         make_room_test "basic space 4" 1000 1000
           { width = 1000; height = 1000; interval = 5 };
         column_test "400x400 grid"
           { width = 400; height = 400; interval = 5 }
           9;
         column_test "600x900 grid"
           { width = 600; height = 900; interval = 5 }
           40;
         column_test "1000x1000 grid"
           { width = 1000; height = 1000; interval = 5 }
           81;
         column_test "500x400 grid"
           { width = 500; height = 400; interval = 5 }
           12;
         column_test "800x400 grid"
           { width = 800; height = 400; interval = 5 }
           21;
         div_room_length_test "0 room"
           { width = 600; height = 700; interval = 5 }
           1;
         div_room_length_test "3 rooms"
           { width = 400; height = 400; interval = 5 }
           3;
         div_room_length_test "7 rooms"
           { width = 400; height = 400; interval = 5 }
           7;
         div_room_length_test "5 rooms"
           { width = 1000; height = 400; interval = 5 }
           5;
         div_room_length_test "2 rooms"
           { width = 1000; height = 800; interval = 5 }
           2;
         two_div_room_test "basic - divide into 2 rooms"
           { width = 800; height = 1000; interval = 5 };
         two_div_room_test "basic - divide into 2 rooms"
           { width = 500; height = 400; interval = 5 };
         two_div_room_test "basic - divide into 2 rooms"
           { width = 900; height = 600; interval = 5 };
         two_div_room_test "basic - divide into 2 rooms"
           { width = 400; height = 400; interval = 5 };
         two_div_room_test "basic - divide into 2 rooms"
           { width = 1000; height = 400; interval = 5 };
         last_size_test "size test last - divide 2 rooms in 400 x 400"
           { width = 400; height = 400; interval = 5 }
           2;
         last_size_test "size test last - divide 7 rooms in 400 x 400"
           { width = 400; height = 400; interval = 5 }
           7;
         last_size_test "size test last - divide 7 rooms in 800 x 1000"
           { width = 800; height = 1000; interval = 5 }
           7;
         last_size_test "size test last - divide 6 rooms in 600 x 1000"
           { width = 600; height = 1000; interval = 5 }
           6;
         last_size_test "size test last - divide 4 rooms in 700 x 700"
           { width = 700; height = 700; interval = 5 }
           4;
         last_size_test "size test last - divide 5 rooms in 1000 x 1000"
           { width = 1000; height = 1000; interval = 5 }
           5;
         last_size_test "size test last - divide 2 rooms in 800 x 400"
           { width = 800; height = 400; interval = 5 }
           2;
         div_size_test "size test nonlast - divide 7 rooms in 400 x 400"
           { width = 400; height = 400; interval = 5 }
           7;
         div_size_test "size test nonlast - divide 6 rooms in 1000 x 1000"
           { width = 1000; height = 1000; interval = 5 }
           6;
         div_size_test "size test nonlast - divide 3 rooms in 900 x 500"
           { width = 900; height = 500; interval = 5 }
           3;
         div_size_test "size test nonlast - divide 1 rooms in 1000 x 400"
           { width = 1000; height = 1000; interval = 5 }
           2;
         div_size_test "size test nonlast - divide 4 rooms in 500 x 400"
           { width = 500; height = 400; interval = 5 }
           4;
         div_size_test "size test nonlast - divide 5 rooms in 400 x 800"
           { width = 400; height = 800; interval = 5 }
           5;
         div_size_test "size test nonlast - divide 2 rooms in 900 x 400"
           { width = 900; height = 400; interval = 5 }
           2;
         div_size_test "size test nonlast - divide 6 rooms in 600 x 700"
           { width = 600; height = 700; interval = 5 }
           6;
         div_size_test "size test nonlast - divide 3 rooms in 900 x 900"
           { width = 900; height = 900; interval = 5 }
           3;
         div_size_test "size test nonlast - divide 5 rooms in 600 x 600"
           { width = 600; height = 600; interval = 5 }
           5;
         div_room_test "test divide 5 rooms coordinate properties 600x600"
           { width = 600; height = 600; interval = 5 }
           5;
         div_room_test "test divide 7 rooms coordinate properties 400x400"
           { width = 400; height = 400; interval = 5 }
           7;
         div_room_test "test divide 3 rooms coordinate properties 400x400"
           { width = 400; height = 400; interval = 5 }
           3;
         div_room_test "test divide 4 rooms coordinate properties 1000x400"
           { width = 1000; height = 400; interval = 5 }
           4;
         div_room_test "test divide 6 rooms coordinate properties 900x600"
           { width = 900; height = 600; interval = 5 }
           6;
         div_room_test "test divide 5 rooms coordinate properties 600x400"
           { width = 600; height = 400; interval = 5 }
           5;
         div_room_test "test divide 2 rooms coordinate properties 900x500"
           { width = 900; height = 500; interval = 5 }
           2;
         div_room_test "test divide 1 rooms coordinate properties 500x400"
           { width = 500; height = 400; interval = 5 }
           2;
         div_room_test "test divide 3 rooms coordinate properties 700x800"
           { width = 700; height = 800; interval = 5 }
           3;
         div_room_test "test divide 6 rooms coordinate properties 800x1000"
           { width = 800; height = 1000; interval = 5 }
           6;
         num_labels_test
           "label test divide 2 rooms coordinate properties 800x1000"
           { width = 800; height = 1000; interval = 5 }
           2;
         num_labels_test
           "label test divide 3 rooms coordinate properties 800x1000"
           { width = 800; height = 1000; interval = 5 }
           3;
         num_labels_test
           "label test divide 7 rooms coordinate properties 800x1000"
           { width = 800; height = 1000; interval = 5 }
           7;
         num_labels_test
           "label test divide 5 rooms coordinate properties 800x1000"
           { width = 1000; height = 1000; interval = 5 }
           5;
         num_labels_test
           "label test divide 6 rooms coordinate properties 800x1000"
           { width = 400; height = 400; interval = 5 }
           6;
         num_labels_test
           "label test divide 1 rooms coordinate properties 800x1000"
           { width = 1000; height = 1000; interval = 5 }
           1;
       ]

(** Test labels.num_labels_to_rooms *)

let _ = run_test_tt_main suite

let () =
  let surface = create_canvas (600, 800) in
  let cr = Cairo.create surface in
  let labels1 = [ { x = 200.; y = 200.; text = "BEDROOM" } ] in
  let _ = draw_labels cr labels1 in
  Cairo.PNG.write surface "label1.png";

  let labels2 = [ { x = 300.; y = 300.; text = "LIVINGROOM" } ] in
  let _ = draw_labels cr labels2 in
  Cairo.PNG.write surface "label3.png";

  let labels3 =
    [
      { x = 300.; y = 300.; text = "LIVINGROOM" };
      { x = 100.; y = 300.; text = "LIVINGROOM" };
      { x = 106.; y = 100.; text = "BATHROOM" };
    ]
  in
  let _ = draw_labels cr labels3 in
  Cairo.PNG.write surface "label2.png";
  Cairo.Surface.finish surface

(**test draw furnitures*)
let () =
  let surface = create_canvas (600, 800) in
  let cr = Cairo.create surface in
  let updated_cr = draw_couch cr 300. 200. in
  let updated_cr = draw_bed updated_cr 100. 400. in
  let updated_cr = draw_table updated_cr 200. 600. in
  let updated_cr = draw_sink updated_cr 400. 300. in
  let updated_cr = draw_three_seated_couch updated_cr 100. 100. in
  let _ = draw_round_table_with_chairs updated_cr 500. 500. in

  Cairo.PNG.write surface "test_couch_bed_sink.png";
  Cairo.Surface.finish surface

(**test draw columns 1*)
let () =
  let surface = create_canvas (900, 900) in
  let cr = Cairo.create surface in
  let _ =
    draw_columns
      (List.flatten
         (generate_columngrid { width = 500; height = 500; interval = 5 }))
      cr 3.
  in
  Cairo.PNG.write surface "test/test_columns_1.png";
  Cairo.Surface.finish surface

(**test draw columns 2*)
let () =
  let surface = create_canvas (1000, 1400) in
  let cr = Cairo.create surface in
  let _ =
    draw_columns
      (List.flatten
         (generate_columngrid { width = 400; height = 1000; interval = 5 }))
      cr 6.
  in
  Cairo.PNG.write surface "test/test_columns_2.png";
  Cairo.Surface.finish surface

(**test draw canvas*)
let () =
  let size = canvas_size { width = 400; height = 1000; interval = 5 } in
  let surface = create_canvas size in
  let cr = Cairo.create surface in
  let _ = draw_canvas cr in
  Cairo.PNG.write surface "test/test_canvas.png";
  Cairo.Surface.finish surface

(**test draw rectangles 1*)
let () =
  let surface = create_canvas (900, 900) in
  let cr = Cairo.create surface in
  let _ = draw_rectangle cr (100., 100.) (500., 700.) 2. in
  Cairo.PNG.write surface "test/test_rectangles_1.png";
  Cairo.Surface.finish surface

(**test draw rectangles 2*)
let () =
  let surface = create_canvas (900, 900) in
  let cr = Cairo.create surface in
  let _ = draw_rectangle cr (50., 100.) (831., 736.) 2. in
  Cairo.PNG.write surface "test/test_rectangles_2.png";
  Cairo.Surface.finish surface

(**test draw doors 1*)
let () =
  let surface = create_canvas (200, 200) in
  let cr = Cairo.create surface in
  let _ = draw_door_down cr (100., 100.) in
  Cairo.PNG.write surface "test/test_doors_1.png";
  Cairo.Surface.finish surface

(**test draw doors 2*)
let () =
  let surface = create_canvas (200, 200) in
  let cr = Cairo.create surface in
  let _ = draw_door_right cr (100., 100.) in
  Cairo.PNG.write surface "test/test_doors_2.png";
  Cairo.Surface.finish surface

(**test draw rooms 1*)
let () =
  let surface = create_canvas (1400, 900) in
  let cr = Cairo.create surface in
  let space = { width = 1000; height = 500; interval = 5 } in
  let rooms = divroom space 7 in
  let _ = draw_rooms rooms cr 2. in
  Cairo.PNG.write surface "test/test_rooms_1.png";
  Cairo.Surface.finish surface

(**test draw rooms 2*)
let () =
  let surface = create_canvas (800, 1000) in
  let cr = Cairo.create surface in
  let space = { width = 400; height = 600; interval = 5 } in
  let rooms = divroom space 4 in
  let _ = draw_rooms rooms cr 5. in
  Cairo.PNG.write surface "test/test_rooms_2.png";
  Cairo.Surface.finish surface

(**test draw rooms 3*)
let () =
  let surface = create_canvas (1100, 1300) in
  let cr = Cairo.create surface in
  let space = { width = 700; height = 900; interval = 5 } in
  let rooms = divroom space 2 in
  let _ = draw_rooms rooms cr 4. in
  Cairo.PNG.write surface "test/test_rooms_3.png";
  Cairo.Surface.finish surface

(**test draw rooms 4*)
let () =
  let surface = create_canvas (1200, 1200) in
  let cr = Cairo.create surface in
  let space = { width = 800; height = 800; interval = 5 } in
  let rooms = divroom space 2 in
  let _ = draw_rooms rooms cr 1. in
  Cairo.PNG.write surface "test/test_rooms_4.png";
  Cairo.Surface.finish surface

(**test draw structural grids 1*)
let () =
  let surface = create_canvas (1200, 1200) in
  let cr = Cairo.create surface in
  let space = { width = 800; height = 800; interval = 5 } in
  let _ = draw_structural_gird cr space in
  Cairo.PNG.write surface "test/test_grids_1.png";
  Cairo.Surface.finish surface

(**test draw structural grids 2*)
let () =
  let surface = create_canvas (1400, 1200) in
  let cr = Cairo.create surface in
  let space = { width = 1000; height = 800; interval = 5 } in
  let _ = draw_structural_gird cr space in
  Cairo.PNG.write surface "test/test_grids_2.png";
  Cairo.Surface.finish surface

(**test draw structural grids labels 1*)
let () =
  let surface = create_canvas (800, 800) in
  let cr = Cairo.create surface in
  let space = { width = 400; height = 400; interval = 5 } in
  let _ = label_structure cr space in
  Cairo.PNG.write surface "test/test_gridslabels_1.png";
  Cairo.Surface.finish surface

(**test draw structural grids labels 2*)
let () =
  let surface = create_canvas (1400, 1400) in
  let cr = Cairo.create surface in
  let space = { width = 1000; height = 1000; interval = 5 } in
  let _ = label_structure cr space in
  Cairo.PNG.write surface "test/test_gridslabels_2.png";
  Cairo.Surface.finish surface

(**test draw dims 1*)
let () =
  let surface = create_canvas (1400, 1400) in
  let cr = Cairo.create surface in
  let space = { width = 1000; height = 1000; interval = 5 } in
  let rooms = divroom space 4 in
  let _ = dim cr space (List.tl rooms) (List.hd rooms) in
  Cairo.PNG.write surface "test/test_dims_1.png";
  Cairo.Surface.finish surface

(**test draw dims 2*)
let () =
  let surface = create_canvas (1400, 1400) in
  let cr = Cairo.create surface in
  let space = { width = 600; height = 1000; interval = 5 } in
  let rooms = divroom space 7 in
  let _ = dim cr space (List.tl rooms) (List.hd rooms) in
  Cairo.PNG.write surface "test/test_dims_2.png";
  Cairo.Surface.finish surface

(**test draw dims 3*)
let () =
  let surface = create_canvas (1400, 1400) in
  let cr = Cairo.create surface in
  let space = { width = 400; height = 900; interval = 5 } in
  let rooms = divroom space 3 in
  let _ = dim cr space (List.tl rooms) (List.hd rooms) in
  Cairo.PNG.write surface "test/test_dims_3.png";
  Cairo.Surface.finish surface

(**test draw dims 4*)
let () =
  let surface = create_canvas (1400, 1400) in
  let cr = Cairo.create surface in
  let space = { width = 400; height = 400; interval = 5 } in
  let rooms = divroom space 1 in
  let _ = dim cr space (List.tl rooms) (List.hd rooms) in
  Cairo.PNG.write surface "test/test_dims_4.png";
  Cairo.Surface.finish surface

let whole x y z =
  let space = { width = x; height = y; interval = 5 } in
  let rooms = divroom space z in
  let size = canvas_size space in
  let surface = create_canvas size in
  let cr = Cairo.create surface in
  let _ = draw_canvas cr in
  let _ =
    draw_rectangle cr
      (float_of_int 200, float_of_int 200)
      (float_of_int x, float_of_int y)
      4.
  in
  let _ = draw_columns (List.flatten (generate_columngrid space)) cr 3. in
  let _ = draw_rooms rooms cr 4. in
  let _ = draw_structural_gird cr space in
  let _ = label_structure cr space in
  let _ = dim cr space (List.tl rooms) (List.hd rooms) in
  let _ = draw_windows_rooms cr space in
  surface

(**test whole 1*)
let () =
  let s = whole 400 1000 5 in
  Cairo.PNG.write s "test/test_whole_1.png";
  Cairo.Surface.finish s

(**test whole 2*)
let () =
  let s = whole 1000 1000 2 in
  Cairo.PNG.write s "test/test_whole_2.png";
  Cairo.Surface.finish s

(**test whole 3*)
let () =
  let s = whole 700 1000 3 in
  Cairo.PNG.write s "test/test_whole_3.png";
  Cairo.Surface.finish s

(**test whole 4*)
let () =
  let s = whole 800 500 6 in
  Cairo.PNG.write s "test/test_whole_4.png";
  Cairo.Surface.finish s

(**test whole 5*)
let () =
  let s = whole 600 700 7 in
  Cairo.PNG.write s "test/test_whole_5.png";
  Cairo.Surface.finish s

(**test whole 6*)
let () =
  let s = whole 400 500 1 in
  Cairo.PNG.write s "test/test_whole_6.png";
  Cairo.Surface.finish s

(**test whole 7*)
let () =
  let s = whole 1000 500 4 in
  Cairo.PNG.write s "test/test_whole_7.png";
  Cairo.Surface.finish s

(**test whole 8*)
let () =
  let s = whole 800 900 5 in
  Cairo.PNG.write s "test/test_whole_8.png";
  Cairo.Surface.finish s

(**test whole 9*)
let () =
  let s = whole 1000 400 3 in
  Cairo.PNG.write s "test/test_whole_9.png";
  Cairo.Surface.finish s

(**test whole 10*)
let () =
  let s = whole 900 600 5 in
  Cairo.PNG.write s "test/test_whole_10.png";
  Cairo.Surface.finish s

(**test whole 6*)
let () =
  let s = whole 400 500 1 in
  Cairo.PNG.write s "test/test_whole_6.png";
  Cairo.Surface.finish s

(**test whole 7*)
let () =
  let s = whole 1000 500 4 in
  Cairo.PNG.write s "test/test_whole_7.png";
  Cairo.Surface.finish s

(**test whole 8*)
let () =
  let s = whole 800 900 5 in
  Cairo.PNG.write s "test/test_whole_8.png";
  Cairo.Surface.finish s

(**test whole 9*)
let () =
  let s = whole 1000 400 3 in
  Cairo.PNG.write s "test/test_whole_9.png";
  Cairo.Surface.finish s

(**test whole 10*)
let () =
  let s = whole 900 600 5 in
  Cairo.PNG.write s "test/test_whole_10.png";
  Cairo.Surface.finish s

(**test whole 11*)
let () =
  let s = whole 700 500 3 in
  Cairo.PNG.write s "test/test_whole_11.png";
  Cairo.Surface.finish s

(**test whole 12*)
let () =
  let s = whole 1000 1000 7 in
  Cairo.PNG.write s "test/test_whole_12.png";
  Cairo.Surface.finish s

(**test whole 13*)
let () =
  let s = whole 700 900 7 in
  Cairo.PNG.write s "test/test_whole_13.png";
  Cairo.Surface.finish s

(**test whole 14*)
let () =
  let s = whole 400 400 2 in
  Cairo.PNG.write s "test/test_whole_14.png";
  Cairo.Surface.finish s

(**test whole 15*)
let () =
  let s = whole 900 500 4 in
  Cairo.PNG.write s "test/test_whole_15.png";
  Cairo.Surface.finish s
