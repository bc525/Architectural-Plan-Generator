open Makeroom.Roomdiv
open Makeroom.Draw
open Makeroom.Labels

let draw_furniture ctx room =
  let x = float_of_int (fst room.tl) +. 200. in
  let y = float_of_int (snd room.tl) +. 300. in
  let z = float_of_int (fst room.tr) in
  let a = float_of_int (snd room.tr) in
  match room.furniture with
  | Some RoundTable ->
      if z -. x < 100. && a -. y < 50. then ctx
      else draw_round_table_with_chairs ctx (x +. 100.) (y +. 100.)
  | Some Table ->
      if z -. x < 100. && a -. y < 50. then ctx else draw_table ctx x y
  | Some Couch ->
      if z -. x < 100. && a -. y < 70. then ctx else draw_couch ctx x y
  | Some ThreeSeatedCouch ->
      if z -. x < 100. && a -. y < 70. then ctx
      else draw_three_seated_couch ctx (x +. 50.) y
  | Some Bed -> if z -. x < 100. && a -. y < 70. then ctx else draw_bed ctx x y
  | Some Sink -> if z -. x < 50. && a -. y < 20. then ctx else draw_sink ctx x y
  | None -> ctx

let add_furniture room roomtypes =
  Random.self_init ();
  match roomtypes with
  | "LIVINGROOM" -> (
      match Random.int 4 with
      | 1 -> room.furniture <- Some RoundTable
      | 2 -> room.furniture <- Some Table
      | 3 -> room.furniture <- Some Couch
      | 0 -> room.furniture <- Some ThreeSeatedCouch
      | _ -> room.furniture <- Some Table)
  | "BEDROOM" -> (
      match Random.int 3 with
      | 1 -> room.furniture <- Some Table
      | 0 -> room.furniture <- Some Bed
      | 2 -> room.furniture <- Some ThreeSeatedCouch
      | _ -> room.furniture <- Some Bed)
  | "BATHROOM" -> room.furniture <- Some Sink
  | "KITCHEN" -> (
      match Random.int 2 with
      | 1 -> room.furniture <- Some Table
      | 0 -> room.furniture <- Some Sink
      | _ -> room.furniture <- Some Sink)
  | _ -> ()

let () =
  print_endline
    "This plan generator will produce a random architecture plan based on your \
     input";
  print_endline
    "Please enter the width and height of this space (ints between 400 and \
     1000)";
  print_endline "Width: ";
  let width = read_line () |> String.trim |> int_of_string in
  prerr_endline "Height: ";
  let height = read_line () |> String.trim |> int_of_string in
  let space = make_room width height in
  print_endline
    "Please enter the number of rooms you want to have (int between 1 and 7 \
     inclusively)";
  print_endline "Number: ";
  let room_number = read_line () |> String.trim |> int_of_string in

  let room_types = get_room_types room_number in

  for i = 1 to 3 do
    let size = canvas_size space in
    let surface = create_canvas size in
    let cr1 = Cairo.create surface in
    let cr2 = draw_canvas cr1 in
    let cr3 =
      draw_rectangle cr2 (200., 200.)
        (float_of_int width, float_of_int height)
        4.
    in
    let roomlst = divroom space room_number in

    let _ = draw_rooms (List.tl roomlst) cr3 4. in
    let columnlst = List.concat (generate_columngrid space) in
    let _ = draw_structural_gird cr3 space in
    let _ = label_structure cr3 space in
    let _ = draw_columns columnlst cr3 3. in
    let _ = draw_door_down cr3 (215., 200.) in
    let _ = draw_windows_rooms cr3 space in
    let _ = dim cr3 space (List.tl roomlst) (List.hd roomlst) in
    let _ = draw_rectangle cr3 (215., 160.) (225., 40.) 0.8 in
    let _ = draw_rectangle cr3 (230., 160.) (15., 40.) 0.5 in
    let _ = draw_rectangle cr3 (245., 160.) (15., 40.) 0.5 in
    let _ = draw_rectangle cr3 (260., 160.) (15., 40.) 0.5 in
    let _ = draw_rectangle cr3 (275., 160.) (15., 40.) 0.5 in
    let _ = draw_rectangle cr3 (290., 160.) (15., 40.) 0.5 in
    let _ = draw_rectangle cr3 (305., 160.) (15., 40.) 0.5 in
    let _ = draw_rectangle cr3 (320., 160.) (15., 40.) 0.5 in
    let _ = draw_rectangle cr3 (335., 160.) (15., 40.) 0.5 in
    let _ = draw_rectangle cr3 (350., 160.) (15., 40.) 0.5 in
    let _ = draw_rectangle cr3 (365., 160.) (15., 40.) 0.5 in
    let _ = draw_rectangle cr3 (380., 160.) (15., 40.) 0.5 in
    let _ = draw_rectangle cr3 (395., 160.) (15., 40.) 0.5 in
    let _ = draw_rectangle cr3 (410., 160.) (15., 40.) 0.5 in
    let _ = draw_rectangle cr3 (425., 160.) (15., 40.) 0.5 in
    let labels = num_labels_to_room roomlst in
    let _ = draw_labels cr1 labels in
    Cairo.PNG.write surface (Printf.sprintf "rectangle%d.png" i);
    let labels_type = type_labels_to_room roomlst room_types in
    let _ = draw_type_labels cr3 labels_type in

    for i = 0 to room_number - 1 do
      add_furniture (List.nth roomlst i) (Array.get room_types i)
    done;

    for j = 0 to List.length roomlst - 1 do
      let room = List.nth roomlst j in
      let _ = draw_furniture cr3 room in
      ()
    done;
    Cairo.PNG.write surface (Printf.sprintf "rectangle%d.png" i);
    Cairo.Surface.finish surface
  done
