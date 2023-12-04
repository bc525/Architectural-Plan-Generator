open Roomdiv

type label = { x : float; y : float; text : string }

let draw_labels cr labels =
  Cairo.set_source_rgb cr 0. 0. 0.;
  Cairo.select_font_face cr "Sans" ~weight:Normal ~slant:Upright;
  Cairo.set_font_size cr 10.;
  List.iter
    (fun label ->
      let x = label.x +. 200. in
      let y = label.y +. 200. in
      Cairo.move_to cr x y;
      Cairo.show_text cr label.text)
    labels;
  cr

let draw_type_labels cr labels =
  Cairo.set_source_rgb cr 0. 0. 0.;
  Cairo.select_font_face cr "Sans" ~weight:Normal ~slant:Upright;
  Cairo.set_font_size cr 8.;
  List.iter
    (fun label ->
      let x = label.x +. 185. in
      let y = label.y +. 210. in
      Cairo.move_to cr x y;
      Cairo.show_text cr label.text)
    labels;
  cr

(** Add numbers to each room generated and return a list of labels*)
let num_labels_to_room rooms =
  let rec loop acc index = function
    | [] -> List.rev acc
    | room :: rest ->
        let tl_x = float_of_int (fst room.tl) in
        let br_x = float_of_int (fst room.br) in
        let tl_y = float_of_int (snd room.tl) in
        let br_y = float_of_int (snd room.br) in
        let x = (tl_x +. br_x) /. 2. in
        let y = (tl_y +. br_y) /. 2. in
        let label = { x; y; text = string_of_int index } in
        loop (label :: acc) (index + 1) rest
  in
  loop [] 1 rooms

let type_labels_to_room rooms room_types =
  let rec loop acc i = function
    | [] -> List.rev acc
    | room :: rest ->
        let tl_x = float_of_int (fst room.tl) in
        let br_x = float_of_int (fst room.br) in
        let tl_y = float_of_int (snd room.tl) in
        let br_y = float_of_int (snd room.br) in
        let x = (tl_x +. br_x) /. 2. in
        let y = (tl_y +. br_y) /. 2. in
        let room_type = room_types.(i) in
        let label = { x; y; text = room_type } in
        loop (label :: acc) (i + 1) rest
  in
  loop [] 0 rooms

let get_room_types n =
  let room_types = Array.make n "" in
  let valid_types = [ "bedroom"; "bathroom"; "kitchen"; "livingroom" ] in
  let rec loop i =
    if i >= n then room_types
    else (
      print_endline
        ("Please enter the type of room for "
        ^ string_of_int (i + 1)
        ^ " (choose from Bedroom, Bathroom, LivingRoom, and Kitchen):");
      let room_type = read_line () |> String.lowercase_ascii in
      match List.mem room_type valid_types with
      | true ->
          room_types.(i) <- String.uppercase_ascii room_type;
          loop (i + 1)
      | false ->
          print_endline
            "Invalid room type entered. Please choose from Bedroom, Bathroom, \
             LivingRoom, and Kitchen.";
          loop i)
  in
  loop 0
