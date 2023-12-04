type line = (int * int) * (int * int)
type wholeroom = { width : int; height : int; interval : int }
type furniture = Sink | Bed | RoundTable | Table | Couch | ThreeSeatedCouch

type room = {
  tl : int * int;
  tr : int * int;
  br : int * int;
  bl : int * int;
  grline : line;
  dir : string;
  mutable furniture : furniture option;
}

type door = { ln : line; dir : string }

let make_room x y = { width = x; height = y; interval = 5 }

let generate_columngrid wr =
  List.init
    ((wr.width / 100) - 1)
    (fun c ->
      let r = List.init ((wr.height / 100) - 1) (fun x -> 100 * (x + 1)) in
      List.map (fun i -> ((c + 1) * 100, i)) r)

let canvas_size wr = (wr.width + 400, wr.height + 400)

let checkdir (rl : room list) =
  match rl with
  | h1 :: h2 :: _ ->
      if h1.dir = "w" && h2.dir = "w" then "h"
      else if h1.dir = "h" && h2.dir = "h" then "w"
      else "fine"
  | _ -> "fine"

let float_to_int f =
  if f < 1. then invalid_arg "float should not be less than 1"
  else
    let i = int_of_float f in
    if f -. float_of_int i < 0.5 then i else i + 1

let moving_h (p1x, p1y) (p2x, _) height =
  let h = Random.float (height / 50 / 2 |> float_of_int) in
  let moving_h = if h < 1. then 50 else 50 * (h |> float_to_int) in
  {
    tl = (p1x, p1y);
    tr = (p2x, p1y);
    br = (p2x, p1y + moving_h);
    bl = (p1x, p1y + moving_h);
    grline = ((p1x, p1y + moving_h), (p2x, p1y + moving_h));
    dir = "h";
    furniture = None;
  }

let moving_w (p1x, p1y) (_, p2y) width =
  let w = Random.float (width / 50 / 2 |> float_of_int) in
  let moving_w = if w < 1. then 50 else 50 * (w |> float_to_int) in
  {
    tl = (p1x, p1y);
    tr = (p1x + moving_w, p1y);
    br = (p1x + moving_w, p2y);
    bl = (p1x, p2y);
    grline = ((p1x + moving_w, p1y), (p1x + moving_w, p2y));
    dir = "w";
    furniture = None;
  }

let rec divroom_hp (p1x, p1y) (p2x, p2y) n (lst : room list) =
  if n = 1 then
    {
      tl = (p1x, p1y);
      tr = (p2x, p1y);
      br = (p2x, p2y);
      bl = (p1x, p2y);
      grline = ((-1, -1), (-1, -1));
      dir = "last";
      furniture = None;
    }
    :: lst
  else
    let width = p2x - p1x in
    let height = p2y - p1y in
    Random.self_init ();
    let direction = checkdir lst in
    if width <= 50 && height <= 50 then lst
    else if (direction = "h" && height > 50) || (direction = "w" && height <= 50)
    then
      let r = moving_h (p1x, p1y) (p2x, p2y) height in
      divroom_hp r.bl (p2x, p2y) (n - 1) (r :: lst)
    else if direction = "h" || (direction = "w" && height > 50) then
      let r = moving_w (p1x, p1y) (p2x, p2y) width in
      divroom_hp r.tr (p2x, p2y) (n - 1) (r :: lst)
    else if Random.bool () then
      let r = moving_h (p1x, p1y) (p2x, p2y) height in
      divroom_hp r.bl (p2x, p2y) (n - 1) (r :: lst)
    else
      let r = moving_w (p1x, p1y) (p2x, p2y) width in
      divroom_hp r.tr (p2x, p2y) (n - 1) (r :: lst)

let divroom wr n = divroom_hp (0, 0) (wr.width, wr.height) n []

let wholeroom_tostring wr =
  "width = " ^ string_of_int wr.width ^ "; " ^ "height = "
  ^ string_of_int wr.height ^ "; " ^ "interval = " ^ string_of_int wr.interval
