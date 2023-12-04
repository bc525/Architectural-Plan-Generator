type line = (int * int) * (int * int)
type wholeroom = { width : int; height : int; interval : int }
type furniture = Sink | Bed | RoundTable | Table | Couch | ThreeSeatedCouch

(** 
AF: the record {width = x; height = y; interval = z} represents a whole plan 
with width of x, height of y and structural grid interval of z. 
*)

type room = {
  tl : int * int;
  tr : int * int;
  br : int * int;
  bl : int * int;
  grline : line;
  dir : string;
  mutable furniture : furniture option;
}
(** 
AF: the record {tl= a; tr = b; br = c; bl=d; grline = line; dir = direction} 
represents the room with topleft corner be [a]; topright corner be [b]; 
bottomright corner be [c]; bottomleft corner be [d]. The line that is 
generated for division be line and the direction that the line is be direction
width of [x], height of y and structural grid interval of [z]. 
RI: [dir] should either be "w" or "h". *)

type door = { ln : (int * int) * (int * int); dir : string }

val generate_columngrid : wholeroom -> (int * int) list list
(**[generate_columngrid w] constructs a 10 x 10 grid of columns of the space 
represented as list of rows - which are list of points in that row.*)

val make_room : int -> int -> wholeroom
(**[makeroom x y] constructs a wholeroom with width of [x] height of [y]; 
    interval of 5.
    Requires: x and y are both multiples of 100 and larger than 400 smaller than 
    1000. *)

val divroom : wholeroom -> int -> room list
(**[divroom wr n] constructs room list to represent the [n] numbers of room that 
  the whole space [wr] been divided into. The [grline] of last room being 
  dividided to is ((-1,-1),(-1,-1)); and the [dir] is "last".
  Requires: 0 <= n <= 7. *)

val wholeroom_tostring : wholeroom -> string

val canvas_size : wholeroom -> int * int
(**[canvas_size wr] constructs a canvas size for drawing based on [wr]. The width 
    and height are both 400 more than the width and height or [wr]. *)
