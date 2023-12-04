open Roomdiv

val create_canvas : int * int -> Cairo.Surface.t
(** [create_canvas (x,y)] returns a empty Cairo.Surface.t. of width [x] and height [y] *)

val draw_canvas : Cairo.context -> Cairo.context
(** [draw_canvas cr] returns a Cairo.context with the everywhere painted white *)

val draw_rectangle :
  Cairo.context -> float * float -> float * float -> float -> Cairo.context
(** [draw_rectangle cr (x,y) (w,h) lineweight] draws a rectangle with width [w]
    height [h] at the start point (x,y) in the user space coordinate with the
    line of [lineweight] width in the [cr]. *)

val draw_rooms : room list -> Cairo.context -> float -> Cairo.context
(** [draw_rooms rms lineweight] draws the list of rooms [rms] in the user 
space coordinate with the line of [lineweight] width in the [cr]. *)

val draw_columns : (int * int) list -> Cairo.context -> float -> Cairo.context
(** [draw_columns columnslst lineweight] draws the list of columns with 
coordinates located in [columnslst] in the user space coordinate with the
line of [lineweight] width in the [cr]. *)

val draw_door_down : Cairo.context -> float * float -> Cairo.context
(** [draw_door_down cr (x,y)] draws a door with the hinge at the start point 
(x,y) in the user space coordinate facing down and swinging to the left in 
the [cr].*)

val draw_door_right : Cairo.context -> float * float -> Cairo.context
(** [draw_door_right cr (x,y)] draws a door with the hinge at the start 
point (x,y) in the user space coordinate facing to the left and swinging up in 
the [cr]. *)

val draw_windows_rooms : Cairo.context -> wholeroom -> Cairo.context
(** [draw_windows_rooms cr rms space] draws a window around the building. *)

val draw_structural_gird : Cairo.context -> wholeroom -> Cairo.context
(** [draw_structural_gird cr space] draws the structural grids around 
    the building. *)

val label_structure : Cairo.context -> wholeroom -> Cairo.context
(** [label_structure cr space] labels the structural grids around 
    the building. *)

val dim : Cairo.context -> wholeroom -> room list -> room -> Cairo.context
(** [dim] labels dimensions of this space. *)

val draw_couch : Cairo.context -> float -> float -> Cairo.context
(** [draw_couch ctx x y] takes in the x and y coordinates as input 
    and draws a couch based on the position of the coordinate pair *)

val draw_bed : Cairo.context -> float -> float -> Cairo.context
(** [draw_bed ctx x y] takes in the x and y coordinates as input 
    and draws a bed based on the position of the coordinate pair *)

val draw_table : Cairo.context -> float -> float -> Cairo.context
(** [draw_table ctx x y] takes in the x and y coordinates as input 
    and draws a table based on the position of the coordinate pair *)

val draw_sink : Cairo.context -> float -> float -> Cairo.context
(** [draw_sink ctx x y] takes in the x and y coordinates as input 
    and draws a sink based on the position of the coordinate pair *)
    
val draw_three_seated_couch : Cairo.context -> float -> float -> Cairo.context
(** [draw_three_seated_couch ctx x y] takes in the x and y coordinates as input 
    and draws a couch based on the position of the coordinate pair *)

val draw_round_table_with_chairs :
  Cairo.context -> float -> float -> Cairo.context
  (** [draw_round_table_with_chairs ctx x y] takes in the x and y coordinates 
  as input and draws a round table based on the position of the coordinate 
  pair *)
  
