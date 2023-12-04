open Roomdiv

type label = { x : float; y : float; text : string }

val draw_labels : Cairo.context -> label list -> Cairo.context
(** [draw_labels cr label] draws a list of number labels on the provided Cairo context [cr].
    It returns the updated Cairo context after drawing the labels.*)

val draw_type_labels : Cairo.context -> label list -> Cairo.context
(** [draw_type_labels cr label]draws a list of labels of rooms on the provided Cairo context [cr].
    It returns the updated Cairo context after drawing the labels.*)

val num_labels_to_room : room list -> label list
(** [num_labels_to_room rooms] generates a label list containing the number labels for each room in the given list [rooms].
    It returns the generated label list. *)

val type_labels_to_room : room list -> string array -> label list
(** [type_labels_to_room rooms room_types] generates a label list based on the room types associated with each room.
    It takes a list of rooms [rooms] and an array of room types [room_types].
    It returns a label list where each label contains the coordinates and the corresponding room type text. *)

val get_room_types : int -> string array
(** [get_room_types n] prompts the user to enter the types of rooms and stores them in an array.
    It takes an integer [n] representing the number of rooms.
    It returns an array containing the room types entered by the user.*)
