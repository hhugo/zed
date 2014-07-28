open React

type 'a t

val create : 'a Zed_edit.context -> int -> int -> 'a t

val get_lines : 'a t -> int * Zed_rope.t option list

val changes : 'a t -> unit event

val context : 'a t -> 'a Zed_edit.context
val edit : 'a t -> 'a Zed_edit.t
val cursor : 'a t -> Zed_cursor.t

val line_start : 'a t -> int
val line_stop : 'a t -> int
