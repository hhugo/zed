


type 'a t = {
  editor : 'a Zed_edit.t;
  context : 'a Zed_edit.context;
  box : box;
  top_left : Zed_cursor.t;
}
and box = {
  cols : int;
  lines : int
}

let context t = t.context
let edit t = t.editor
let cursor t = Zed_edit.cursor t.context

let iter (f : 'a -> unit) (e : 'a React.event)  =
  React.E.fix (fun e' ->
      let _ = React.E.map f e' in
      e,()
    )

let create context lines cols =
  let editor = Zed_edit.edit context in
  let t = {
    editor;
    context;
    box = {
      lines;
      cols;
    };
    top_left = Zed_edit.new_cursor editor
  } in
  let cursor = cursor t in
  let line = React.S.changes (Zed_cursor.line cursor) in
  iter (fun i ->
      let b_start = Zed_cursor.get_line t.top_left in
      let b_stop  = b_start + t.box.lines - 1 in
      let lines = Zed_edit.lines t.editor in
      if i < b_start
      then Zed_cursor.goto t.top_left (Zed_lines.line_start lines i)
      else if i > b_stop
      then Zed_cursor.goto t.top_left (Zed_lines.line_start lines (i - t.box.lines + 1))
      else ()
    ) line;
  t


let changes t =
  let cursor = Zed_edit.cursor t.context in
  Zed_edit.update t.editor [cursor;t.top_left]

let cursor_position t =
  React.S.l2 (fun (line,col) (startline,_) ->
      line - startline,col)
    (Zed_cursor.coordinates (cursor t))
    (Zed_cursor.coordinates t.top_left)

let get_lines t =
  let b_start = Zed_cursor.get_line t.top_left in
  let b_stop  = b_start + t.box.lines - 1 in
  let rec loop acc a b =
    if a > b
    then b_start,List.rev acc
    else loop (try (Some (Zed_edit.get_line t.editor a)) :: acc with _ -> None :: acc)
        (succ a) b in
  loop [] b_start b_stop


let line_start t = Zed_cursor.get_line t.top_left
let line_stop t  =
  let b_start = Zed_cursor.get_line t.top_left in
  b_start + t.box.lines - 1
