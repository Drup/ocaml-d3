open D3
open React

type dims =
  { width : int; height : int signal }

(* Taken from Tyxml_js.R *)
let get_prop node name =
  if Js.Optdef.test (Js.Unsafe.get node name)
  then Some name
  else None

let iter_prop_protected node name f =
  match get_prop node name with
  | Some n -> begin try f n with _ -> () end
  | None -> ()

let signal f name signal =
  let set_signal node  _ _ =
    let name = Js.string name in
    let _ =
      S.map (fun v ->
        let v = Js.string v in
        ignore (node##setAttribute(name, v)) ;
        iter_prop_protected node name (fun name -> Js.Unsafe.set node name v)
      ) signal
    in
    S.value signal
  in
  f name set_signal

let int_signal f name s = signal f name @@ S.map string_of_int s

let view dims padding =
  let svg =
    append "svg"
    |. int attr "width"  dims.width
    |. int_signal attr "height" dims.height
  in
  let rect =
    append "rect"
    |. str attr "fill"   "black"
    |. int attr "width"  (dims.width - 2 * padding)
    |. int_signal attr "height" (S.map (fun x -> x - 2 * padding) dims.height)
    |. int attr "x"      padding
    |. int attr "y"      padding
  in
  svg <.> rect

let height, set_current = S.create 0

let () =
  let rec loop i () =
    let open Lwt.Infix in
    Lwt_js.sleep 0.01 >>= fun () ->
    let j = (i + 1) mod 150 in
    set_current (100 + j) ;
    loop j () in
  Lwt.async (loop 0)

let _ =
  run "body" () (view { width = 300; height } 20)
