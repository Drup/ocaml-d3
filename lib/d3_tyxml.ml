open D3

module type S = sig
  type t
end

(* module Xml = struct *)

(*   module W = Xml_wrap.NoWrap *)

(*   type 'a wrap = 'a *)
(*   type 'a list_wrap = 'a list *)

(*   type uri = string *)
(*   let uri_of_string s = s *)
(*   let string_of_uri s = s *)
(*   type aname = string *)

(*   class type biggest_event = object *)
(*     inherit Dom_html.event *)
(*     (\* inherit Dom_html.mouseEvent *\) *)
(*     (\* inherit Dom_html.keyboardEvent *\) *)
(*   end *)

(*   type biggest_event_handler = (biggest_event, unit) E.handler *)
(*   type event_handler = (Dom_html.event, unit) E.handler *)
(*   type mouse_event_handler = (Dom_html.event, unit) E.handler *)
(*   type keyboard_event_handler = (Dom_html.event, unit) E.handler *)
(*   type attrib_k = *)
(*     | Event of biggest_event_handler *)
(*     | Attr of string W.t *)
(*   type attrib = aname * attrib_k *)

(*   let attr f name s : attrib = name,Attr s *)

(*   let float_attrib = attr Xml_print.string_of_number *)
(*   let int_attrib = attr string_of_int *)
(*   let string_attrib = attr (fun x -> x) *)
(*   let space_sep_attrib = attr (String.concat " ") *)
(*   let comma_sep_attrib = attr (String.concat ",") *)

(*   let uri_attrib = attr uri_of_string *)
(*   let uris_attrib = attr (String.concat " ") *)

(*   let event_handler_attrib name value = *)
(*     name,Event (value :> biggest_event_handler) *)
(*   let mouse_event_handler_attrib name (value : mouse_event_handler) = *)
(*     name,Event (value :> biggest_event_handler) *)
(*   let keyboard_event_handler_attrib name (value : keyboard_event_handler) = *)
(*     name,Event (value :> biggest_event_handler) *)

(*   let attr_to_d3 (aname, v) = match v with *)
(*     | Attr v -> *)
(*       str D3.attr aname v *)
(*     | Event f -> *)
(*       E.handle aname (fun e _ i -> f e () i) *)


(*   type elt = { d3 : 'a . ('a,'a) D3.t } *)
(*   type ename = string *)

(*   let empty () = { d3 = D3.update } *)
(*   let comment _ = { d3 = D3.update } *)

(*   let pcdata s = *)
(*     let f _ _ _ = s in *)
(*     let d3 x = D3.text f x in *)
(*     { d3 } *)
(*   let encodedpcdata = pcdata *)

(*   let entity e = *)
(*     let entity = Dom_html.decode_html_entities (Js.string ("&" ^ e ^ ";")) in *)
(*     pcdata @@ Js.to_string entity *)

(*   let leaf ?(a=[]) elem = *)
(*     let attrs s = List.fold_right attr_to_d3 a s in *)
(*     let d3 s = (D3.append elem |- attrs) s in *)
(*     { d3 } *)

(*   let node ?(a=[]) elem children = *)
(*     let attrs s = List.fold_right attr_to_d3 a s in *)
(*     let children s = List.fold_right (fun x -> x.d3) children s in *)
(*     let d3 s = (D3.append elem |- attrs |- children) s in *)
(*     { d3 } *)

(*   let cdata s = pcdata s *)
(*   let cdata_script s = cdata s *)
(*   let cdata_style s = cdata s *)

(* end *)


module type XML = sig
  type data
  include Xml_sigs.T
    with type uri = string
     and type elt = (data, data) D3.t
     and type 'a W.attr = (data, 'a) D3.fn
     and type 'a W.t = 'a
     and type ('a,'b) W.ft = 'a -> 'b
     and type 'a W.tlist = 'a list
     and type event_handler = (Dom_html.event, unit) D3.E.handler
     and type mouse_event_handler = (Dom_html.event, unit) D3.E.handler
     and type keyboard_event_handler = (Dom_html.event, unit) D3.E.handler
end

module Xml_data (M : S) : XML with type data = M.t = struct
  type data = M.t

  module W = struct
    type 'a attr = (M.t, 'a) D3.fn
    let return x _ _ _ = x

    type ('a,'b) ft = 'a -> 'b
    let fmap f (fn : 'a attr) : 'b attr =
      fun node data i -> f @@ fn node data i

    type 'a t = 'a
    type 'a tlist = 'a list
    let nil () = []
    let singleton x = [ x ]
    let cons x l = x :: l
    let append = List.append

  end

  type 'a wrap = 'a
  type 'a list_wrap = 'a list
  type 'a attr_wrap = 'a W.attr

  type uri = string
  let uri_of_string s = s
  let string_of_uri s = s
  type aname = string

  class type biggest_event = object
    inherit Dom_html.event
    (* inherit Dom_html.mouseEvent *)
    (* inherit Dom_html.keyboardEvent *)
  end

  type biggest_event_handler = (biggest_event, unit) E.handler
  type event_handler = (Dom_html.event, unit) E.handler
  type mouse_event_handler = (Dom_html.event, unit) E.handler
  type keyboard_event_handler = (Dom_html.event, unit) E.handler
  type attrib_k =
    | Event of biggest_event_handler
    | Attr of string W.attr
  type attrib = aname * attrib_k

  let attr f name fn : attrib =
    name,Attr (fun node data i -> f @@ fn node data i)

  let float_attrib = attr Xml_print.string_of_number
  let int_attrib = attr string_of_int
  let string_attrib = attr (fun x -> x)
  let space_sep_attrib = attr (String.concat " ")
  let comma_sep_attrib = attr (String.concat ",")

  let uri_attrib = attr uri_of_string
  let uris_attrib = attr (String.concat " ")

  let event_handler_attrib name value =
    name,Event (value :> biggest_event_handler)
  let mouse_event_handler_attrib name (value : mouse_event_handler) =
    name,Event (value :> biggest_event_handler)
  let keyboard_event_handler_attrib name (value : keyboard_event_handler) =
    name,Event (value :> biggest_event_handler)

  let attr_to_d3 (aname, v) = match v with
    | Attr f ->
      D3.attr aname f
    | Event f ->
      E.handle aname (fun e _ i -> f e () i)


  type elt = (M.t, M.t) D3.t
  type ename = string

  let empty () = D3.update
  let comment _ = D3.update

  (* D3.text remove the other children of the parent node.
     This is not the good semantic for pcdata, so we wrap it up in a span.
  *)
  let pcdata fn = D3.append "span" |. D3.text fn
  let encodedpcdata = pcdata

  let entity e =
    let entity = Dom_html.decode_html_entities (Js.string ("&" ^ e ^ ";")) in
    pcdata @@ fun _ _ _ -> Js.to_string entity

  let leaf ?(a=[]) elem =
    let attrs = List.map attr_to_d3 a in
    D3.append elem |. seq attrs

  let node ?(a=[]) elem children =
    let attrs = List.map attr_to_d3 a in
    D3.append elem |. seq attrs |- seq children

  let cdata s = pcdata (fun _ _ _ -> s)
  let cdata_script s = cdata s
  let cdata_style s = cdata s

end

module Svg (M: S) = Svg_f.Make(Xml_data(M))
module Html5 (M: S) = Html5_f.Make(Xml_data(M))(Svg(M))

module type Html5 = sig
  module M : sig type t end
  include Html5_sigs.Make(Xml_data(M))(Svg(M)).T
end

let html (type data) (f : (module Html5 with type M.t = data) -> _) =
  let module M = struct type t = data end in
  let module H = struct module M = M include Html5(M) end in
  f (module H)
