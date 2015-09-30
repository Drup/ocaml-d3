
module type S = sig
  type t
end

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

module Xml_data (M:S) : XML with type data = M.t

module Svg (M:S) : Svg_sigs.Make(Xml_data(M)).T
module Html5 (M:S) : Html5_sigs.Make(Xml_data(M))(Svg(M)).T

module type Html5 = sig
  module M : sig type t end
  include Html5_sigs.Make(Xml_data(M))(Svg(M)).T
end

val html : ((module Html5 with type M.t = 'a) -> ('a,'a) D3.t) -> ('a, 'a) D3.t
