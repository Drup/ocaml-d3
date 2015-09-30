open D3
open D3_tyxml

module T = D3.Tyxml.Make(Tyxml_js.To_dom)
module H = Tyxml_js.Html5

let low_footer =
  let content =
    H.(p [
      pcdata "Created by " ;
      a ~a:[a_href "http://computationallyendowed.com"]
        [pcdata "Spiros Eliopoulos"]
    ])
  in
  static "footer"
  |. str attr "id" "info"
  |. T.html (fun _ _ _ -> content)

let (~:) s _ _ _ = s

let section =
  let module H = D3_tyxml.Html5(String) in
  let open H in
  toelt @@
  section [
    h2 [pcdata ~:"title!"] ;
    pcdata (fun _ s i -> Printf.sprintf "%s %i" s i) ;
  ]

let view =
  seq [
    selectAll "section"
    |. data (fun s _ -> [ s ; s ])
    |- nest enter [section]
    ;
    low_footer
  ]

let d3_div = H.div []
let content = H.[
    h1 [pcdata "A fabulous d3-tyxml example."] ;
    d3_div ;
  ]

let () =
  Tyxml_js.Register.body content ;
  T.run d3_div "Content" view
