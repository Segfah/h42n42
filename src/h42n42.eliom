[%%shared
open Eliom_lib
open Eliom_content
open Html.D
open Js_of_ocaml
]

module H42n42_app =
  Eliom_registration.App (
  struct
    let application_name = "h42n42"
    let global_data_path = None
  end)

let river =
  div ~a:[a_class ["river"]] [
    img ~a:[a_class ["death-cross"]] ~src:(make_uri ~service:(Eliom_service.static_dir ()) ["images"; "muertecita.png"]) ~alt:"Muerte" ()
  ]

let bueno = 
  div ~a:[a_class ["bueno"]] []

let hospital =
  div ~a:[a_class ["hospital"]] [
    div ~a:[a_class ["healing-plants"]] [
        img ~a:[a_class ["plants-img"]] ~src:(make_uri ~service:(Eliom_service.static_dir ()) ["images"; "plantica.png"]) ~alt:"Cruz" ()
      ];
    div ~a:[a_class ["healing-plants"]] [
        img ~a:[a_class ["plants-img"]] ~src:(make_uri ~service:(Eliom_service.static_dir ()) ["images"; "plantica.png"]) ~alt:"Cruz" ()
      ];
    div ~a:[a_class ["healing-plants"]] [
        img ~a:[a_class ["plants-img"]] ~src:(make_uri ~service:(Eliom_service.static_dir ()) ["images"; "plantica.png"]) ~alt:"Cruz" ()
      ];
    div ~a:[a_class ["healing-plants"]] [
        img ~a:[a_class ["plants-img"]] ~src:(make_uri ~service:(Eliom_service.static_dir ()) ["images"; "plantica.png"]) ~alt:"Cruz" ()
      ]
  ]

let board =
  div ~a:[a_class [""]] [
    div ~a:[a_class ["container"]] [
      river;
      bueno;
      hospital;
    ]
  ]

let page =
  body [
    div ~a:[a_class ["wrap"]] [
      div ~a:[a_class ["title"]] [
        h1 [txt "H42N42"]
      ];
      div ~a:[a_class ["autor"]] [
        h3 [txt "corozco"]
      ];
      board;
    ]
  ]


let main_service =
  Eliom_service.create
    ~path:(Eliom_service.Path [])
    ~meth:(Eliom_service.Get Eliom_parameter.unit)
    ()

let () =
  H42n42_app.register
    ~service:main_service
    (fun () () ->
      Lwt.return
        (Eliom_tools.F.html
          ~title:"h42n42"
          ~css:[["css";"h42n42.css"]]
          ~js:[["js";"h42n42.js"]]
          Html.F.(page)))
