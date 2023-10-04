[%%shared
open Eliom_lib
open Eliom_content
open Html.D
]

module H42n42_app =
  Eliom_registration.App (
  struct
    let application_name = "h42n42"
    let global_data_path = None
  end)

let main_service =
  Eliom_service.create
    ~path:(Eliom_service.Path [])
    ~meth:(Eliom_service.Get Eliom_parameter.unit)
    ()

let board =
  div ~a:[a_class [""]] [
    div ~a:[a_class ["title"]] [
      h1 [txt "H42N42"]
    ];
    div ~a:[a_class ["title"]] [
      h3 [txt "corozco"]
    ];
    div ~a:[a_class ["container"]] [
      div ~a:[a_class ["rio"]] [
        img ~a:[a_class ["death-cross"]] ~src:(make_uri ~service:(Eliom_service.static_dir ()) ["images"; "muertecita.png"]) ~alt:"Muerte" ()
      ];
      div ~a:[a_class ["bueno"]] [];
      div ~a:[a_class ["container-enfermeria"]] [
        div ~a:[a_class ["enfermeria"]] [
            img ~a:[a_class ["enfermeria-cross"]] ~src:(make_uri ~service:(Eliom_service.static_dir ()) ["images"; "plantica.png"]) ~alt:"Cruz" ()
          ];
        div ~a:[a_class ["enfermeria"]] [
            img ~a:[a_class ["enfermeria-cross"]] ~src:(make_uri ~service:(Eliom_service.static_dir ()) ["images"; "plantica.png"]) ~alt:"Cruz" ()
          ];
        div ~a:[a_class ["enfermeria"]] [
            img ~a:[a_class ["enfermeria-cross"]] ~src:(make_uri ~service:(Eliom_service.static_dir ()) ["images"; "plantica.png"]) ~alt:"Cruz" ()
          ];
        div ~a:[a_class ["enfermeria"]] [
            img ~a:[a_class ["enfermeria-cross"]] ~src:(make_uri ~service:(Eliom_service.static_dir ()) ["images"; "plantica.png"]) ~alt:"Cruz" ()
          ]
      ]
    ]
  ]

let page =
  body [
    div ~a:[a_class ["wrapper"]] [
      board;
      div ~a:[a_class ["title"]] [
        h1 [txt "H42N42"]
      ];
    ]
  ]




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
