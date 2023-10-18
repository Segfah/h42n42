[%%shared
open Eliom_lib
open Eliom_content
open Html.D
open Js_of_ocaml

let bueno = div ~a:[a_class ["bueno"]] []

]

[%%client
open Eliom_lib
open Eliom_content
open Html.D
open Js_of_ocaml
open Js_of_ocaml_lwt
open Creet

	let speed = ref 10.

	let rec runner creet = 
		(* Add/remove creet *)
		(* Change status *)
		(* Move creets *)
		let creet = Creet.move creet in
		let%lwt () = Lwt_js.sleep 1.0 in
		runner creet

	let play () =
	  let creet = Creet.create () in
	  Html.Manip.appendChild ~%bueno creet.elt;
	  Lwt.async (fun () -> runner creet)


(**)]

[%%server
open Eliom_content
open Html.D


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

let plant_div () =
  div ~a:[a_class ["healing-plants"]] [
      img ~a:[a_class ["plants-img"]] ~src:(make_uri ~service:(Eliom_service.static_dir ()) ["images"; "plantica.png"]) ~alt:"Cruz" ()
  ]

let hospital =
  div ~a:[a_class ["hospital"]] (List.init 4 (fun _ -> plant_div ()))

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
      let _ = [%client (play () : unit)] in
      Lwt.return
        (Eliom_tools.D.html
          ~title:"h42n42"
          ~css:[["css";"h42n42.css"]]
          ~js:[["js";"h42n42.js"]]
          Html.D.(page)))

(**)]
