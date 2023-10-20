[%%shared
open Eliom_lib
open Eliom_content
open Html.D
open Js_of_ocaml

let bueno = div ~a:[a_class ["bueno"]] []
let start_button = div ~a:[a_class ["button-start"]; a_id "start-button"] [txt "JUGAR"]
]

[%%client
open Eliom_lib
open Eliom_content
open Html.D
open Js_of_ocaml
open Js_of_ocaml_lwt
open Creet

  (* ------------  RUNNER ------------   *)

  (* Met à jour le statut de chaque 'creet' en fonction de sa santé. *)
  let updateStatuses creets_list =
      let healthy, sick = List.partition (fun creet -> creet.status == Healthy) creets_list in
      let updated_healthy = List.map (fun creet -> Creet.update creet sick) healthy in
      let updated_sick = List.map (fun creet -> Creet.update creet healthy) sick in
      updated_sick @ updated_healthy

  (* Retire de la div les 'creets' qui sont morts. *)
  let removeDead creets_list =
      List.iter (fun creet ->
          if creet.status == Dead && creet.state_counter == 0 then
              Html.Manip.removeChild ~%bueno creet.elt;
      ) creets_list;
      List.filter (fun creet -> 
          not (creet.status == Dead && creet.state_counter == 0)
      ) creets_list

  let displayLostMessage () =
      let overlay = div ~a:[a_class ["perdiste-overlay"]] [] in
      let imagen = img ~a:[a_class ["virus-gif"]] ~src:(make_uri ~service:(Eliom_service.static_dir ()) ["images"; "virus.gif"]) ~alt:"Virus" () in
      let text = div ~a:[a_class ["perdiste-text"]] [txt "Perdiste"] in
      let button_clone = div ~a:[a_class ["button-start"]] [txt "Volver a Jugar"] in

      let removeOverlayAndClass _ = 
          Dom.removeChild (Dom_html.document##.body) (Html.To_dom.of_div overlay);
          (match Dom_html.getElementById_opt "start-button" with
          | None -> ()
          | Some btn -> btn##.classList##remove (Js.string "button-des"));
          Js._true
      in

      (Html.To_dom.of_div button_clone)##.onclick := Dom_html.handler removeOverlayAndClass;
      Dom.appendChild (Html.To_dom.of_div overlay) (Html.To_dom.of_img imagen);
      Dom.appendChild (Html.To_dom.of_div overlay) (Html.To_dom.of_div text);
      Dom.appendChild (Html.To_dom.of_div overlay) (Html.To_dom.of_div button_clone);
      Dom.appendChild (Dom_html.document##.body) (Html.To_dom.of_div overlay)

  let rec runner creets_list = 
      let updated_list = updateStatuses creets_list in
      let remaining_creet_list = removeDead updated_list in

      if remaining_creet_list = [] then begin
          displayLostMessage ();
          Lwt.return_unit
      end else begin
          let%lwt () = Lwt_js.sleep 0.001 in
          runner remaining_creet_list
      end

  (* ------------  FIN RUNNER ------------   *)
	
	let creet_init () =
          let creet = Creet.create () in
          Html.Manip.appendChild ~%bueno creet.elt;
		  creet

    let play () =
      Random.self_init();
      let list = List.init 4 (fun _ -> creet_init ()) in

	  (* This creet is created just to test Mean *)
	  (* The function change status has been modified to forcefully create mean *)
      let screet = Creet.create () in
      let screet = Creet.change_status_randomly screet in
      let list = list @ [screet] in
      Html.Manip.appendChild ~%bueno screet.elt;

      Lwt.async (fun () -> runner list)
    
  (* Attache un événement de clic au bouton de démarrage *)
  let attach_start_event () =
    let button = Dom_html.getElementById_coerce "start-button" Dom_html.CoerceTo.div in

    match button with
    | None -> ()
    | Some btn -> 
      btn##.onclick := Dom_html.handler (fun _ -> 
        let () = Firebug.console##log (Js.string "clic") in
        btn##.classList##add (Js.string "button-des");
        play ();
        Js._true
      )

  let () = attach_start_event ()

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
    img ~a:[a_class ["death-cross"]] ~src:(make_uri ~service:(Eliom_service.static_dir ()) ["images"; "virus.gif"]) ~alt:"Virus" ()
  ]

let plant_div () =
  div ~a:[a_class ["doctor-container"]] [
      img ~a:[a_class ["doctor-img"]] ~src:(make_uri ~service:(Eliom_service.static_dir ()) ["images"; "doctor.gif"]) ~alt:"Doctor" ()
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
      start_button;
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
      let _ = [%client (attach_start_event () : unit)] in
      Lwt.return
        (Eliom_tools.D.html
          ~title:"h42n42"
          ~css:[["css";"h42n42.css"]]
          ~js:[["js";"h42n42.js"]]
          Html.D.(page)))

(**)]
