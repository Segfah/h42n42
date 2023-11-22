[%%shared
open Eliom_lib
open Eliom_content
open Html.D
open Js_of_ocaml

let bueno = div ~a:[a_class ["bueno"]] []
let start_button = div ~a:[a_class ["button-start"]; a_id "start-button"] [txt "JUGAR"]
let params_checkbox =
  div ~a:[a_class ["parameters-containers"]] [
    div ~a:[a_class ["speed-checkbox"]] [
        input ~a:[a_input_type `Checkbox; a_name "speedCheckbox"; a_id "speedCheckbox"] ();
        label ~a:[a_label_for "speedCheckbox"] [txt "Velocidad x2"];
      ];
    div ~a:[a_class ["sick_checkbox"]] [
        input ~a:[a_input_type `Checkbox; a_name "sickCheckbox"; a_id "sickCheckbox"] ();
        label ~a:[a_label_for "sickCheckbox"] [txt "Personaje enfermo"];
      ]
  ]
]
 
[%%client
open Eliom_lib
open Eliom_content
open Html.D
open Js_of_ocaml
open Js_of_ocaml_lwt
open Creet

  let default_speed = ref 0.5

	let creet_init () = (
      let creet = Creet.create !default_speed in
      Html.Manip.appendChild ~%bueno creet.elt;
		  creet
  )

  (* ------------  RUNNER ------------   *)

  (* Met à jour le statut de chaque 'creet' en fonction de sa santé et de grab. *)
    let updateStatuses creets_list =
      let healthy, sick = List.partition (fun creet -> creet.status == Healthy) creets_list in
      let updated_healthy = List.map (fun creet -> if not creet.grab then Creet.update creet sick else creet) healthy in
      let updated_sick = List.map (fun creet -> if not creet.grab then Creet.update creet healthy else creet) sick in

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

  let rec runner creets_list timestamp = 
      let updated_list = updateStatuses creets_list in
      let remaining_creet_list = removeDead updated_list in
      let healthy_count = List.fold_left (fun acc c -> if c.status == Creet.Healthy then acc + 1 else acc) 0 remaining_creet_list in
      if timestamp mod 1500 == 0 && !default_speed < 1.6 then
          default_speed := !default_speed *. 1.10;
        let remaining_creet_list = 
            if healthy_count > 0 && (List.length remaining_creet_list) < 10 && timestamp mod 1000 == 0 then
                remaining_creet_list @ [creet_init ()]
            else
                remaining_creet_list
      in
      if remaining_creet_list = [] then begin
          displayLostMessage ();
          Lwt.return_unit
      end else begin
          let%lwt () = Lwt_js.sleep 0.001 in
          runner remaining_creet_list (timestamp + 1)
      end

  (* ------------  FIN RUNNER ------------   *)


  let play ~is_sick_active ~is_double_speed =
    Random.self_init();
    default_speed := if is_double_speed then 0.8 else 0.4;
    let list_ref = ref (List.init 4 (fun _ -> creet_init ())) in
    if is_sick_active then begin
      let modified_screet = Creet.create !default_speed |> Creet.change_status_randomly in
      Html.Manip.appendChild ~%bueno modified_screet.elt;
      list_ref := modified_screet :: !list_ref;
    end;

    Lwt.async (fun () -> runner !list_ref 1)
    
  (* Attache un événement de clic au bouton de démarrage *)
  let attach_start_event () =
    let button = Dom_html.getElementById_coerce "start-button" Dom_html.CoerceTo.div in
    let scheckbox = Dom_html.getElementById_coerce "speedCheckbox" Dom_html.CoerceTo.input in
    let sick_checkbox = Dom_html.getElementById_coerce "sickCheckbox" Dom_html.CoerceTo.input in

    match (button, scheckbox, sick_checkbox) with
    | (Some btn, Some chk, Some sick_chk) ->
      btn##.onclick := Dom_html.handler (fun _ ->
        let is_double_speed = Js.to_bool chk##.checked in
        let is_sick_active = Js.to_bool sick_chk##.checked in
        btn##.classList##add (Js.string "button-des");
        play ~is_sick_active ~is_double_speed;

        (* Resto del código si es necesario *)

        Js._true
      )
    | _ -> ()

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
    div ~a:[a_class ["container"]; a_id("miContenedor")] [
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
      params_checkbox;
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
