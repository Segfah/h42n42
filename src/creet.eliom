[%%client
open Eliom_content
open Html.D
open Js_of_ocaml
open Js_of_ocaml_lwt
open Lwt_js_events

	type state = Healthy | Sick | Berserk

	type creet = {
		elt: Html_types.div elt;
		dom : Dom_html.divElement Js.t;
		margin_top: int;
		margin_left: int;
		size: int;
	}

	let _into_px number = Js.string (Printf.sprintf "%dpx" number)

	let create () = 
		let elt = div ~a:[ a_class [ "creet" ] ] [] in
		let creet = {
				elt;
				dom = Html.To_dom.of_div elt;
				size = 50;
				margin_top = 0;
				margin_left = 0;
		} in
		(* Initialise dom css components 10 is default size in px *)
		creet.dom##.style##.height := _into_px creet.size;
		creet.dom##.style##.width  := _into_px creet.size;
		creet

	let move creet = 
		creet.margin_top = creet.margin_top + 10;
		creet.margin_left = creet.margin_left + 10;

		creet.dom##.style##.top := _into_px creet.margin_top;
		creet.dom##.style##.left := _into_px creet.margin_left;
		creet
		

	let log = Firebug.console##log (Js.string "This is one creet");
	
(**)]
