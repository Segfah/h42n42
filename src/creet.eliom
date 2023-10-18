[%%client
open Eliom_content
open Html.D
open Js_of_ocaml
open Js_of_ocaml_lwt
open Lwt_js_events

	type state = Healthy | Sick | Berserk | Mean

	type creet = {
		elt: Html_types.div elt;
		dom : Dom_html.divElement Js.t;
		mutable margin_top: int;
		mutable margin_left: int;
		mutable status: state;
		size: int;
	}

	let _into_px number = Js.string (Printf.sprintf "%dpx" number)

	let create () = 
		let elt = div ~a:[ a_class [ "creet" ] ] [] in
		let creet = {
				elt;
				dom = Html.To_dom.of_div elt;
				size = 50;
				margin_top  = Random.int (620 - 50);
				margin_left = Random.int (800 - 50);
				status = Healthy;
		} in
		(* Initialise dom css components 10 is default size in px *)
		creet.dom##.style##.height := _into_px creet.size;
		creet.dom##.style##.width  := _into_px creet.size;

		creet.dom##.style##.marginTop := _into_px creet.margin_top;
		creet.dom##.style##.marginLeft := _into_px creet.margin_left;

		creet
	
	let random_direction () =
		let step_top  = Random.int 2 in
		let step_left = Random.int 2 in
		( (if Random.bool () = true then step_top else Int.neg step_top),
			if Random.bool () = true then step_left else Int.neg step_left )
	
	let move creet = 
		let step_top, step_left = random_direction () in
		let speed = 5 in
		creet.margin_top  <- creet.margin_top + step_top * speed;
		creet.margin_left <- creet.margin_left + step_left * speed;

		creet.margin_top  <- Int.min creet.margin_top (620 - creet.size);
		creet.margin_top  <- Int.max creet.margin_top (-80);
		creet.margin_left <- Int.min creet.margin_left (800 - creet.size);
		creet.margin_left <- Int.max creet.margin_left (0);

		creet.dom##.style##.marginTop := _into_px creet.margin_top;
		creet.dom##.style##.marginLeft := _into_px creet.margin_left;
		creet
		

	let log = Firebug.console##log (Js.string "This is one creet");
	
(**)]
