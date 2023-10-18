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
		mutable speed: int;
	}

	let _into_px number = Js.string (Printf.sprintf "%dpx" number)

	let create () = 
		let elt = div ~a:[ a_class [ "creet" ] ] [] in
		let size = 50 in
		let creet = {
				elt;
				dom = Html.To_dom.of_div elt;
				size;
				margin_top  = Random.int (620 - size);
				margin_left = Random.int (800 - size);
				status = Healthy;
				speed = 5;
		} in
		(* Initialise dom css components 10 is default size in px *)
		creet.dom##.style##.height := _into_px creet.size;
		creet.dom##.style##.width  := _into_px creet.size;

		creet.dom##.style##.marginTop := _into_px creet.margin_top;
		creet.dom##.style##.marginLeft := _into_px creet.margin_left;

		creet
	
	let print_creet creet = 
		let status_str = match creet.status with
			| Healthy -> "Healthy"
			| Sick    -> "Sick"
			| Berserk -> "Berserk"
			| Mean    -> "Mean"
		in
		let msg = Printf.sprintf "size: %d, margin_top: %d, margin_left: %d, status: %s" 
			creet.size creet.margin_top creet.margin_left status_str in
		Firebug.console##log (Js.string msg)

	let set_background_image creet =
		let img_url = match creet.status with
			| Healthy -> "../images/run_sonic.gif"
			| Sick -> "../images/maladev1.gif"
			| Berserk -> "../images/berserkv1.gif"
			| Mean -> "../images/deathv1.gif"
		in
		creet.dom##.style##.backgroundImage := Js.string (Printf.sprintf "url('%s')" img_url)


	let random_direction () =
		let step_top  = Random.int 2 in
		let step_left = Random.int 2 in
		( (if Random.bool () = true then step_top else Int.neg step_top),
			if Random.bool () = true then step_left else Int.neg step_left )

	let change_status_randomly creet =
		let chance = Random.int 100 in
		match creet.status with
		| Healthy ->
			creet.speed <- int_of_float (float_of_int creet.speed *. 0.85);
			if chance < 80 then begin
				creet.status <- Sick;
				set_background_image creet;
			end
			else if chance < 90 then begin
				creet.status <- Berserk;
				set_background_image creet;
			end
			else begin
				creet.status <- Mean;
				set_background_image creet;
			end;
			creet
		| _ -> creet  (* s'il est deja malade il ne change pas de status*)


	let move creet = 
		let step_top, step_left = random_direction () in
		creet.margin_top  <- creet.margin_top + step_top * creet.speed;
		creet.margin_left <- creet.margin_left + step_left * creet.speed;

		creet.margin_top  <- Int.min creet.margin_top (620 - creet.size);
		creet.margin_top  <- Int.max creet.margin_top (-80);
		creet.margin_left <- Int.min creet.margin_left (800 - creet.size);
		creet.margin_left <- Int.max creet.margin_left (0);

		creet.dom##.style##.marginTop := _into_px creet.margin_top;
		creet.dom##.style##.marginLeft := _into_px creet.margin_left;

		if creet.margin_top < -50 then (* la taille de l'image css *)
      	  ignore (change_status_randomly creet);

		print_creet creet;

		creet
		
	let log = Firebug.console##log (Js.string "This is one creet");
	
(**)]
