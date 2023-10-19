[%%client
open Eliom_content
open Html.D
open Js_of_ocaml
open Js_of_ocaml_lwt
open Lwt_js_events

    type state = Healthy | Sick | Berserk | Mean

	type direction = {
			vertical: float;
			horizontal: float;
	}

    type creet = {
        elt: Html_types.div elt;
        dom : Dom_html.divElement Js.t;
        mutable status: state;
        mutable margin_top: float;
        mutable margin_left: float;
        mutable size: float;
        mutable speed: float;
		mutable dir: direction;
		mutable mov_counter: int;
    }

    let _into_px number = Js.string (Printf.sprintf "%fpx" number)

    let random_direction () =
        {
			vertical   = if Random.int 100 <= 50 then -1. else 1.;
            horizontal = if Random.int 100 <= 50 then -1. else 1.;
		}
	
	let find_nearest_creet () =
	    let dir = {
				vertical = 1.;
				horizontal = 1.;
		} in 
		dir

    let create () = 
        let elt = div ~a:[ a_class [ "creet" ] ] [] in
        let size = 50. in
        let creet = {
                elt;
                dom = Html.To_dom.of_div elt;
                size;
                margin_top  = Random.float (620. -. size);
                margin_left = Random.float (800. -. size);
                status = Healthy;
                speed = 1.;
				dir = random_direction ();
				mov_counter = 0;
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
        let msg = Printf.sprintf "size: %f, margin_top: %f, margin_left: %f, status: %s" 
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

    let change_status_randomly creet =
        let chance = Random.int 100 in
        match creet.status with
        | Healthy ->
            creet.speed <- creet.speed *. 0.85;
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
				(* Reduce mean creet size by 15% *)
				creet.size <- creet.size *. 0.85;
                set_background_image creet;
            end;
            creet
        | _ -> creet  (* s'il est deja malade il ne change pas de status*)


    let move creet = 
        let next_margin_top = creet.margin_top +. creet.dir.vertical *. creet.speed in
        let next_margin_left = creet.margin_left +. creet.dir.horizontal *. creet.speed in

        if next_margin_top >= (620. -. creet.size) || next_margin_top <= -80. then begin
			creet.margin_top <- creet.margin_top -. creet.dir.vertical *. creet.speed; 
		end
		else
			creet.margin_top <- creet.margin_top +. creet.dir.vertical *. creet.speed;


        if next_margin_left >= (800. -. creet.size) || next_margin_left <= 0. then begin
        	creet.margin_left <- creet.margin_left -. creet.dir.horizontal *. creet.speed;
		end
		else
        	creet.margin_left <- creet.margin_left +. creet.dir.horizontal *. creet.speed;


		(*
        *creet.margin_top  <- Float.min creet.margin_top (620. -. creet.size);
        *creet.margin_top  <- Float.max creet.margin_top (-80.);
        *creet.margin_left <- Float.min creet.margin_left (800. -. creet.size);
        *creet.margin_left <- Float.max creet.margin_left (0.);
		*)

        creet.dom##.style##.marginTop  := _into_px creet.margin_top;
        creet.dom##.style##.marginLeft := _into_px creet.margin_left;

        if creet.margin_top < -50. then (* la taille de l'image css *)
            ignore (change_status_randomly creet);

        print_creet creet;

        creet
	
   let inc value = value + 1

   let update creet = 
       let _ = match creet.status with
	   | Healthy | Berserk | Sick ->
		   Firebug.console##log (creet.mov_counter);
	       if  Random.int 100 <= 3 then
	           creet.dir <- random_direction ();
	   | Mean ->
	       creet.dir <- find_nearest_creet ();
	   in
	   move creet
        
    let log = Firebug.console##log (Js.string "This is one creet");
    
(**)]
