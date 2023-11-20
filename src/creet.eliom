[%%client
open Eliom_content
open Html.D
open Js_of_ocaml
open Js_of_ocaml_lwt
open Lwt_js_events

    type state = Healthy | Sick | Berserk | Mean | Dead

	type direction = {
			vertical: float;
			horizontal: float;
	}

    type creet = {
        elt: Html_types.div elt;
        dom : Dom_html.divElement Js.t;
		mutable grab : bool;
        mutable status: state;
        mutable margin_top: float;
        mutable margin_left: float;
        mutable size: float;
        mutable speed: float;
		mutable dir: direction;
		mutable state_counter: int;
		mutable probability: float;
    }

    let _into_px number = Js.string (Printf.sprintf "%fpx" number)

	let default_speed = 0.5

	let update_size creet size =
		creet.size <- size;
		creet.dom##.style##.height := _into_px creet.size;
		creet.dom##.style##.width := _into_px creet.size


    let set_background_image creet =
        let img_url = match creet.status with
            | Healthy -> "../images/run_sonic.gif"
            | Sick -> "../images/maladev1.gif"
            | Berserk -> "../images/berserkv1.gif"
            | Mean -> "../images/robotnic.gif"
            | Dead -> "../images/bombv1.gif"
        in
        creet.dom##.style##.backgroundImage := Js.string (Printf.sprintf "url('%s')" img_url)

	let nursing creet =
		match creet.status with
		| Sick ->
			creet.speed <- creet.speed *. 1.25;
			creet.status <- Healthy;
			creet.state_counter <- -1;
			set_background_image creet;
			creet
		| Berserk ->
			creet.speed <- creet.speed *. 1.25;
			creet.status <- Healthy;
			creet.state_counter <- -1;
			update_size creet (50.);
			set_background_image creet;
			creet
		| Mean ->
			creet.status <- Healthy;
			creet.state_counter <- -1;
			update_size creet (50.);
			set_background_image creet;
			creet
		| _ -> creet 

    let change_status_randomly creet =
        let chance = Random.int 100 in
        match creet.status with
        | Healthy ->
            creet.speed <- creet.speed *. 0.85;
            creet.state_counter <- 800;
            if chance < 80 then begin
                creet.status <- Sick;
			end
            else if chance < 90 then begin
                creet.status <- Berserk;
                set_background_image creet;
            end
            else begin
                creet.status <- Mean;
				(* Reduce mean creet size by 15% *)
				update_size creet (creet.size *. 0.85);
				creet.speed<- default_speed *. 1.10;
                set_background_image creet;
            end;
            creet
        | _ -> creet  (* s'il est deja malade il ne change pas de status*)


    let random_direction creet =
        let vertical_chances = match creet with 
			| None -> 50.
			| Some creet ->
				match creet.status with
				| Healthy -> creet.probability
				| Berserk -> 40.
				| _ -> 50.
		in
		{
			vertical   = if Random.float 100. <= vertical_chances then -1. else 1.;
            horizontal = if Random.float 100. <= 50. then -1. else 1.;
		}

    let nul_direction () = { vertical = 0.; horizontal = 0. }

	
	let distance creet_one creet_two =
	    let x = creet_one.margin_top -. creet_two.margin_top in
	    let y = creet_one.margin_left -. creet_two.margin_left in
		let dist = Float.sqrt (x *. x +. y *. y) in
		dist
	
	let intersect creet_one creet_two =
		let distance = distance creet_one creet_two in
		let creet_delta = creet_one.size /. 2. +. creet_two.size /. 2. in
		distance < creet_delta
	
	let rec find x lst =
    	match lst with
    	| [] -> raise (Failure "Not Found")
    	| h :: t -> if x = h then 0 else 1 + find x t

	let min_list lst = List.fold_left min (List.hd lst) (List.tl lst)
	
	let find_nearest_creet creet creets_list =
		if List.length creets_list == 0 then
			None
		else begin
			let distances = List.map(distance creet) creets_list in
			let min_dist = min_list distances in
			let index = find min_dist distances in
			let nearest = List.nth creets_list index in
			Some nearest
		end

	let dir_nearest_creet creet creets_list =
		let nearest_opt = find_nearest_creet creet creets_list in
		let dir = match nearest_opt with
			| None -> random_direction (Some creet)
			| Some nearest ->
				let distance = distance creet nearest in
				let x = nearest.margin_top -. creet.margin_top in
				let y = nearest.margin_left -. creet.margin_left in
	    		{
						vertical   = x /. distance;
						horizontal = y /. distance;
				} 
		in
		dir

	let event_mouse creet event =
		Firebug.console##log event;
		let container = Js.Opt.get (Dom_html.document##getElementById (Js.string "miContenedor"))
						(fun () -> assert false) in
		let container_rect = container##getBoundingClientRect in

		let container_width = Js.Optdef.get container_rect##.width (fun () -> assert false) in
		let container_height = Js.Optdef.get container_rect##.height (fun () -> assert false) in

		let mouse_x = (float_of_int event##.clientX) -. container_rect##.left in
		let mouse_y = (float_of_int event##.clientY) -. container_rect##.top -. 80. in

		let creet_half_width = creet.size /. 2. in
		let creet_half_height = creet.size /. 2. in

		let left = mouse_x -. creet_half_width in
		let top = mouse_y -. creet_half_height in

		creet.margin_left <- max 0. (min (container_width -. creet.size) left);
		creet.margin_top <- max (-80.) (min (620. -. creet.size) top);

		creet.dom##.style##.marginLeft := _into_px creet.margin_left;
		creet.dom##.style##.marginTop := _into_px creet.margin_top


	let _handle_events creet mouse_down _ =
		if creet.status = Dead then
			Lwt.return_unit
		else begin
			creet.grab <- true;
			event_mouse creet mouse_down;
			let container = Js.Opt.get (Dom_html.document##getElementById (Js.string "miContenedor"))
							(fun () -> assert false) in
			Lwt.pick
				[
					mousemoves container (fun mouse_move _ ->
						event_mouse creet mouse_move;
						Lwt.return ());
					(let%lwt mouse_up = mouseup container in
					event_mouse creet mouse_up;
					creet.grab <- false;
					(* Aquí se llama a nursing si se cumplen las condiciones *)
					if creet.margin_top > 500. then begin
						ignore (nursing creet)
					end;
					Lwt.return ());
				]
		end

    let create () = 
        let elt = div ~a:[ a_class [ "creet" ] ] [] in
        let size = 50. in
        let creet = {
                elt;
                dom = Html.To_dom.of_div elt;
                size;
				grab = false;
                margin_top  = Random.float (620. -. size);
                margin_left = Random.float (800. -. size);
                status = Healthy;
                speed = default_speed;
				dir = random_direction None;
				state_counter = 0;
				probability = 50.;
        } in
        (* Initialise dom css components 10 is default size in px *)
        creet.dom##.style##.height := _into_px creet.size;
        creet.dom##.style##.width  := _into_px creet.size;

		creet.dom##.style##.marginTop := _into_px creet.margin_top;
        creet.dom##.style##.marginLeft := _into_px creet.margin_left;
		Lwt.async (fun () -> mousedowns creet.dom (_handle_events creet));
        creet
    
    let print_creet creet = 
        let status_str = match creet.status with
            | Healthy -> "Healthy"
            | Sick    -> "Sick"
            | Berserk -> "Berserk"
            | Mean    -> "Mean"
            | Dead    -> "Dead"
        in
        let msg = Printf.sprintf "size: %f, margin_top: %f, margin_left: %f, status: %s" 
            creet.size creet.margin_top creet.margin_left status_str in
        Firebug.console##log (Js.string msg)

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

        creet.dom##.style##.marginTop  := _into_px creet.margin_top;
        creet.dom##.style##.marginLeft := _into_px creet.margin_left;

        if creet.margin_top < -50. then (* la taille de l'image css *)
            ignore (change_status_randomly creet);
        creet
	
	let compute_creet_dir creet creets_list = 
		let dir = match creet.status with
	   	| Healthy | Berserk | Sick ->
	       if  Random.int 100 <= 3 then
	           random_direction (Some creet)
		   else
			   creet.dir
	   	| Mean ->
	       dir_nearest_creet creet creets_list
		| Dead ->
           nul_direction ()
		in
		dir

   let berserk_size creet =
		if creet.state_counter mod 50 == 0 then
			update_size creet (creet.size *. 1.05)

	
	let update_speed_and_probability creet =
		if creet.state_counter mod 1000 == 0 then
			creet.speed <- creet.speed *. 1.10;
			creet.probability <- creet.probability *. 1.0001
			



   (* Creet list contains either sick or helthy creets depending on the creet state *)
   let update creet creets_list = 
	   creet.state_counter <- creet.state_counter - 1;
	   update_speed_and_probability creet;
       if creet.state_counter == 70 then (* tiempo para morir *)
            creet.status <- Dead;
            set_background_image creet;

	   let _ = match creet.status with
	   		| Healthy -> 
				let nearest = find_nearest_creet creet creets_list in
				let intersect = match nearest with
					| None -> false
					| Some c -> intersect creet c
				in
				if intersect == true then
            		ignore (change_status_randomly creet)
	   		| Berserk -> berserk_size creet
	   		| Sick    -> ignore ()
	   		| Mean -> ignore ()
			| Dead -> ignore ()
	   in
       let dir = compute_creet_dir creet creets_list in
	   creet.dir <- dir;
	   move creet
        
(**)]
