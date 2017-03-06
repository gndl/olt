(*---------------------------------------------------------------------------
   Copyright (c) 2017 Gaëtan Dubreil. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(*---------------------------------------------------------------------------
   Copyright (c) 2017 Gaëtan Dubreil

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)

open Graphics

let neutralArea = new Area.c 0 0 0 0

let frameDuration = 0.04

type t = {
(*	mutable areas : Area.c list;*)
	mutable onKeyPressed : (char -> unit);
	mutable runtimes : (unit -> unit) list;
	mutable frametimes : (float -> unit) list;
	mutable continu : bool;
}

let g = {onKeyPressed = (fun k -> ()); runtimes = []; frametimes = []; continu = true;}
(*
let addArea (area:#Area.c) = g.areas <- area::g.areas
let addAreas areas = g.areas <- areas @ g.areas

let removeArea area = g.areas <- List.filter (fun a -> a != area) g.areas
*)
let setOnKeyPressed f = g.onKeyPressed <- f
let addRuntime f = g.runtimes <- f :: g.runtimes
let addFrametime f = g.frametimes <- f :: g.frametimes

let stop() = g.continu <- false

let getUnder x y areas =
	let rec gau = function [] -> neutralArea
		| a::t -> match a#getUnder x y with Some au -> au | None -> gau t
	in gau areas

let getNotifiedUnder x y getCallback areas =
	let rec gau = function [] -> neutralArea
		| a::t -> (
			match a#getUnder x y with Some au -> (
				match getCallback au with Some cb -> cb x y; au
				| None -> gau t)
			| None -> gau t)
	in gau areas

let run() =
	let buttonPressed = ref false in
	let mouseX = ref 0 and mouseY = ref 0 in
	let areaUnderMouse = ref neutralArea in
	let sensitiveAreas = Area.sensitives() in
	let lastFrameTime = ref 0. in
	(*
	let onButtonPressedMouseMotionEventClient =
		List.filter(fun a -> not a#onButtonPressedMouseMotion = None) Area.sensitives() in
		*)
	let buttonReleasedMouseMotionClients =
		List.filter(fun a -> match a#onButtonReleasedMouseMotion with
			| Some _ -> true | None -> false) sensitiveAreas in

	g.continu <- true;

	while g.continu do
		let ev = wait_next_event [(*Button_down; Button_up;*) Key_pressed; Mouse_motion; Poll]
		in
		if ev.keypressed then (
			let key = Graphics.read_key() in
			
			let rec ka = function [] -> false | a::t -> (
				match a#onKeyPressed with
				| None -> ka t
				| Some f -> if f key then true else ka t)
			in
			if not(ka (Area.sensitives())) then g.onKeyPressed key;
		);

		if ev.button then (
			if !buttonPressed then (
				match !areaUnderMouse#onButtonPressedMouseMotion with
				| Some f -> if !mouseX <> ev.mouse_x || !mouseY <> ev.mouse_y then
					f ev.mouse_x ev.mouse_y;
				| None -> ();
			) else (
				areaUnderMouse := getNotifiedUnder ev.mouse_x ev.mouse_y
				(fun a -> a#onButtonPressed) (Area.sensitives());
			);
		) else (
			if !buttonPressed then (
				areaUnderMouse := getNotifiedUnder ev.mouse_x ev.mouse_y
				(fun a -> a#onButtonReleased) (Area.sensitives());
			)
			else (
				if buttonReleasedMouseMotionClients <> []
					&& (!mouseX <> ev.mouse_x || !mouseY <> ev.mouse_y) then
				(
					let aum = getUnder ev.mouse_x ev.mouse_y buttonReleasedMouseMotionClients
					in
					if aum == !areaUnderMouse then (
						match !areaUnderMouse#onButtonReleasedMouseMotion with
						| Some m -> m ev.mouse_x ev.mouse_y
						| None -> ();
					)
					else ();
				);
			);
		);

		buttonPressed := ev.button;
		mouseX := ev.mouse_x;
		mouseY := ev.mouse_y;

		let time = Unix.gettimeofday() in

		if (time -. !lastFrameTime) > frameDuration then (
			lastFrameTime := time;

			let rec ft = function [] -> () | f::t -> f time; ft t
			in
			ft g.frametimes;
			
			let rec ra = function [] -> () | a::t -> a#refresh(); ra t
			in
			ra(Area.backShows());
			ra(Area.middleShows());
			ra(Area.frontShows());
(**)
			New.graphicRelease();
		(*Gc.compact();*)
		);

		let rec rt = function [] -> () | f::t -> f(); rt t in rt g.runtimes;
	done;

	(*Area.resetActives();*)
	g.frametimes <- [];
	g.runtimes <- [];
;;

