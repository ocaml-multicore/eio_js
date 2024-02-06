(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Vincent Balat
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

open Js_of_ocaml

let async ~sw f =
  Eio.Fiber.fork ~sw @@ fun () ->
  Eio_js.yield ();
  f ()

let opt_map f = function None -> None | Some x -> Some (f x)

let make_event event_kind ?use_capture ?passive target =
  let el = ref Js.null in
  Eio_js_backend.await
    ~setup:(fun ~resolve ~reject:_ ->
      let cancel () = Js.Opt.iter !el Dom_html.removeEventListener in
      el :=
        Js.some
          (Dom.addEventListenerWithOptions
             ?capture:(opt_map Js.bool use_capture)
             ?passive:(opt_map Js.bool passive) target event_kind
             (Dom_html.handler (fun (ev : #Dom_html.event Js.t) ->
                  cancel ();
                  resolve ev;
                  Js.bool true))
             (* true because we do not want to prevent default ->
                                  the user can use the preventDefault function
                                  above. *));
      cancel)
    ~cancel:(fun cancel -> cancel ())

type cancel = { cancel : unit -> unit }

let with_error_log f x =
  try f x with e -> Firebug.console##log (Js.string (Printexc.to_string e))

let seq_loop ~sw evh ?(cancel_handler = false) ?use_capture ?passive target
    handler =
  let cancel, cancel_resolver = Eio.Promise.create () in
  let wait_for_cancellation () = Eio.Promise.await cancel in
  let cancel =
    { cancel = (fun () -> Eio.Promise.resolve cancel_resolver ()) }
  in
  (if cancel_handler then
     let rec aux () =
       let e = evh ?use_capture ?passive target in
       with_error_log (handler e) cancel;
       aux ()
     in
     Eio.Fiber.fork ~sw (fun () -> Eio.Fiber.first wait_for_cancellation aux)
   else
     let rec aux () =
       let e =
         Eio.Fiber.first
           (fun () ->
             wait_for_cancellation ();
             None)
           (fun () -> Some (evh ?use_capture ?passive target))
       in
       Option.iter
         (fun e ->
           with_error_log (handler e) cancel;
           aux ())
         e
     in
     Eio.Fiber.fork ~sw aux);
  cancel

let async_loop ~sw evh ?use_capture ?passive target handler =
  let cancel, cancel_resolver = Eio.Promise.create () in
  let wait_for_cancellation () = Eio.Promise.await cancel in
  let cancel =
    { cancel = (fun () -> Eio.Promise.resolve cancel_resolver ()) }
  in
  let rec aux () =
    let e =
      Eio.Fiber.first
        (fun () ->
          wait_for_cancellation ();
          None)
        (fun () -> Some (evh ?use_capture ?passive target))
    in
    Option.iter
      (fun e ->
        Eio.Fiber.fork ~sw (fun () -> with_error_log (handler e) cancel);
        aux ())
      e
  in
  Eio.Fiber.fork ~sw aux;
  cancel

(*
let buffered_loop
    evh
    ?(cancel_handler = false)
    ?(cancel_queue = true)
    ?use_capture
    ?passive
    target
    handler =
  let cancelled = ref false in
  let queue = ref [] in
  let cur = ref (Lwt.fail (Failure "Lwt_js_event")) in
  let cur_handler = ref (Lwt.return ()) in
  let lt, _lw = Lwt.task () in
  let spawn = Lwt_condition.create () in
  Lwt.on_cancel lt (fun () ->
      Lwt.cancel !cur;
      if cancel_handler then Lwt.cancel !cur_handler;
      if cancel_queue then queue := [];
      cancelled := true);
  let rec spawner () =
    if not !cancelled
    then (
      let t = evh ?use_capture ?passive target in
      cur := t;
      t
      >>= fun e ->
      queue := e :: !queue;
      Lwt_condition.signal spawn ();
      spawner ())
    else Lwt.return ()
  in
  let rec runner () =
    cur_handler := Lwt.return ();
    if not !cancelled
    then (
      match !queue with
      | [] -> Lwt_condition.wait spawn >>= runner
      | e :: tl ->
          queue := tl;
          cur_handler := with_error_log (handler e) lt;
          !cur_handler >>= runner)
    else Lwt.return ()
  in
  Lwt.async (catch_cancel spawner);
  Lwt.async runner;
  lt
*)

let func_limited_loop ~sw event limited_func ?use_capture ?passive target
    handler =
  let count = ref 0 in
  async_loop ~sw event ?use_capture ?passive target (fun ev lt ->
      incr count;
      let nb = !count in
      limited_func ();
      if !count = nb then handler ev lt)

let limited_loop ~sw event ?(elapsed_time = 0.1) =
  func_limited_loop ~sw event (fun () -> Eio_js.sleep elapsed_time)

let click ?use_capture ?passive target =
  make_event Dom_html.Event.click ?use_capture ?passive target

let copy ?use_capture ?passive target =
  make_event Dom_html.Event.copy ?use_capture ?passive target

let cut ?use_capture ?passive target =
  make_event Dom_html.Event.cut ?use_capture ?passive target

let paste ?use_capture ?passive target =
  make_event Dom_html.Event.paste ?use_capture ?passive target

let dblclick ?use_capture ?passive target =
  make_event Dom_html.Event.dblclick ?use_capture ?passive target

let mousedown ?use_capture ?passive target =
  make_event Dom_html.Event.mousedown ?use_capture ?passive target

let mouseup ?use_capture ?passive target =
  make_event Dom_html.Event.mouseup ?use_capture ?passive target

let mouseover ?use_capture ?passive target =
  make_event Dom_html.Event.mouseover ?use_capture ?passive target

let mousemove ?use_capture ?passive target =
  make_event Dom_html.Event.mousemove ?use_capture ?passive target

let mouseout ?use_capture ?passive target =
  make_event Dom_html.Event.mouseout ?use_capture ?passive target

let keypress ?use_capture ?passive target =
  make_event Dom_html.Event.keypress ?use_capture ?passive target

let keydown ?use_capture ?passive target =
  make_event Dom_html.Event.keydown ?use_capture ?passive target

let keyup ?use_capture ?passive target =
  make_event Dom_html.Event.keyup ?use_capture ?passive target

let change ?use_capture ?passive target =
  make_event Dom_html.Event.change ?use_capture ?passive target

let input ?use_capture ?passive target =
  make_event Dom_html.Event.input ?use_capture ?passive target

let timeupdate ?use_capture ?passive target =
  make_event Dom_html.Event.timeupdate ?use_capture ?passive target

let dragstart ?use_capture ?passive target =
  make_event Dom_html.Event.dragstart ?use_capture ?passive target

let dragend ?use_capture ?passive target =
  make_event Dom_html.Event.dragend ?use_capture ?passive target

let dragenter ?use_capture ?passive target =
  make_event Dom_html.Event.dragenter ?use_capture ?passive target

let dragover ?use_capture ?passive target =
  make_event Dom_html.Event.dragover ?use_capture ?passive target

let dragleave ?use_capture ?passive target =
  make_event Dom_html.Event.dragleave ?use_capture ?passive target

let drag ?use_capture ?passive target =
  make_event Dom_html.Event.drag ?use_capture ?passive target

let drop ?use_capture ?passive target =
  make_event Dom_html.Event.drop ?use_capture ?passive target

let focus ?use_capture ?passive target =
  make_event Dom_html.Event.focus ?use_capture ?passive target

let blur ?use_capture ?passive target =
  make_event Dom_html.Event.blur ?use_capture ?passive target

let scroll ?use_capture ?passive target =
  make_event Dom_html.Event.scroll ?use_capture ?passive target

let submit ?use_capture ?passive target =
  make_event Dom_html.Event.submit ?use_capture ?passive target

let select ?use_capture ?passive target =
  make_event Dom_html.Event.select ?use_capture ?passive target

let abort ?use_capture ?passive target =
  make_event Dom_html.Event.abort ?use_capture ?passive target

let error ?use_capture ?passive target =
  make_event Dom_html.Event.error ?use_capture ?passive target

let load ?use_capture ?passive target =
  make_event Dom_html.Event.load ?use_capture ?passive target

let canplay ?use_capture ?passive target =
  make_event Dom_html.Event.canplay ?use_capture ?passive target

let canplaythrough ?use_capture ?passive target =
  make_event Dom_html.Event.canplaythrough ?use_capture ?passive target

let durationchange ?use_capture ?passive target =
  make_event Dom_html.Event.durationchange ?use_capture ?passive target

let emptied ?use_capture ?passive target =
  make_event Dom_html.Event.emptied ?use_capture ?passive target

let ended ?use_capture ?passive target =
  make_event Dom_html.Event.ended ?use_capture ?passive target

let loadeddata ?use_capture ?passive target =
  make_event Dom_html.Event.loadeddata ?use_capture ?passive target

let loadedmetadata ?use_capture ?passive target =
  make_event Dom_html.Event.loadedmetadata ?use_capture ?passive target

let loadstart ?use_capture ?passive target =
  make_event Dom_html.Event.loadstart ?use_capture ?passive target

let pause ?use_capture ?passive target =
  make_event Dom_html.Event.pause ?use_capture ?passive target

let play ?use_capture ?passive target =
  make_event Dom_html.Event.play ?use_capture ?passive target

let playing ?use_capture ?passive target =
  make_event Dom_html.Event.playing ?use_capture ?passive target

let ratechange ?use_capture ?passive target =
  make_event Dom_html.Event.ratechange ?use_capture ?passive target

let seeked ?use_capture ?passive target =
  make_event Dom_html.Event.seeked ?use_capture ?passive target

let seeking ?use_capture ?passive target =
  make_event Dom_html.Event.seeking ?use_capture ?passive target

let stalled ?use_capture ?passive target =
  make_event Dom_html.Event.stalled ?use_capture ?passive target

let suspend ?use_capture ?passive target =
  make_event Dom_html.Event.suspend ?use_capture ?passive target

let volumechange ?use_capture ?passive target =
  make_event Dom_html.Event.volumechange ?use_capture ?passive target

let waiting ?use_capture ?passive target =
  make_event Dom_html.Event.waiting ?use_capture ?passive target

(* special case for mousewheel, because it depends on the browser *)
let mousewheel ?use_capture ?passive target =
  Eio_js_backend.await
    ~setup:(fun ~resolve ~reject:_ ->
      let el = ref Js.null in
      let cancel () = Js.Opt.iter !el Dom_html.removeEventListener in
      el :=
        Js.some
          (Dom_html.addMousewheelEventListenerWithOptions
             ?capture:(opt_map Js.bool use_capture)
             ?passive:(opt_map Js.bool passive) target
             (fun (ev : #Dom_html.event Js.t) ~dx ~dy ->
               Firebug.console##log ev;
               cancel ();
               resolve (ev, (dx, dy));
               Js.bool true)
             (* true because we do not want to prevent default ->
                               the user can use the preventDefault function
                               above. *));
      cancel)
    ~cancel:(fun cancel -> cancel ())

let wheel ?use_capture ?passive target =
  make_event Dom_html.Event.wheel ?use_capture ?passive target

let touchstart ?use_capture ?passive target =
  make_event Dom_html.Event.touchstart ?use_capture ?passive target

let touchmove ?use_capture ?passive target =
  make_event Dom_html.Event.touchmove ?use_capture ?passive target

let touchend ?use_capture ?passive target =
  make_event Dom_html.Event.touchend ?use_capture ?passive target

let touchcancel ?use_capture ?passive target =
  make_event Dom_html.Event.touchcancel ?use_capture ?passive target

let lostpointercapture ?use_capture ?passive target =
  make_event Dom_html.Event.lostpointercapture ?use_capture ?passive target

let gotpointercapture ?use_capture ?passive target =
  make_event Dom_html.Event.gotpointercapture ?use_capture ?passive target

let pointerenter ?use_capture ?passive target =
  make_event Dom_html.Event.pointerenter ?use_capture ?passive target

let pointercancel ?use_capture ?passive target =
  make_event Dom_html.Event.pointercancel ?use_capture ?passive target

let pointerdown ?use_capture ?passive target =
  make_event Dom_html.Event.pointerdown ?use_capture ?passive target

let pointerleave ?use_capture ?passive target =
  make_event Dom_html.Event.pointerleave ?use_capture ?passive target

let pointermove ?use_capture ?passive target =
  make_event Dom_html.Event.pointermove ?use_capture ?passive target

let pointerout ?use_capture ?passive target =
  make_event Dom_html.Event.pointerout ?use_capture ?passive target

let pointerover ?use_capture ?passive target =
  make_event Dom_html.Event.pointerover ?use_capture ?passive target

let pointerup ?use_capture ?passive target =
  make_event Dom_html.Event.pointerup ?use_capture ?passive target

let transitionend ?use_capture ?passive elt =
  make_event Dom_html.Event.transitionend ?use_capture ?passive elt

let transitionstart ?use_capture ?passive elt =
  make_event Dom_html.Event.transitionstart ?use_capture ?passive elt

let transitionrun ?use_capture ?passive elt =
  make_event Dom_html.Event.transitionrun ?use_capture ?passive elt

let transitioncancel ?use_capture ?passive elt =
  make_event Dom_html.Event.transitioncancel ?use_capture ?passive elt

let clicks ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw click ?cancel_handler ?use_capture ?passive t

let copies ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw copy ?cancel_handler ?use_capture ?passive t

let cuts ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw cut ?cancel_handler ?use_capture ?passive t

let pastes ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw paste ?cancel_handler ?use_capture ?passive t

let dblclicks ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw dblclick ?cancel_handler ?use_capture ?passive t

let mousedowns ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw mousedown ?cancel_handler ?use_capture ?passive t

let mouseups ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw mouseup ?cancel_handler ?use_capture ?passive t

let mouseovers ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw mouseover ?cancel_handler ?use_capture ?passive t

let mousemoves ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw mousemove ?cancel_handler ?use_capture ?passive t

let mouseouts ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw mouseout ?cancel_handler ?use_capture ?passive t

let keypresses ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw keypress ?cancel_handler ?use_capture ?passive t

let keydowns ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw keydown ?cancel_handler ?use_capture ?passive t

let keyups ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw keyup ?cancel_handler ?use_capture ?passive t

let changes ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw change ?cancel_handler ?use_capture ?passive t

let inputs ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw input ?cancel_handler ?use_capture ?passive t

let timeupdates ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw timeupdate ?cancel_handler ?use_capture ?passive t

let dragstarts ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw dragstart ?cancel_handler ?use_capture ?passive t

let dragends ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw dragend ?cancel_handler ?use_capture ?passive t

let dragenters ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw dragenter ?cancel_handler ?use_capture ?passive t

let dragovers ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw dragover ?cancel_handler ?use_capture ?passive t

let dragleaves ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw dragleave ?cancel_handler ?use_capture ?passive t

let drags ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw drag ?cancel_handler ?use_capture ?passive t

let drops ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw drop ?cancel_handler ?use_capture ?passive t

let mousewheels ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw mousewheel ?cancel_handler ?use_capture ?passive t

let wheels ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw wheel ?cancel_handler ?use_capture ?passive t

let touchstarts ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw touchstart ?cancel_handler ?use_capture ?passive t

let touchmoves ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw touchmove ?cancel_handler ?use_capture ?passive t

let touchends ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw touchend ?cancel_handler ?use_capture ?passive t

let touchcancels ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw touchcancel ?cancel_handler ?use_capture ?passive t

let focuses ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw focus ?cancel_handler ?use_capture ?passive t

let blurs ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw blur ?cancel_handler ?use_capture ?passive t

let scrolls ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw scroll ?cancel_handler ?use_capture ?passive t

let submits ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw submit ?cancel_handler ?use_capture ?passive t

let selects ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw select ?cancel_handler ?use_capture ?passive t

let aborts ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw abort ?cancel_handler ?use_capture ?passive t

let errors ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw error ?cancel_handler ?use_capture ?passive t

let loads ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw load ?cancel_handler ?use_capture ?passive t

let canplays ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw canplay ?cancel_handler ?use_capture ?passive t

let canplaythroughs ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw canplaythrough ?cancel_handler ?use_capture ?passive t

let durationchanges ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw durationchange ?cancel_handler ?use_capture ?passive t

let emptieds ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw emptied ?cancel_handler ?use_capture ?passive t

let endeds ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw ended ?cancel_handler ?use_capture ?passive t

let loadeddatas ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw loadeddata ?cancel_handler ?use_capture ?passive t

let loadedmetadatas ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw loadedmetadata ?cancel_handler ?use_capture ?passive t

let loadstarts ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw loadstart ?cancel_handler ?use_capture ?passive t

let pauses ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw pause ?cancel_handler ?use_capture ?passive t

let plays ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw play ?cancel_handler ?use_capture ?passive t

let playings ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw playing ?cancel_handler ?use_capture ?passive t

let ratechanges ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw ratechange ?cancel_handler ?use_capture ?passive t

let seekeds ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw seeked ?cancel_handler ?use_capture ?passive t

let seekings ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw seeking ?cancel_handler ?use_capture ?passive t

let stalleds ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw stalled ?cancel_handler ?use_capture ?passive t

let suspends ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw suspend ?cancel_handler ?use_capture ?passive t

let volumechanges ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw volumechange ?cancel_handler ?use_capture ?passive t

let waitings ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw waiting ?cancel_handler ?use_capture ?passive t

let lostpointercaptures ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw lostpointercapture ?cancel_handler ?use_capture ?passive t

let gotpointercaptures ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw gotpointercapture ?cancel_handler ?use_capture ?passive t

let pointerenters ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw pointerenter ?cancel_handler ?use_capture ?passive t

let pointercancels ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw pointercancel ?cancel_handler ?use_capture ?passive t

let pointerdowns ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw pointerdown ?cancel_handler ?use_capture ?passive t

let pointerleaves ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw pointerleave ?cancel_handler ?use_capture ?passive t

let pointermoves ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw pointermove ?cancel_handler ?use_capture ?passive t

let pointerouts ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw pointerout ?cancel_handler ?use_capture ?passive t

let pointerovers ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw pointerover ?cancel_handler ?use_capture ?passive t

let pointerups ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw pointerup ?cancel_handler ?use_capture ?passive t

let transitionends ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw transitionend ?cancel_handler ?use_capture ?passive t

let transitionstarts ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw transitionstart ?cancel_handler ?use_capture ?passive t

let transitionruns ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw transitionrun ?cancel_handler ?use_capture ?passive t

let transitioncancels ~sw ?cancel_handler ?use_capture ?passive t =
  seq_loop ~sw transitioncancel ?cancel_handler ?use_capture ?passive t

let request_animation_frame () =
  Eio_js_backend.await
    ~setup:(fun ~resolve ~reject:_ ->
      Dom_html.window##requestAnimationFrame
        (Js.wrap_callback (fun (_ : float) -> resolve ())))
    ~cancel:(fun id -> Dom_html.window##cancelAnimationFrame id)

let onload () = make_event Dom_html.Event.load Dom_html.window

let domContentLoaded =
  let complete = Js.string "complete" in
  let doc = Dom_html.window##.document in
  fun () ->
    if doc##.readyState <> complete then
      let rec aux () =
        let _ : _ Js.t = make_event (Dom.Event.make "readystatechange") doc in
        if doc##.readyState <> complete then aux ()
      in
      aux ()

let onunload () = make_event Dom_html.Event.unload Dom_html.window
let onbeforeunload () = make_event Dom_html.Event.beforeunload Dom_html.window
let onresize () = make_event Dom_html.Event.resize Dom_html.window

let onorientationchange () =
  make_event Dom_html.Event.orientationchange Dom_html.window

let onpopstate () = make_event Dom_html.Event.popstate Dom_html.window
let onhashchange () = make_event Dom_html.Event.hashchange Dom_html.window

let onorientationchange_or_onresize () =
  Eio.Fiber.first onresize onorientationchange

let onresizes ~sw t =
  seq_loop ~sw (fun ?use_capture:_ ?passive:_ () -> onresize ()) () t

let onorientationchanges ~sw t =
  seq_loop ~sw (fun ?use_capture:_ ?passive:_ () -> onorientationchange ()) () t

let onpopstates ~sw t =
  seq_loop ~sw (fun ?use_capture:_ ?passive:_ () -> onpopstate ()) () t

let onhashchanges ~sw t =
  seq_loop ~sw (fun ?use_capture:_ ?passive:_ () -> onhashchange ()) () t

let onorientationchanges_or_onresizes ~sw t =
  seq_loop ~sw
    (fun ?use_capture:_ ?passive:_ () -> onorientationchange_or_onresize ())
    () t

let limited_onresizes ~sw ?elapsed_time t =
  limited_loop ~sw
    (fun ?use_capture:_ ?passive:_ () -> onresize ())
    ?elapsed_time () t

let limited_onorientationchanges ~sw ?elapsed_time t =
  limited_loop ~sw
    (fun ?use_capture:_ ?passive:_ () -> onorientationchange ())
    ?elapsed_time () t

let limited_onorientationchanges_or_onresizes ~sw ?elapsed_time t =
  limited_loop ~sw
    (fun ?use_capture:_ ?passive:_ () -> onorientationchange_or_onresize ())
    ?elapsed_time () t
