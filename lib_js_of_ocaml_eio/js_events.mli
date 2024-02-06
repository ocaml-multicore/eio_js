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

(** Programming mouse or keyboard events handlers using Lwt *)

open Js_of_ocaml

(**
   Reminder:
   Event capturing starts with the outer most element in the DOM and
   works inwards to the HTML element the event took place on (capture phase)
   and then out again (bubbling phase).

   Examples of use:

   Waiting for a click on [elt1] before continuing:

   {[Eio_js_events.click elt1]}

   Defining a thread that waits for ESC key on an element:

   {[let rec esc elt =
      let ev = keydown elt in
      if ev##.keyCode = 27
      then Lwt.return ev
      else esc elt]}

  {2 Create Eio fibers for events} *)

val make_event :
  (#Dom_html.event as 'a) Js.t Dom_html.Event.typ ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  'a Js.t
(** [make_event ev target] creates an Eio fiber that waits
    for the event [ev] to happen on [target] (once).
    This thread isa cancellable.
    If you set the optional parameter [~use_capture:true],
    the event will be caught during the capture phase,
    otherwise it is caught during the bubbling phase
    (default).
    If you set the optional parameter [~passive:true],
    the user agent will ignore [preventDefault] calls
    inside the event callback.
*)

type cancel = { cancel : unit -> unit }

val seq_loop :
  sw:Eio.Switch.t ->
  (?use_capture:bool -> ?passive:bool -> 'target -> 'event) ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  'target ->
  ('event -> cancel -> unit) ->
  cancel
(** [seq_loop (make_event ev) target handler] creates a looping Eio
    fiber that waits for the event [ev] to happen on [target], then
    execute [handler], and start again waiting for the event. Events
    happening during the execution of the handler are ignored. See
    [async_loop] and [buffered_loop] for alternative semantics.

    For example, the [clicks] function below is defined by:

    [let clicks ?use_capture ?passive t = seq_loop click ?use_capture ?passive t]

    The loop can be cancelled by calling the returned [cancel] function.
    In order for the loop to be canceled from within the handler,
    the handler also receives the function as its second parameter.

    By default, cancelling the loop will not cancel the potential
    currently running handler. This behaviour can be changed by
    setting the [cancel_handler] parameter to true.
*)

val async_loop :
  sw:Eio.Switch.t ->
  (?use_capture:bool -> ?passive:bool -> 'target -> 'event) ->
  ?use_capture:bool ->
  ?passive:bool ->
  'target ->
  ('event -> cancel -> unit) ->
  cancel
(** [async_loop] is similar to [seq_loop], but each handler runs
    independently. No event is thus missed, but since several
    instances of the handler can be run concurrently, it is up to the
    programmer to ensure that they interact correctly.

    Cancelling the loop will not cancel the potential currently running
    handlers.
*)

(*
val buffered_loop :
     (?use_capture:bool -> ?passive:bool -> 'target -> 'event)
  -> ?cancel_handler:bool
  -> ?cancel_queue:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> 'target
  -> ('event -> unit Lwt.t -> unit Lwt.t)
  -> unit
(** [buffered_loop] is similar to [seq_loop], but any event that
    occurs during an execution of the handler is queued instead of
    being ignored.

    No event is thus missed, but there can be a non predictable delay
    between its trigger and its treatment. It is thus a good idea to
    use this loop with handlers whose running time is short, so the
    memorized event still makes sense when the handler is eventually
    executed. It is also up to the programmer to ensure that event
    handlers terminate so the queue will eventually be emptied.

    By default, cancelling the loop will not cancel the (potential)
    currently running handler, but any other queued event will be
    dropped. This behaviour can be customized using the two optional
    parameters [cancel_handler] and [cancel_queue].
*)
*)
val async : sw:Eio.Switch.t -> (unit -> unit) -> unit
(** [async t] records a thread to be executed later.
    It is implemented by forking a fiber that immediately calls [Eio_js.yield].
    This is useful if you want to create a new event listener
    when you are inside an event handler.
    This avoids the current event to be caught by the new event handler
    (if it propagates).
*)

val func_limited_loop :
  sw:Eio.Switch.t ->
  (?use_capture:bool -> ?passive:bool -> 'a -> 'b) ->
  (unit -> unit) ->
  ?use_capture:bool ->
  ?passive:bool ->
  'a ->
  ('b -> cancel -> unit) ->
  cancel
(** [func_limited_loop event delay_fun target handler] will behave like
    [Lwt_js_events.async_loop event target handler] but it will run [delay_fun]
    first, and execute [handler] only when [delay_fun] is finished and
    no other event occurred in the meantime.

    This allows to limit the number of events caught.

    Be careful, it is an asynchrone loop, so if you give too little time,
    several instances of your handler could be run in same time **)

val limited_loop :
  sw:Eio.Switch.t ->
  (?use_capture:bool -> ?passive:bool -> 'a -> 'b) ->
  ?elapsed_time:float ->
  ?use_capture:bool ->
  ?passive:bool ->
  'a ->
  ('b -> cancel -> unit) ->
  cancel
(** Same as func_limited_loop but take time instead of function
    By default elapsed_time = 0.1s = 100ms **)

(**  {2 Predefined functions for some types of events} *)

val click :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.mouseEvent Js.t

val copy :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.clipboardEvent Js.t

val cut :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.clipboardEvent Js.t

val paste :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.clipboardEvent Js.t

val dblclick :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.mouseEvent Js.t

val mousedown :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.mouseEvent Js.t

val mouseup :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.mouseEvent Js.t

val mouseover :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.mouseEvent Js.t

val mousemove :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.mouseEvent Js.t

val mouseout :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.mouseEvent Js.t

val keypress :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.keyboardEvent Js.t

val keydown :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.keyboardEvent Js.t

val keyup :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.keyboardEvent Js.t

val input :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.event Js.t

val timeupdate :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.event Js.t

val change :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.event Js.t

val dragstart :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.dragEvent Js.t

val dragend :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.dragEvent Js.t

val dragenter :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.dragEvent Js.t

val dragover :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.dragEvent Js.t

val dragleave :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.dragEvent Js.t

val drag :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.dragEvent Js.t

val drop :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.dragEvent Js.t

val focus :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.focusEvent Js.t

val blur :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.focusEvent Js.t

val scroll :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.event Js.t

val submit :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.submitEvent Js.t

val select :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.event Js.t

val mousewheel :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.mouseEvent Js.t * (int * int)
(** This function returns the event,
    together with the numbers of ticks the mouse wheel moved.
    Positive means down or right.
    This interface is compatible with all (recent) browsers. *)

val wheel :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.mousewheelEvent Js.t

val touchstart :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.touchEvent Js.t

val touchmove :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.touchEvent Js.t

val touchend :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.touchEvent Js.t

val touchcancel :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.touchEvent Js.t

val lostpointercapture :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.pointerEvent Js.t

val gotpointercapture :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.pointerEvent Js.t

val pointerenter :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.pointerEvent Js.t

val pointercancel :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.pointerEvent Js.t

val pointerdown :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.pointerEvent Js.t

val pointerleave :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.pointerEvent Js.t

val pointermove :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.pointerEvent Js.t

val pointerout :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.pointerEvent Js.t

val pointerover :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.pointerEvent Js.t

val pointerup :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.pointerEvent Js.t

val transitionend :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.transitionEvent Js.t
(** Returns when a CSS transition terminates on the element. *)

val transitionstart :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.transitionEvent Js.t

val transitionrun :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.transitionEvent Js.t

val transitioncancel :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.transitionEvent Js.t

val load :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.imageElement Js.t ->
  Dom_html.event Js.t

val error :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.imageElement Js.t ->
  Dom_html.event Js.t

val abort :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.imageElement Js.t ->
  Dom_html.event Js.t

val canplay :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.event Js.t

val canplaythrough :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.event Js.t

val durationchange :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.event Js.t

val emptied :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.event Js.t

val ended :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.event Js.t

val loadeddata :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.event Js.t

val loadedmetadata :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.event Js.t

val loadstart :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.event Js.t

val pause :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.event Js.t

val play :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.event Js.t

val playing :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.event Js.t

val ratechange :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.event Js.t

val seeked :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.event Js.t

val seeking :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.event Js.t

val stalled :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.event Js.t

val suspend :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.event Js.t

val volumechange :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.event Js.t

val waiting :
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  Dom_html.event Js.t

val clicks :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.mouseEvent Js.t -> cancel -> unit) ->
  cancel

val copies :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.clipboardEvent Js.t -> cancel -> unit) ->
  cancel

val cuts :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.clipboardEvent Js.t -> cancel -> unit) ->
  cancel

val pastes :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.clipboardEvent Js.t -> cancel -> unit) ->
  cancel

val dblclicks :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.mouseEvent Js.t -> cancel -> unit) ->
  cancel

val mousedowns :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.mouseEvent Js.t -> cancel -> unit) ->
  cancel

val mouseups :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.mouseEvent Js.t -> cancel -> unit) ->
  cancel

val mouseovers :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.mouseEvent Js.t -> cancel -> unit) ->
  cancel

val mousemoves :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.mouseEvent Js.t -> cancel -> unit) ->
  cancel

val mouseouts :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.mouseEvent Js.t -> cancel -> unit) ->
  cancel

val keypresses :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.keyboardEvent Js.t -> cancel -> unit) ->
  cancel

val keydowns :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.keyboardEvent Js.t -> cancel -> unit) ->
  cancel

val keyups :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.keyboardEvent Js.t -> cancel -> unit) ->
  cancel

val inputs :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.event Js.t -> cancel -> unit) ->
  cancel

val timeupdates :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.event Js.t -> cancel -> unit) ->
  cancel

val changes :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.event Js.t -> cancel -> unit) ->
  cancel

val dragstarts :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.dragEvent Js.t -> cancel -> unit) ->
  cancel

val dragends :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.dragEvent Js.t -> cancel -> unit) ->
  cancel

val dragenters :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.dragEvent Js.t -> cancel -> unit) ->
  cancel

val dragovers :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.dragEvent Js.t -> cancel -> unit) ->
  cancel

val dragleaves :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.dragEvent Js.t -> cancel -> unit) ->
  cancel

val drags :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.dragEvent Js.t -> cancel -> unit) ->
  cancel

val drops :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.dragEvent Js.t -> cancel -> unit) ->
  cancel

val mousewheels :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.mouseEvent Js.t * (int * int) -> cancel -> unit) ->
  cancel

val wheels :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.mousewheelEvent Js.t -> cancel -> unit) ->
  cancel

val touchstarts :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.touchEvent Js.t -> cancel -> unit) ->
  cancel

val touchmoves :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.touchEvent Js.t -> cancel -> unit) ->
  cancel

val touchends :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.touchEvent Js.t -> cancel -> unit) ->
  cancel

val touchcancels :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.touchEvent Js.t -> cancel -> unit) ->
  cancel

val focuses :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.focusEvent Js.t -> cancel -> unit) ->
  cancel

val blurs :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.focusEvent Js.t -> cancel -> unit) ->
  cancel

val scrolls :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.event Js.t -> cancel -> unit) ->
  cancel

val submits :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.submitEvent Js.t -> cancel -> unit) ->
  cancel

val selects :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.event Js.t -> cancel -> unit) ->
  cancel

val loads :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.imageElement Js.t ->
  (Dom_html.event Js.t -> cancel -> unit) ->
  cancel

val errors :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.imageElement Js.t ->
  (Dom_html.event Js.t -> cancel -> unit) ->
  cancel

val aborts :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.imageElement Js.t ->
  (Dom_html.event Js.t -> cancel -> unit) ->
  cancel

val canplays :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.event Js.t -> cancel -> unit) ->
  cancel

val canplaythroughs :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.event Js.t -> cancel -> unit) ->
  cancel

val durationchanges :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.event Js.t -> cancel -> unit) ->
  cancel

val emptieds :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.event Js.t -> cancel -> unit) ->
  cancel

val endeds :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.event Js.t -> cancel -> unit) ->
  cancel

val loadeddatas :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.event Js.t -> cancel -> unit) ->
  cancel

val loadedmetadatas :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.event Js.t -> cancel -> unit) ->
  cancel

val loadstarts :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.event Js.t -> cancel -> unit) ->
  cancel

val pauses :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.event Js.t -> cancel -> unit) ->
  cancel

val plays :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.event Js.t -> cancel -> unit) ->
  cancel

val playings :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.event Js.t -> cancel -> unit) ->
  cancel

val ratechanges :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.event Js.t -> cancel -> unit) ->
  cancel

val seekeds :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.event Js.t -> cancel -> unit) ->
  cancel

val seekings :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.event Js.t -> cancel -> unit) ->
  cancel

val stalleds :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.event Js.t -> cancel -> unit) ->
  cancel

val suspends :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.event Js.t -> cancel -> unit) ->
  cancel

val volumechanges :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.event Js.t -> cancel -> unit) ->
  cancel

val waitings :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.event Js.t -> cancel -> unit) ->
  cancel

val lostpointercaptures :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.pointerEvent Js.t -> cancel -> unit) ->
  cancel

val gotpointercaptures :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.pointerEvent Js.t -> cancel -> unit) ->
  cancel

val pointerenters :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.pointerEvent Js.t -> cancel -> unit) ->
  cancel

val pointercancels :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.pointerEvent Js.t -> cancel -> unit) ->
  cancel

val pointerdowns :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.pointerEvent Js.t -> cancel -> unit) ->
  cancel

val pointerleaves :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.pointerEvent Js.t -> cancel -> unit) ->
  cancel

val pointermoves :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.pointerEvent Js.t -> cancel -> unit) ->
  cancel

val pointerouts :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.pointerEvent Js.t -> cancel -> unit) ->
  cancel

val pointerovers :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.pointerEvent Js.t -> cancel -> unit) ->
  cancel

val pointerups :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.pointerEvent Js.t -> cancel -> unit) ->
  cancel

val transitionends :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.transitionEvent Js.t -> cancel -> unit) ->
  cancel

val transitionstarts :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.transitionEvent Js.t -> cancel -> unit) ->
  cancel

val transitionruns :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.transitionEvent Js.t -> cancel -> unit) ->
  cancel

val transitioncancels :
  sw:Eio.Switch.t ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.transitionEvent Js.t -> cancel -> unit) ->
  cancel

val request_animation_frame : unit -> unit
(** Returns when a repaint of the window by the browser starts.
    (see JS method [window.requestAnimationFrame]) *)

val onload : unit -> Dom_html.event Js.t
(** Returns when the page is loaded *)

val domContentLoaded : unit -> unit
val onunload : unit -> Dom_html.event Js.t
val onbeforeunload : unit -> Dom_html.event Js.t
val onresize : unit -> Dom_html.event Js.t
val onorientationchange : unit -> Dom_html.event Js.t
val onpopstate : unit -> Dom_html.popStateEvent Js.t
val onhashchange : unit -> Dom_html.hashChangeEvent Js.t
val onorientationchange_or_onresize : unit -> Dom_html.event Js.t

val onresizes :
  sw:Eio.Switch.t -> (Dom_html.event Js.t -> cancel -> unit) -> cancel

val onorientationchanges :
  sw:Eio.Switch.t -> (Dom_html.event Js.t -> cancel -> unit) -> cancel

val onpopstates :
  sw:Eio.Switch.t -> (Dom_html.popStateEvent Js.t -> cancel -> unit) -> cancel

val onhashchanges :
  sw:Eio.Switch.t -> (Dom_html.hashChangeEvent Js.t -> cancel -> unit) -> cancel

val onorientationchanges_or_onresizes :
  sw:Eio.Switch.t -> (Dom_html.event Js.t -> cancel -> unit) -> cancel

val limited_onresizes :
  sw:Eio.Switch.t ->
  ?elapsed_time:float ->
  (Dom_html.event Js.t -> cancel -> unit) ->
  cancel

val limited_onorientationchanges :
  sw:Eio.Switch.t ->
  ?elapsed_time:float ->
  (Dom_html.event Js.t -> cancel -> unit) ->
  cancel

val limited_onorientationchanges_or_onresizes :
  sw:Eio.Switch.t ->
  ?elapsed_time:float ->
  (Dom_html.event Js.t -> cancel -> unit) ->
  cancel
