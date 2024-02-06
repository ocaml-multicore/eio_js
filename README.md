# eio_js - Eio for JavaScript environments

The following packages are provided:
- `js_of_ocaml-eio`, an Eio counterpart to package `js_of_ocaml-lwt`;
- `eio_brr`, a package providing Eio variants of Brr functions (which returns directly instead of returning a future);
- `eio_js_backend`, a common Eio JavaScript backend.

You can use `Eio_brr.start` or `Js_of_ocaml_eio.Eio_js.start` to execute a function asynchronously in a context where Eio operations can be performed.

Here is a short example using `Eio_brr`.
```ocaml
let counter = get_element "counter" in
let text = get_element "text" in
let output = get_element "output" in
Eio_brr.start @@ fun () ->
Fiber.both
  (fun () ->
    (* A little text editor *)
    while true do
      let ev = Eio_brr.Ev.next Ev.keyup (El.as_target text) in
      let target = Jv.get (Ev.to_jv ev) "target" in
      let text = Jv.get target "value" |> Jv.to_jstr in
      El.set_children output [ El.txt text ]
    done)
  (fun () ->
    (* A little timer counting up *)
    let i = ref 0 in
    while true do
      El.set_children counter [ El.txt' (string_of_int !i ^ "s") ];
      Eio_brr.G.set_timeout ~ms:1000;
      incr i
    done)
```
