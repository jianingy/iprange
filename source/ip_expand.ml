(*
 * filename   : ip_expand.ml
 * created at : 2013-04-13 18:15:10
 * author     : Jianing Yang <jianingy.yang AT gmail DOT com>
 *)

open Iprange

let out_format = ref CIDR
let netmask = ref "32"
let networks = ref []

let set_out_format format =
  if String.compare format "netmask" = 0 then out_format := NETMASK
  else if String.compare format "cidr" = 0 then out_format := CIDR
  else if String.compare format "wildcard" = 0 then out_format := WILDCARD
  else if String.compare format "range" = 0 then out_format := RANGE
  else failwith "invalid output format"
let spec = [
  ("-type", Arg.String set_out_format, "output ip format. valid values are netmask, cidr, wildcard or range.");
  ("-cidr", Arg.String (fun i -> netmask := i), "specifiy output network size in CIDR notation");
  ("-netmask", Arg.String (fun i -> netmask := i), "specify output network size in NETMASK notation ");
]

let read_network () =
  let rec readlines () =
    try
      let line = input_line stdin in
      line :: readlines ()
    with
	End_of_file -> [] in
  readlines ()

let print_network network =
  print_endline (string_of_network network !out_format)

let usage = "usage: " ^ Sys.argv.(0) ^ " [-t netmask|cidr|wildcard] [-cidr|-netmask] ip1 ip2 ..."
let () =
  Arg.parse spec (fun x -> networks := (x :: (!networks))) usage;
  ignore(List.map
	   (fun x -> expand_network ~netmask:!netmask print_network x)
	   (if List.length !networks > 0 then !networks else (read_network ())))
