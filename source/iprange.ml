(*
 * filename   : ip4_network.ml
 * created at : 2013-04-11 20:34:45
 * author     : Jianing Yang <jianingy.yang AT gmail DOT com>
 *)

open Pcre
open Int64
open Printf

type network = { prefix: int64; wildcard: int64 }
type network_range = { start: int64; finish: int64 }
type output_format = NETMASK | WILDCARD | CIDR

let warn s = eprintf "warning: %s\n" s
let ip_re = regexp "^(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$"
let check_wildcard wildcard =
  let rec _shift x =
    if logand x one = zero then
      logand x (of_int 0xffffffff)
    else
      _shift (shift_right x 1) in
  if compare (_shift wildcard) zero != 0
  then failwith "invalid netmask"
  else wildcard

let range_of_network network =
  let canonical_prefix = logand network.prefix (lognot network.wildcard) in
  if compare canonical_prefix network.prefix != 0 then
    warn "prefix does not start at a network boundary. using previous boundary.";
  {
    start=canonical_prefix;
    finish=add canonical_prefix network.wildcard
  }

let ip_of_string ip_string =
  match pmatch ~rex:ip_re ip_string with
    | true -> let _split s = split ~rex:(regexp "\\.") s in
	      let _sum sum dec =
		add (shift_left sum 8) (of_string dec) in
	      List.fold_left _sum zero (_split ip_string)
    | false -> failwith ("invalid ip address: " ^ ip_string)

let wildcard_of_netmask netmask =
  let netmask_int = ip_of_string netmask in
    (* check for valid wilcard *)
  let wildcard = logxor netmask_int (of_int 0xffffffff) in
  check_wildcard wildcard

let wildcard_of_cidr cidr =
  shift_right_logical (of_int 0xffffffff) (int_of_string cidr)

let network_of_string network_string =
  match split ~rex:(regexp "/") network_string with
    | ip :: mask :: _ -> if pmatch ~rex:(regexp "^\\d+$") mask
      then range_of_network
	{
	  prefix=ip_of_string ip;
	  wildcard=wildcard_of_cidr mask
	}
      else range_of_network
	{
	  prefix=ip_of_string ip;
	  wildcard=wildcard_of_netmask mask
	}
    | ip :: _ -> (match split ~rex:(regexp "-") network_string with
	| ip_start :: ip_finish :: _ ->
	  let start = ip_of_string ip_start
	  and finish = ip_of_string ip_finish in
	  if compare start finish < 0  then {start=start; finish=finish}
	  else {start=finish;finish=start}
	| ip :: _ -> range_of_network
	  {
	    prefix=ip_of_string ip;
	    wildcard=zero
	  }
	| _ -> failwith "empty ip address")
    | _ -> failwith "empty ip address"

let merge_network networks =
  let rec _sort_network networks =
    let _compare_network x y = compare x.start y.start > 0 in
    match networks with
      | [] -> []
      | hd :: tl -> let lhs, rhs = List.partition (_compare_network hd) tl in
		    (_sort_network lhs) @ (hd :: _sort_network rhs) in
  let _merge_network acc x =
      (* output in reverse order *)
    match acc with
      | [] -> [x]
      | hd :: tl -> let bound = succ hd.finish in
		    if compare bound x.start < 0 then x :: acc
		    else if compare bound x.start >= 0 && compare hd.finish x.finish < 0 then
		      {start=hd.start; finish=x.finish} :: tl
		    else acc in
  List.fold_left _merge_network [] (_sort_network networks)

let network_prefix_size network =
  let network_size = succ (sub network.finish network.start) in
  let rec _find_max_capacity network size =
    if compare network_size size < 0 then shift_right size 1
    else if compare network_size size = 0 then size
    else if compare (logand network.start size) zero > 0 then size
    else _find_max_capacity network (shift_left size 1) in
  _find_max_capacity network one

let normalize_network network =
  let rec _divide_network acc network =
    let size = network_prefix_size network in
    let next_start = add network.start size in
    let this_finish = pred next_start in
      (*      print_endline ("size=" ^ (to_string size) ^ "; next=" ^ (to_string next_start) ^ "; start=" ^ (to_string network.start) ^ "; finish=" ^ (to_string network.finish)); *)
    if compare this_finish network.finish = 0 then
      match acc with
	| [] -> [network]
	| _ -> network :: acc
    else let divided = {start=network.start; finish=this_finish} in
	 _divide_network (divided :: acc) {start=next_start; finish=network.finish} in
  _divide_network [] network

let string_of_ip ip =
  to_string (logand (shift_right ip 24) (of_int 0xff)) ^ "."
  ^ to_string (logand (shift_right ip 16) (of_int 0xff)) ^ "."
  ^ to_string (logand (shift_right ip 8) (of_int 0xff)) ^ "."
  ^ to_string (logand ip (of_int 0xff))

let cidr_of_wildcard wildcard =
  let rec _cidr x cidr =
    if compare (logand x one) one = 0 then _cidr (shift_right x 1) (cidr + 1)
    else cidr in
  32 - (_cidr wildcard 0)

let string_of_network network output_format =
  let prefix = string_of_ip network.start in
  let wildcard = sub network.finish network.start in
  match output_format with
    | CIDR -> prefix ^ "/" ^ (string_of_int (cidr_of_wildcard wildcard))
    | WILDCARD -> prefix ^ "/" ^ (string_of_ip wildcard)
    | NETMASK -> prefix ^ "/" ^ (string_of_ip (logxor wildcard (of_int 0xffffffff)))

let compress_network ?(output_type=NETMASK) network_strings =
  let networks = List.map network_of_string network_strings in
  let _normalize acc x = (normalize_network x) @ acc in
  let result = List.fold_left _normalize [] (merge_network networks) in
  List.map (fun x -> string_of_network x output_type) result

let expand_network ?(netmask="32") (callback:network_range -> unit) network_string =
  let network = network_of_string network_string in
  let wildcard = if pmatch ~rex:ip_re netmask then wildcard_of_netmask netmask
    else if pmatch ~rex:(regexp "\\d+") netmask then wildcard_of_cidr netmask
    else failwith ("invalid mask string: " ^ netmask) in
  if compare (sub network.finish network.start) wildcard < 0 then
    (warn ("network is too small to be expanded: " ^ network_string);
     callback network)
  else
    let rec _expand_network remain =
      if compare remain.finish remain.start >= 0 then
	let current = {start=remain.start; finish=add remain.start wildcard} in
	let next = {start=add current.finish one; finish=remain.finish} in
	callback current;
	_expand_network next in
    ignore(_expand_network network)
