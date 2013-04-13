(*
 * filename   : ip4_network.ml
 * created at : 2013-04-11 20:34:45
 * author     : Jianing Yang <jianingy.yang AT gmail DOT com>
 *)

(*
if a netmask is valid:
netmask XOR 0xffffffff
*)

#use "topfind" ;;
#require "str" ;;
#require "pcre" ;;
open Pcre
open Int64
type network = { prefix: int64; wildcard: int64 }
type network_range = { start: int64; finish: int64 }


class ip4_network =
object(self)
  method range_of_network network =
    let canonical_prefix = logand network.prefix (lognot network.wildcard) in
    if compare canonical_prefix network.prefix != 0 then
      print_endline "warning: prefix not start at a network boundary. using previous boundary.";
    {
      start=canonical_prefix;
      finish=add canonical_prefix network.wildcard
    }
  method wildcard_of_netmask netmask =
    let netmask_int = self#ip_of_string netmask in
    (* check for valid wilcard *)
    logxor netmask_int (of_int 0xffffffff)
  method wildcard_of_cidr cidr =
    (shift_right_logical (of_int 0xffffffff) (int_of_string cidr))
  method ip_of_string ip_string =
    let rex = regexp "^(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$" in
    match pmatch ~rex ip_string with
      | true -> let _split s = split ~rex:(regexp "\\.") s in
		let _sum sum dec =
		  add (shift_left sum 8) (of_string dec) in
		List.fold_left _sum zero (_split ip_string)
      | false -> failwith "invalid ip address"
  method network_of_string network_string =
    match split ~rex:(regexp "/") network_string with
      | ip :: mask :: _ -> (match pmatch ~rex:(regexp "^\\d+$") mask with
	  | true -> self#range_of_network
	    {
	      prefix=self#ip_of_string ip;
	      wildcard=self#wildcard_of_cidr mask
	    }
	  | false -> self#range_of_network
	    {
	      prefix=self#ip_of_string ip;
	      wildcard=self#wildcard_of_netmask mask
	    })
      | ip :: _ -> self#range_of_network
	{
	prefix=self#ip_of_string ip;
	wildcard=one
	}
      | _ -> failwith "empty ip address"

end;;

let n = new ip4_network in
n#network_of_string "1.2.3.4/255.255.255.252" ;;
