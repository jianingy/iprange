(*
 * filename   : iprange.ml
 * created at : 2013-01-18 12:44:47
 * author     : Jianing Yang <jianingy.yang AT gmail DOT com>
 *)

open Int64

type ip_block = { in_addr: int64; prefix: int }
type ip_format = NETMASK | CIDR

let ip_to_int s_addr =
  let dot_split s =
    Str.split (Str.regexp "\\.") s_addr in
  let sum s x =
    add (shift_left s 8) (of_string x) in
  List.fold_left sum zero (dot_split s_addr)

let int_to_ip in_addr =
  let rec div x =
    if x = zero then ""
    else
      let shifted = shift_right_logical x 8 in
      let masked = logand x (of_int 0xff) in
      (div shifted) ^ "." ^ (string_of_int (to_int masked)) in
  Str.string_after (div in_addr) 1

let ip_block_to_ip_range ib =
  let wildcard = shift_right_logical (of_int 0xffffffff) ib.prefix in
  let netmask = lognot wildcard in
  let ip_start = logand ib.in_addr netmask in
  let ip_end = logor ib.in_addr wildcard in
  (ip_start, ip_end)

(* tail-recursive to avoid stack overflow *)
let rec ip_range_to_ip_block ?(acc=[]) ?(block_max=32) (ip_start, ip_end) =

  (* merge max_block_size and fit_block_size together *)
  let rec max_block_size ip =
    if logand (lognot ip) one = one
    then 1 + (max_block_size (shift_right_logical ip 1))
    else 0 in

  let rec fit_block_size size =
    if (compare (add (sub ip_end ip_start) one) (shift_left one size) < 0) || (size > block_max)
    then fit_block_size (size - 1)
    else size in

  if ip_start > ip_end then acc
  else if compare ip_start ip_end = 0 then {in_addr=ip_end; prefix=32} :: acc
  else let suffix = fit_block_size (max_block_size ip_start) in
       let ip_next = add ip_start (shift_left one suffix) in
       ip_range_to_ip_block ~acc:({in_addr=ip_start; prefix=32 - suffix} :: acc) ~block_max:block_max (ip_next, ip_end)

let ip_block_of_string s =
  let split s =
    Str.split (Str.regexp "/") s in
  match split s with
      [] -> {in_addr=zero; prefix=0}
    | ip :: [] -> {in_addr=ip_to_int ip; prefix=32}
    | ip :: prefix :: _ -> {in_addr=ip_to_int ip; prefix=int_of_string prefix}

let rec sort_ip_block ibs =
  let ip_block_compare pivot x = compare pivot.in_addr x.in_addr > 0 in
  match ibs with
      [] -> []
    | hd :: tl -> let lhs, rhs = List.partition (ip_block_compare hd) tl in
		  (sort_ip_block lhs) @ (hd :: (sort_ip_block rhs))

let merge_ip_block ibs =
  let merge_ip_range merged next =
    match merged with
	[] -> [next]
      | hd :: tl -> if compare (fst next) (add one (snd hd)) > 0
	then next :: merged
	else if compare (snd next) (snd hd) > 0 then (fst hd, snd next) :: tl
	else merged in
  let sorted_ibs = sort_ip_block ibs in
  let sorted_range = List.map ip_block_to_ip_range sorted_ibs in
  List.fold_left (fun acc x -> acc @ (ip_range_to_ip_block x)) [] (List.fold_left merge_ip_range [] sorted_range)

let split_ip_block ibs size =
  List.fold_left (fun acc x -> acc @ (ip_range_to_ip_block ~block_max:size x)) [] (List.map ip_block_to_ip_range ibs)

let ip_block_to_string ib format =
  match format with
      CIDR -> (int_to_ip ib.in_addr) ^ "/" ^ (string_of_int ib.prefix)
    | NETMASK -> (int_to_ip ib.in_addr) ^ "/" ^ (int_to_ip (logand 0xffffffffL (shift_left 0xffffffffL (32 - ib.prefix))))

let read_ip_block () =
  let rec readlines () =
    try
      let line = input_line stdin in
      line :: readlines ()
    with
	End_of_file -> [] in
  List.map ip_block_of_string (readlines ())

let ip_compress ibs out_format=
  let print_ip_block x =
    print_endline (ip_block_to_string x out_format) in
  List.iter (fun x -> print_ip_block x) (merge_ip_block ibs)

let ip_expand ibs prefix out_format =
  let print_ip_block x =
    print_endline (ip_block_to_string x out_format) in
  List.iter (fun x -> print_ip_block x) (split_ip_block (merge_ip_block ibs) (32 - prefix))

type avail_action = Compress | Expand
let expand_prefix = ref 32
let out_format = ref CIDR
let do_action = ref Compress
let ip_addr = ref []
let usage = "usage: " ^ Sys.argv.(0) ^ " [-bc] ip1 ip2 ..."

let speclist = [
    ("-n", Arg.Unit   (fun () -> out_format := NETMASK), ": output in netmask format");
    ("-c", Arg.Unit   (fun () -> do_action := Compress), ": compress ip addresses");
    ("-x", Arg.Unit   (fun () -> do_action := Expand), ": expand ip addresses");
    ("-s", Arg.Int    (fun s -> expand_prefix := s), ": size of expanded block");
]

let () =
  (* Read the arguments *)
  Arg.parse
    speclist
    (fun x -> ip_addr := (x :: (!ip_addr)))
    usage;
  let ibs =
    if List.length !ip_addr > 0 then
      List.map ip_block_of_string !ip_addr
    else
      read_ip_block () in
  match !do_action with
      Compress -> ip_compress ibs !out_format
    | Expand -> ip_expand ibs !expand_prefix !out_format;
  ()
