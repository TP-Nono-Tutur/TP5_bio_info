let main =
  let commande = Sys.argv.(1) in
  match commande with
  | "test" ->
     let string = Sys.argv.(2)
     in print_endline string
  | _ -> prerr_endline "commande inconue"
	       
let rec concat l1 l2 =
	match (l1,l2) with
		([],l2) -> []
		|(l1,[]) -> []
		|(x::r1,y::r2) -> (x^y) ::concat r1 l2


let bwt chaine = 
	let liste = List.map (Char.escaped) (String_mp.to_list chaine)
	and result = liste
	and n = List.length liste in
	for i = 1 to n do
		result = List.sort compare result;
		result = concat liste result
	done
