open Stack

type element = 
		Asn_Op
	|	Int of int
	|   Flt of float
	|	Id of string
	|	Vec2_Op
	|	Add_Op
	|	On_Op
	|	Off_Op

let stck = Stack.create ()
let a = Stack.push (Int(1),"one","int") stck
let a = Stack.pop stck

let thrd (x,y,z) = z	

let evalTuple (x,y,z) g i = (match x with
		Int(v) ->  Stack.push (x,y,z) (stck)
	| 	Flt(v) -> Stack.push (x,y,z) (stck)
	|	Add_Op	-> let t1 = Stack.pop stck and t2 = Stack.pop stck in
					(
						let v1 = (thrd t1) and v2 = (thrd t2) in
						(
							if (v1 != v2) then
							( 
								raise ( Failure ("Type mismatch: " ^ (v1) ^ " and " ^ (v2)) ) 
							)
							else
							(
								Stack.push (x,y,v1) stck
							)
						)
					)
	|	Id(v)	-> 		if List.exists (fun s -> (fst s) = v) (Array.to_list g)
						then
						(
							let f = List.find (fun s -> (fst s) = v) (Array.to_list g) in
							print_endline ("Existing value: " ^ (fst f));
							Stack.push (x,y,(snd f)) stck
						)
						else
						(
							g.(!i) <- (v, "TypeToInfer");
							print_endline ("Inserting new value: " ^ v);
							i := !i + 1;
							Stack.push (x,y,z) stck
						)				
	|	_ -> ()
	)

let sa lst g i = 	g.(!i) <- ("alex","name"); 
					i := !i + 1;
					Stack.clear stck;
					List.iter (fun (x) -> evalTuple x g i) lst;
					lst 
  
	