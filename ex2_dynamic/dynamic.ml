module Dynamic : sig
	type donnee
	val longueur_plps : string -> string -> int
	val mot_plps : string -> string -> string
end = struct
	type donnee = {n: int ; i: int}

	(* plps : Longueur Plus Long Préfixe des Suffixes *)
	let plps (s1:string) (s2:string) : (int*int) =
		let n = (String.length s1) and m = (String.length s2) and maxi = ref {n= 0; i= 0} in

		let sol = Array.make_matrix n m {n=0; i= 0} in (
			for inv_i = 1 to n do
				let i = (n-inv_i) in
				for inv_j = 1 to m do
					let j = (m-inv_j) in
					if (String.get s1 i) = (String.get s2 j) then
						if i<n-1 && j<m-1 then 
							let n=sol.(i+1).(j+1).n+1 in (
							if (!maxi).n < n then maxi := {n=n; i=i};
							sol.(i).(j) <- {n=n; i=i}
						) else 
							sol.(i).(j) <- {n=1; i=i}
					else ()
				done
			done;
			Printf.printf "Taille espace dynamique : %d\n" (Obj.reachable_words (Obj.repr sol));
			((!maxi).n, (!maxi).i)
		)

	(* mot_plps : Mot le Plus Long Préfixe des Suffixes *)
	let mot_plps (s1:string) (s2:string) : string =
		let (n, i) = plps s1 s2
		in String.sub s1 i n

	let longueur_plps (s1:string) (s2:string) : int = 
		let n,_ = plps s1 s2 in n
end



