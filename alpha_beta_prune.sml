datatype 'a tree = Br of 'a tree list | Lf of 'a

fun empty [] = true
 |	empty _  = false

fun max a b = if a > b then a else b

fun min a b = if a < b then a else b

fun head (x::xs) = x

fun tl (x::xs) = xs
 |	tl [] = []

fun player _ _ (Lf(v)) opp = v
 |  player a b (Br(s)) opp = 
		let
			val curValue = ref ~99999999
			val suc = ref s 
			val na = ref a
			val thissuc = ref (Lf(0))
		in
			while empty (!suc) = false do
			(
				thissuc := head (!suc);
				curValue := (max (!curValue) (opp (!na) b (!thissuc)));
				if (!curValue) > b then suc := []
				else if (!curValue) > (!na) then na := !curValue else ();
				suc := tl (!suc)
			); !curValue
		end

fun opponent _ _ (Lf(v)) = v
 |  opponent a b (Br(s)) = 
		let
			val curValue = ref 9999999
			val suc = ref s 
			val na = ref a
			val thissuc = ref (Lf(0))
		in
			while empty (!suc) = false do
			(
				thissuc := head (!suc);
				curValue := (min (!curValue) (player (!na) b (!thissuc) opponent));
				if (!curValue) > b then suc := []
				else if (!curValue) > (!na) then na := !curValue else ();
				suc := tl (!suc)
			); !curValue
		end

val max20 = Lf(1)
val max21 = Lf(~15)
val max22 = Lf(2)
val max23 = Lf(19)
val max24 = Lf(18)
val max25 = Lf(23)
val max26 = Lf(4)
val max27 = Lf(3)
val max28 = Lf(2)
val max29 = Lf(1)
val max210 = Lf(7)
val max211 = Lf(8)
val max212 = Lf(9)
val max213 = Lf(10)
val max214 = Lf(~2)
val max215 = Lf(5)
val max216 = Lf(~1)
val max217 = Lf(~30)
val max218 = Lf(4)
val max219 = Lf(7)
val max220 = Lf(20)
val max221 = Lf(~1)
val max222 = Lf(~1)
val max223 = Lf(~5)

val min20 = Br([max20,max21])
val min21 = Br([max22,max23])
val min22 = Br([max24,max25])
val min23 = Br([max26,max27])
val min24 = Br([max28,max29])
val min25 = Br([max210,max211])
val min26 = Br([max212,max213])
val min27 = Br([max214,max215])
val min28 = Br([max216,max217])
val min29 = Br([max218,max219])
val min210 = Br([max220,max221])
val min211 = Br([max222,max223])

val max10 = Br([min20,min21])
val max11 = Br([min22,min23])
val max12 = Br([min24,min25])
val max13 = Br([min26,min27])
val max14 = Br([min28,min29])
val max15 = Br([min210,min211])

val min10 = Br([max10,max11])
val min11 = Br([max12,max13])
val min12 = Br([max14,max15])

val max00 = Br([min10,min11,min12])

val r = player (~999999) (999999) (max00) (opponent)
