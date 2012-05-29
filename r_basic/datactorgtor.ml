(*    de base je prends une fonctions qui effectue une operatiosn sur x *)
(*		*)
(*		*)
(*    data : constituer d'une suite de couple de valeur (x , f (x)) *)
(*    f est l'inconnu *)
(*    f (x) est un bool representant un element de la donnee a la position x *)
(*    x equivaut a l'abscisse , incrementer suivant un pas ( par defaut ) *)
(*		*)
(*		*)
(*    trouver l'equations  *)

(* number list <=> coef *)
(* matrice              *)
(* *)

let root = 12::23::43::45::34::[]  (* 5 valeurs racines <=> f est un polynome de degre 4 *)
(* P (x) = A * x^n + A * x ^ n-1 + ... + A1 * X + A0
   root : les valeurs pour lesquel Px s'annulle
   P (0) = 12 = f(x)= ax3 + bx2 + cx +d = 0 + 0 + 0 + d <=> d = 12
   P (1) = 23 = f(x)= ax3 + bx2 + cx +d = a + b + c + 12 <=> c = a + b -12
   P (2) = 43 = f(x)= ax3 + bx2 + cx +d = a8 + b4 + 2(a + b - 12) + 12 = 4a + 2b + a + b = 0 <=> 5a + 3b = 0 <=> a  = -3b / 5
   P (3) = 45 = f(x)= ax3 + bx2 + cx +d = 9a + 
   P (4) = 34 = f(x)= ax3 + bx2 + cx +d =

      P(X) est connu
} *)



