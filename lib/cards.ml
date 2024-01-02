type valeur = 
  | Deux  
  | Trois
  | Quatre
  | Cinq
  | Six 
  | Sept
  | Huit
  | Neuf
  | Dix
  | Valet
  | Dame
  | Roi
  | As

type couleur =
  | Carreau
  | Coeur
  | Pique 
  | Trefle

type carte = {
  value: valeur;
  col: couleur;
}

let valeur_to_str = function
  | Deux -> "2"
  | Trois -> "3"
  | Quatre -> "4"
  | Cinq -> "5"
  | Six  -> "6"
  | Sept -> "7"
  | Huit -> "8"
  | Neuf -> "9"
  | Dix -> "10"
  | Valet -> "J"
  | Dame -> "Q"
  | Roi -> "K"
  | As -> "A"

let couleur_to_str = function 
  | Carreau -> "♦️"
  | Coeur -> "♥️"
  | Pique -> "♠️"
  | Trefle -> "♣️"

let print_carte crt = 
  print_string (Printf.sprintf " [%s%s] " (couleur_to_str crt.col) (valeur_to_str crt.value))

let rec print_deck = function 
  | hd::tl -> print_carte hd; print_deck tl
  | [] -> print_string "\n"


let cartesian_product list1 list2 = 
  let rec aux res l1 l2 = match l1, l2 with 
    | h1::t1, h2::t2 -> aux ((h1, h2)::res) (h1::t1) t2 
    | _::t1, [] -> aux res t1 list2
    | [], _ -> res
  in aux [] list1 list2

let generate_random_deck () = 
  let valeurs = [Deux; Trois; Quatre; Cinq; Six; Sept; Huit; Neuf; Dix; Valet; Dame; Roi; As] in 
  let couleurs = [Carreau; Coeur; Pique; Trefle] in 
  cartesian_product valeurs couleurs 
    |> List.map (fun (v, c) -> {value=v;col=c}, Random.int 100)
    |> List.sort (fun (_, r1) (_, r2) -> compare r1 r2)
    |> List.map (fun (crt, _) -> crt)






