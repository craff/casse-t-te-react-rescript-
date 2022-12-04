
@val external preferred : string = "navigator.language"
@val external all : array<string> = "navigator.languages"

exception Found(string)
let choose : (string => option<string>) => string = fn =>
  try {
    all->Belt.Array.forEach(lang =>
      switch fn(lang) {
      | None => ()
      | Some(t) => raise(Found(t))
      })
    switch fn("en") {
      | None => assert(false)
      | Some(t) => raise(Found(t))
    }
  } catch { | Found(r) => r }

/*
let  = choose(lang => switch lang {
  | "en" => Some("")
  | "fr" => Some("")
  | _    => None})
*/

let easy = choose(lang => switch lang {
  | "en" => Some("Easy")
  | "fr" => Some("Facile")
  | _    => None})

let medium = choose(lang => switch lang {
  | "en" => Some("Medium")
  | "fr" => Some("Moyen")
  | _    => None})

let hard = choose(lang => switch lang {
  | "en" => Some("Hard")
  | "fr" => Some("Difficile")
  | _    => None})

let undefined = choose(lang => switch lang {
  | "en" => Some("Your solution is undefined")
  | "fr" => Some("Votre solution n'est pas définie")
  | _    => None})

let link = choose(lang => switch lang {
  | "en" => Some("Link to share if you enjoyed this problem: ")
  | "fr" => Some("Lien à partager si vous vous êtes bien amusé avec ce problem : ")
  | _    => None})

let title = choose(lang => switch lang {
  | "en" => Some("A Corean puzzle")
  | "fr" => Some("Un casse tête Corréen")
  | _    => None})

let description  = choose(lang => switch lang {
  | "en" => Some("The rule is simple, you have to fill all the empty cases with integer between 1 and N to ensure that the equality holds. A different number must be used in each cell. Good luck!")
  | "fr" => Some("La règle est simple, il suffit de compléter les cases vides avec les chiffres de 1 à N pour que l'égalité soit juste. Un chiffre différent dans chaque case. Bon courage!")
  | _    => None})

let cancel = choose(lang => switch lang {
  | "en" => Some("Cancel")
  | "fr" => Some("Interrompre")
  | _    => None})

let solutions_found = choose(lang => switch lang {
  | "en" => Some("solutions found in")
  | "fr" => Some("solutions trouvées en")
  | _    => None})

let not_integer = choose(lang => switch lang {
  | "en" => Some("you must provide only integers")
  | "fr" => Some("il faut proposer des entiers")
  | _    => None})

let bad_interval = choose(lang => switch lang {
  | "en" => Some("all integers must between 1 and ")
  | "fr" => Some("tous les entiers doivent être entre 1 et ")
  | _    => None})

let not_all = choose(lang => switch lang {
  | "en" => Some("all integers from 1 must be used exactly once")
  | "fr" => Some("tous les entiers à partir de 1 doivent être utilisé exactement une fois")
  | _    => None})

let not_good = choose(lang => switch lang {
  | "en" => Some("your solution does not give the correct result")
  | "fr" => Some("votre solution ne donne pas le bon résult")
  | _    => None})

let bad_solution = choose(lang => switch lang {
  | "en" => Some("Bad solution")
  | "fr" => Some("Mauvaise solution ")
  | _    => None})

let good_solution = choose(lang => switch lang {
  | "en" => Some("Good solution")
  | "fr" => Some("Bonne solution")
  | _    => None})

let check_solution = choose(lang => switch lang {
  | "en" => Some("Check")
  | "fr" => Some("Vérifie")
  | _    => None})

let generate1 = choose(lang => switch lang {
  | "en" => Some("Generate a problem with a level")
  | "fr" => Some("Genère un problem avec un niveau")
  | _    => None})

let generate2 = choose(lang => switch lang {
  | "en" => Some("and a number of solutions \u2264 ")
  | "fr" => Some("et un nombre de solutions \u2264 ")
  | _    => None})

let solve = choose(lang => switch lang {
  | "en" => Some("Solve")
  | "fr" => Some("Résoud")
  | _    => None})

let nb_tested = choose(lang => switch lang {
  | "en" => Some("problems tested in")
  | "fr" => Some("problèmes testés en")
  | _    => None})
