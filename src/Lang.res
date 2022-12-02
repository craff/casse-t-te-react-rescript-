
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

let title = choose(lang => switch lang {
  | "en" => Some("A Corean puzzle")
  | "fr" => Some("Un casse tête Corréen")
  | _    => None})

let description  = choose(lang => switch lang {
  | "en" => Some("The rule is simple, tou have to fill all the empty cases with integer between 1 and 9 to ensure that the equality holds. Each numver between 1 and 9 must be used exactly once. Good luck!")
  | "fr" => Some("La règle est simple, il suffit de compléter les cases vides avec les chiffres de 1 à 9 pour que l'égalité soit juste. Un chiffre différent dans chaque case. Bon courage!")
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
  | "en" => Some("all integers must between 1 and 9")
  | "fr" => Some("tous les entiers doivent être entre 1 et 9")
  | _    => None})

let not_all = choose(lang => switch lang {
  | "en" => Some("all integers between 1 and 9 must be used once")
  | "fr" => Some("tous les entiers entre 1 et 9 doivent être utilisé une fois")
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
  | "en" => Some("Check my solution")
  | "fr" => Some("Teste ma solution")
  | _    => None})

let generate = choose(lang => switch lang {
  | "en" => Some("Generate a pb whose number of solutions is \u2264 ")
  | "fr" => Some("Genère un problème avec un nombre de solution \u2264 ")
  | _    => None})

let solve = choose(lang => switch lang {
  | "en" => Some("Solve automatically")
  | "fr" => Some("Résoud automatiquement")
  | _    => None})

let nb_tested = choose(lang => switch lang {
  | "en" => Some("problems tested in")
  | "fr" => Some("problèmes testés en")
  | _    => None})
