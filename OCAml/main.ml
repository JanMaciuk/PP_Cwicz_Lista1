open Printf

(*Zadanie 1:*)
  let rec flatten1 listOfLists = match listOfLists with
  | [] -> [] (*Pionowe kreski są trochę jak case, separują*)
  | head::tail -> List.append head (flatten1 tail)


(*Zadanie 2:*)
  let rec count list searched = match list with
  | [] -> 0
  | head::tail -> (if(head==searched) then 1 else 0) + (count tail searched)
  (*// Jeżeli pierwszy element jest szukanym to zwiększam licznik o 1 i przechodzę dalej.*)


(*Zadanie 3:*)
  let rec replicate element (times:int) =
    if(times>0) then element::(replicate element (times-1)) else []
    (*Zwracana lista : dodaj element do listy dopóki times > 0*)


(*Zadanie 4:*)
  let rec sqrList (list:int list) = match list with 
  | [] -> []
  | head::tail -> (head*head)::(sqrList tail)
  (*Dodaj kwadrat pierwszego elementu do reszty*)


(*Zadanie 5:*)
  let rec listReverse list = match list with
  | [] -> []
  | head :: tail -> (listReverse tail) @ [head]

  let palindrome list = list = (listReverse list)
  (*jeżeli lista jest identyczna z jej odwrotnością - jest palindromem*)

  
(*Zadanie 6:*)
  let rec listLength list : int = match list with
  | [] -> 0
  | head::tail -> 1 + (listLength tail)


  (*Zadanie 1:*)
  let result1Ints = flatten1 [[1; 2; 3]; [4; 5]; [6; 7; 8]; [9]]
  let _ = List.iter (printf "%d ") result1Ints
  let _ = printf "\n"

  let result1Strings = flatten1 [["a"; "b"]; ["c"; "d"; "e"]; ["f"; "g"]; ["h"]]
  let _ = List.iter (printf "%s ") result1Strings
  let _ = printf "\n\n"

  (*Zadanie 2:*)
  let _ = (printf "%d ") (count [1; 2; 2; 5; 5; 1; 1; 2; 6] 2)
  let _ = printf "\n\n"

  (*Zadanie 3:*)
  let result3 = replicate "repeat"  5
  let _ = List.iter (printf "%s ") result3
  let _ = printf "\n\n"

  (*Zadanie 4:*)
  let result4 = sqrList [1; 2; 3; 4; 5]
  let _ = List.iter (printf "%d ") result4
  let _ = printf "\n\n"

  (*Zadanie 5:*)
  
  let _ = print_endline (string_of_bool (palindrome [1;2;3;4;5]))
  let _ = printf "\n"

   (*Zadanie 6:*)

    let result6 = listLength [1; 2; 3; 4; 5]
    let _ = printf "%d" result6
    let _ = printf "\n\n"