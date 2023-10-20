import scala.annotation.tailrec

def flatten1[A](listOfLists: List[List[A]]): List[A] = listOfLists match {
  case Nil => Nil
  case head :: tail => head ++ flatten1(tail)
  // head jest pierwszym elementem (pierwszą listą) a tail to reszta (pozostałe listy)
  // Dodaj elementy pierwszej listy do reszty
}

def count[A](list: List[A], searched: A): Int = list match {
  case Nil => 0
  case head :: tail => (if (head == searched) 1 else 0) + count(tail, searched)
  // Jeżeli pierwszy element jest szukanym to zwiększam licznik o 1 i przechodzę dalej.
}

def replicate[A](element: A, times: Int): List[A] = {
  if (times > 0) element :: replicate(element, times - 1)
  else Nil
  //Zwracana lista : dodaj element do listy dopóki times > 0
}

def sqrList(list: List[Int]): List[Int] =  list match {
  case Nil => Nil
  case head :: tail => head * head :: sqrList(tail)
  // Dodaj kwadrat pierwszego elementu do reszty
}

@tailrec
def palindrome[A](list: List[A]): Boolean = list match {
  case Nil => true // Jeżeli usunęliśmy wszystkie elementy przy sprawdzaniu to mamy palindrom
  case head :: Nil => true // Ostatni element, jeżeli tu dotarliśmy to mamy palindrom
  case head :: tail => if (head == tail.last) palindrome(tail.dropRight(1)) else false
  // Jeżeli pierwszy element jest równy ostatniemu to wykonaj ponownie bez pierwszego i ostatniego elementu
} // Na wykładzie była przedstawiona metoda reverse, ale zakładam że nie o to chodziło w ćwiczeniu (byłoby banalne)

def listLength[A](list: List[A]): Int = list match{
  case Nil => 0
  case head :: tail => 1+ listLength(tail)
}
@main
def main(): Unit = {
  println("Start")
  println(flatten1(List(List(1, 2, 3), List(4, 5), List(6, 7, 8))))
  println(count(List(2,7,4,3,2,2), 2))
  println(count(List("a","b","c","a","c"), "a"))
  println(replicate("test", 5))
  println(sqrList(List(1,2,3,4,5)))
  println(palindrome(List(1,2,3,2,1)))
  println(listLength(List("a","b","c","a","c")))
}