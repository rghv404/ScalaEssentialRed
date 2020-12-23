
sealed trait List[+A] {
    // Excercise 3.1 Remove the head of the list -- meaning return only the tail
    def tail[A](lst: List[A]): List[A] = lst match {
        case Nil => Nil
        case Cons(h, t) => t
    }
}
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

