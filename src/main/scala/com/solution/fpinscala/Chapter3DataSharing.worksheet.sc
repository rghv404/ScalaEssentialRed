
sealed trait List[+A]
    
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

// Excercise 3.1 Remove the head of the list -- meaning return only the tail
def tail[A](lst: List[A]): List[A] = lst match {
        case Nil => Nil
        case Cons(head, tail) => tail
}

// Excercise 3.2 Alter the head of the list
def setHead[A](lst: List[A], newHead: A): List[A] = lst match {
    case Nil => Nil
    case Cons(head, tail) => Cons(newHead, tail)
}

// Generalize tail to use drop which removes the first n element\
def drop[A](lst: List[A], n: Int): List[A] =  n match {
    case 0 => lst
    case k => lst match {
        case Nil => Nil
        case Cons(head, tail) => drop(tail, k - 1)
    }
}

def dropWhile[A](lst: List[A], f: A => Boolean): List[A] = lst match {
    case Nil => Nil
    case Cons(head, tail) => f(head) match {
        case true => dropWhile(tail, f)
        case false => Cons(head, tail)
    }
}

val intList: List[Int] = Cons(1, Cons(2, Cons(3, Cons(13, Nil))))
tail(intList)
setHead(intList, 90)
drop(intList, 3)
dropWhile(intList, (x: Int) => x < 3)
