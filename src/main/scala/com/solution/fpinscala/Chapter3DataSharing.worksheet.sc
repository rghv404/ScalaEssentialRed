import scala.annotation.tailrec

sealed trait List[+A]
    
final case object Nil extends List[Nothing]
final case class Cons[+A](head: A, tail: List[A]) extends List[A]

// Excercise 3.1 Remove the head of the List -- meaning return only the tail
def tail[A](lst: List[A]): List[A] = lst match {
        case Nil => Nil
        case Cons(head, tail) => tail
}

// Excercise 3.2 Alter the head of the List
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

def append[A](lst1: List[A], lst2: List[A]): List[A] = lst1 match {
    case Nil => lst2
    case Cons(head, tail) => Cons(head, append(tail, lst2))
}

val intList: List[Int] = Cons(1, Cons(2, Cons(3, Cons(13, Nil))))
tail(intList)
setHead(intList, 90)
drop(intList, 3)
dropWhile(intList, (x: Int) => x < 3)
append(intList, Cons(12, Cons(144, Nil)))

// Excercise 3.6 
def init[A](lst: List[A]): List[A] = lst match {
    case Nil => Nil
    case Cons(h, Nil) => Nil
    case Cons(h, tail) => Cons(h, init(tail))
}

init(intList)

// Generalizing these special function using foldRight
def foldRight[A, B](lst: List[A], z:B)(f: (A, B) => B): B = lst match {
    case Nil => z
    // case Cons(0, tail) => 0.0
    case Cons(head, tail) => f(head, foldRight(tail, z)(f))
}

// now sum and product can be shown using foldRight
def sum(l: List[Int]) = foldRight(l, 0)((x, y) => x + y)
def product(l: List[Double]) = foldRight(l, 1.0)((x, y) => x * y)
def product2(l: List[Double]) = foldRight(l, 1.0)(_ * _)
// foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
def length[A](l: List[A]) = foldRight(l, 0)((x, y) => 1 + y)

length(intList)

// Since the foldRight is not tail recursive and will resul tin a stackOVerflow error for large Lists 
// (not stack-safe).
@tailrec
final def foldLeft[A, B](lst: List[A], z: B)(f: (B, A) => B): B = lst match {
    case Nil => z
    case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
}

// implementing lis functions using foldLeft
def sumLeft(l: List[Int]) = foldLeft(l, 1)(_ + _)
def prodLeft(l: List[Double]) = foldLeft(l, 1.0)(_ * _)
def lengthLeft[A](l: List[A]) = foldLeft(l, 0)((x, y) => 1 + x)

// call above funcs
sumLeft(intList)
// prodLeft(intList)
length(intList)

// //Exercise 3.12 - fucntion to reverse a linkedList via foldLeft
// def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc, h) => Cons(h, acc))

// Exercise 3.13 - implement foldRight via foldLeft
// def foldRightviaFoldLeft[A, B](lst: List[A], z: B)(f: (A, B) => B): B = {
//     foldLeft(reverse(lst), z)(f)
// }


//The other implementations build up a chain of functions which, when called, results in the operations being performed
// with the correct associativity. We are calling `foldRight` with the `B` type being instantiated to `B => B`, then
//  calling the built up function with the `z` argument.
def foldRightviaFoldLeft_1[A, B](lst: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(lst, (b: B) => b) ((g, a) => b => g(f(a, b)))(z)
}

def foldLeftviaFoldRight[A, B](lst: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(lst, (b: B) => b)((a, g) => b => g(f(b, a)))(z)
}

def appendviaFoldRight[A](l1: List[A], l2: List[A]): List[A] = foldRight(l1, l2)(Cons(_, _))
appendviaFoldRight(intList, intList)

// concat to faltten a list of list to list

// def applyFun[Int](lst: List[Int]): List[Int] = foldLeft(lst, Nil: List[Int])((tail, h) => Cons(h + 1, tail))

def doubleToString[Double](lst: List[Double]): List[String] = foldLeft(lst, Nil: List[String])((tail, h) => Cons(h.toString(), tail))

// applyFun(intList)
doubleToString(intList)


// Exersise 3.16 - fucntion to add 1 to each element
def map[A, B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(head, tail) => Cons(f(head), map(tail)(f))
}

def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(head, tail) => if (f(head)) Cons(head, tail) else filter(tail)(f)
}

def filterviaMap[A](as: List[A])(f: A => Boolean) = map(as)(f)

filter(intList)(x => x > 3)
// basically we can't use map to filter as evident below
filterviaMap(intList)(x => x > 3)

// def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = as match {
//     case Nil => Nil
//     // case Cons(head, tail) => f(head) ++ flatMap(tail)(f)
// }