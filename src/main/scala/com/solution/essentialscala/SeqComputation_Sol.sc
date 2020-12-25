/*
Convert Int linked CustomList to generic using scala types
 */

sealed trait LinkedCustomList[T]{
  def length: Int = {
    this match {
      case Pair(_, t) => 1 + t.length
      case End() => 0
    }
  }
  def contains(value: T): Boolean = {
    this match {
      case End() => false
      case Pair(hd, tl) => if (hd equals value) true else tl.contains(value)
    }
  }

  def apply(n: Int): T = {
    this match {
      case Pair(hd, tl) => if (n == 0) hd else tl(n-1)
      case End() =>  throw new Exception("Bad things happened")
    }
  }
}
final case class End[T]() extends LinkedCustomList[T]
final case class Pair[T](head: T, tail: LinkedCustomList[T]) extends LinkedCustomList[T]

val example = Pair(1, Pair(2, Pair(3, End())))
assert(example.length == 3)
assert(example.tail.length == 2)
assert(End().length == 0)

val example2 = Pair(1, Pair(2, Pair(3, End())))
assert(example2.contains(3) == true)
assert(example2.contains(4) == false)
assert(End().contains(0) == false)
//example.contains("not an Int")

val example3 = Pair(1, Pair(2, Pair(3, End())))
assert(example3(0) == 1)
assert(example3(1) == 2)
assert(example3(2) == 3)
assert(try {
  example3(3)
  false
} catch {
  case e: Exception => true
})

/*
Change apply so it returns a Result, with a failure case indica􏰀ng
what went wrong. Here are some test cases to help you:
 */
sealed trait Result[A]
case class Success[A](result: A) extends Result[A]
case class Failure[A](reason: String) extends Result[A]

sealed trait BetterLinkedCustomList[T]{
  def apply(n: Int): Result[T] = {
    this match {
      case BetterPair(hd, tl) => if (n == 0) Success[T](hd) else tl(n-1)
      case BetterEnd() =>  Failure[T]("Index out of bounds")
    }
  }
}
final case class BetterEnd[T]() extends BetterLinkedCustomList[T]
final case class BetterPair[T](head: T, tail: BetterLinkedCustomList[T]) extends BetterLinkedCustomList[T]

val example4 = BetterPair(1, BetterPair(2, BetterPair(3, BetterEnd())))
assert(example4(0) == Success(1))
assert(example4(1) == Success(2))
assert(example4(2) == Success(3))
assert(example4(3) == Failure("Index out of bounds"))

/*
Enter the Higher order function
 */
