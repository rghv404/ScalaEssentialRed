import scala.annotation.tailrec                                                  //import scala.annotation.tailrec

/*
Using polymorphism and then using pa􏰃ern matching implement a method
called next which returns the next TrafficLight in the standard
Red -> Green -> Yellow -> Red cycle. Do you think it is be􏰃er to implement
this method inside or outside the class? If inside, would you use pa􏰃ern
matching or polymorphism
 */

//sealed trait TrafficLight{
//  def next: TrafficLight
//}
//
//final case object Red extends TrafficLight {
//  def next: TrafficLight = Green
//}
//final case object Green extends TrafficLight {
//  def next: TrafficLight = Yellow
//}
//final case object Yellow extends TrafficLight {
//  def next: TrafficLight = Red
//}

//object NextLight{
//  def showNextLight(light: TrafficLight):TrafficLight = light.next
//}

// above is polymorphism

sealed trait TrafficLight{                                                       //defined trait TrafficLight
  def next: TrafficLight = this match {
    case Red => Green
    case Green => Yellow
    case Yellow => Green
  }
}

case object Red extends TrafficLight                                             //defined object Red
case object Green extends TrafficLight                                           //defined object Green
case object Yellow extends TrafficLight                                          //defined object Yellow

/*
We’re now going to write some methods that use a Calculation to
perform a larger calcula􏰀on. These methods will have a somewhat unusual
shape—this is a precursor to things we’ll be exploring soon—but if you
follow the pa􏰃erns you will be fine.
Create a Calculator object. On Calculator define methods + and - that
accept a Calculation and an Int, and return a new Calculation.
Here are some examples */
sealed trait Calculation                                                         //defined trait Calculation
final case class Success(result: Int) extends Calculation                        //defined class Success
final case class Failure(reason: String) extends Calculation                     //defined class Failure

object Calculator{                                                               //defined object Calculator
  def + (calculation: Calculation, a: Int): Calculation = calculation match {
    case Success(num: Int) => Success(num + a)
    case Failure(reason: String) => Failure(reason)
  }

  def - (calculation: Calculation, a: Int): Calculation = calculation match {
    case Success(num: Int) => Success(num - a)
    case Failure(reason: String) => Failure(reason)
  }

  def / (calculation: Calculation, a:Int): Calculation = calculation match {
    case Success(num: Int) => a match {
      case 0 => Failure("Division by zero")
      case _ => Success(num/a)
    }
    case Failure(reason: String) => Failure(reason)
  }
}
assert(Calculator.+(Success(1), 1) == Success(2))
assert(Calculator.-(Success(1), 1) == Success(0))
assert(Calculator.+(Failure("Badness"), 1) == Failure("Badness"))

assert(Calculator./(Success(4), 2) == Success(2))
assert(Calculator./(Success(4), 0) == Failure("Division by zero"))
assert(Calculator./(Failure("Badness"), 0) == Failure("Badness"))

//type level stuff
//powerful type system, generics in scala is particularly hard
//type is organized into Kinds
object Kinds {                                                                   //defined object Kinds
  //kind = type of type

  //level 0 type == kind that can be attached to values (most basic stuff)
  val number:Int = 42// Int/String are a regaular type that can be attached to a value

  // logic of a list shoudl ideally be regardless of the type it contain
  // below T determines the type of stuff List will hold
  class LinkedList[T] { // generic == level 1 Type (cannot be attached to a value on it's own
    // level 1 type takes arguments of level 0 type
    // code
  }

  val aList: LinkedList[Int] = ???
  // once we define aList as a list of Int by passing Int to level 1 it then
  // become

  //level 2 types -- type Fucntor takes argument which is
  class Functor[F[_]] // takes an type argument which in itself is generic
  val functorList = new Functor[List] //functorList is a level zero type
  //************* functorList is a level 0 type -- functor also acts as type constructor
  // also acts as a type constructor because it takes a type [F[_]] => gives Functor[F]

  //examples
  class HashMap[K, V] // levle 1
  val anAddressBook = new HashMap[String, String]

  class ComposedFunctor[F[_], G[_]] // lvel 2
  val aComposedFunctor = new ComposedFunctor[List, Option]

  class Formatter[F[_], T] // level 2 --type constructor, type lambdas
  val aFormatter = new Formatter[Option, String]
}


/*
Exerise 5
Use tail recursion to find length of below linked list
 */
@tailrec                                                                         //defined trait IntList
sealed trait IntList{
  def length: Int = {
    this match {
      case End => 0
      case Pair(_, tl) => 1 + tl.length
    }
  }

  def product: Int = {
    this match {
      case End => 1
      case Pair(hd, tl) => hd * tl.product
    }
  }

  def double: IntList = {
    this match {
      case End => End
      case Pair(hd, tl) => Pair(2*hd, tl.double)
    }
  }
}

case object End extends IntList                                                  //defined object End
final case class Pair(head: Int, tail: IntList) extends IntList                  //defined class Pair

val example = Pair(1, Pair(2, Pair(3, End)))                                     //example: Pair = Pair(1,Pair(2,Pair(3,End)))
assert(example.length == 3)
assert(example.tail.length == 2)
assert(End.length == 0)

assert(example.product == 6)
assert(example.tail.product == 6)
assert(End.product == 1)

assert(example.double == Pair(2, Pair(4, Pair(6, End))))
assert(example.tail.double == Pair(4, Pair(6, End)))
assert(End.double == End)
