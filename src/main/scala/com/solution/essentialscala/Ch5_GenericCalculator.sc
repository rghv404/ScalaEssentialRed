/*
This worksheet is specifically to test teh generic and higher order function knowledge gained in Chapter 5
to implement the generic version of Calculator interface something we implemented iat the end of chapter 4
 */
 //We'll abstract the calculation part of the Caculator suing teh Sum patter we saw
sealed trait Sum[+A, +B]{
  def fold[C](error: A => C, success: B => C): C = this match {
    case Failure(v) => error(v)
    case Success(v) => success(v)
  }
  def map[C](f: B => C): Sum[A, C] = this match {
    case Failure(v) => Failure(v)
    case Success(v) => Success(f(v))
  }
  def flatMap[AA >: A, C](f: B => Sum[AA, C]): Sum[AA, C] = this match {
    case Failure(v) => Failure(v)
    case Success(v) => f(v)
  }
}
final case class Failure[A](value: A) extends Sum[A, Nothing]
final case class Success[B](value: B) extends Sum[Nothing, B]

sealed trait Expression{

  def helper(l: Expression, r: Expression, func: (Double, Double) => Sum[String, Double]): Sum[String, Double] =
    l.eval.flatMap(
      left => r.eval.flatMap(
        right => func(left, right)
      )
    )

  def eval: Sum[String, Double] = this match {
    case Addition(l, r) => l.eval.flatMap(v => r.eval.flatMap(v2 => Success(v + v2)))
    case Subtraction(l, r) => l.eval.flatMap((left => r.eval.flatMap(right => Success(left - right))))
    case Division(l, r) => helper(l, r, (a, b) =>
      if (b == 0) Failure("Divisio by Zero") else Success(a / b))
    case SquareRoot(value) => value.eval.flatMap(v =>
      if (v > 0) Success(Math.sqrt(v))
      else Failure("Square root of negative number"))
    case Number(v) => Success(v)
  }
}
final case class Addition(left: Expression, right: Expression) extends Expression
final case class Subtraction(left: Expression, right: Expression) extends Expression
final case class Division(left: Expression, right: Expression) extends Expression
final case class SquareRoot(value: Expression) extends Expression
final case class Number(value: Int) extends Expression

assert(Addition(Number(1), Number(2)).eval == Success(3))
Addition(Number(1), Number(2)).eval
assert(SquareRoot(Number(-1)).eval == Failure("Square root of negative number"))
assert(Division(Number(4), Number(0)).eval == Failure("Division by zero"))
assert(Division(Addition(Subtraction(Number(8), Number(6)), Number(2)), Number(2)).eval == Success(2.0))