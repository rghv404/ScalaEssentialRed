import scala.annotation.tailrec
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

// Exercise 3.25 -- size that counts number of node
// @tailrec
final def fold[A, B](node: Tree[A])(f: A => B)(g: (B, B) => B): B = node match {
    case Leaf(value) => f(value)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
}

final def map[A, B](node: Tree[A])(f: A => B): Tree[B] = node match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
}

val basicTree: Tree[Int] = Branch(Branch(Leaf(2), Branch(Leaf(3), Leaf(4))), Branch(Leaf(5), Leaf(6)))
val size: Int = fold(basicTree)(a => 1)(1 + _ + _)
val sum: Int = fold(basicTree)(a => a)(_ + _)
val max: Int = fold(basicTree)(a => a)(_ max _)
val maxDepth: Int = fold(basicTree)(a => 0)((z, treeVal) => 1 + (z max treeVal)) //seems wrong
 // add 1 to each value of tree using map
val addOne = map(basicTree)(1 + _)

// map using fold Left
def mapViaFold[A, B](node: Tree[A])(f: A => B): Tree[B] = fold(node)(a => Leaf(f(a)): Tree[B])(Branch(_, _)) // I still cant grasp mu head fully around this