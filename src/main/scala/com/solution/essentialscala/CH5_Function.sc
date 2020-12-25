/*
Here comes the function abstraction over the repetetive and now understood structural recursion
 */
sealed trait IntCustomList {
  def fold[T](end: T, f: (Int, T) => T): T = {
    this match {
      case End => end
      case Pair(hd, tl) => f(hd, tl.fold(end, f))
    }
  }
  def sum: Int = fold[Int](0, (a, b) => a + b)
  def product: Int = fold[Int](1, (a, b) => a * b)
  def length: Int = fold[Int](0, (_, b) => 1 + b)
  def double: IntCustomList = fold[IntCustomList](End, (hd, tl) => Pair(2*hd, tl))
}

final case object End extends IntCustomList
final case class Pair(head: Int, tail: IntCustomList) extends IntCustomList

val example = Pair(1, Pair(2, Pair(3, End)))
assert(example.product == 6)
assert(example.length == 3)
assert(example.sum == 6)
assert(example.double == Pair(2, Pair(4, Pair(6, End))))


/*
Fold Function for Generix LinkedCustomList
* Fold will have two params representing each class the trait can be. Here end and pair
* Since End takes no params it simply return type B and pair takes two params
 */

sealed trait LinkedCustomList[A]{
  def fold[B](end: B, node: (A, B) => B): B = {
    this match {
      case End() => end
      case Node(hd, tl) => node(hd, tl.fold(end, node))
    }
  }
}

final case class End[A]() extends LinkedCustomList[A]
final case class Node[A](head: A, tl: LinkedCustomList[A]) extends LinkedCustomList[A]

/*
Binary generic Tree
trenode function parameter takes B because the class associated with it has both recursive params
 */
sealed trait Tree[A]{
  def fold[B](leaf: A => B)(treeNode: (B, B) => B): B = {
    this match {
      case Leaf(elem) => leaf(elem)
      case TreeNode(l, r) => treeNode(l.fold(leaf)(treeNode), r.fold(leaf)(treeNode))
    }
  }
//  def toString: A = fold[String](str => s"$str")((l,r) => l + " " + r)
}
final case class Leaf[A](element: A) extends Tree[A]
final case class TreeNode[A](left: Tree[A], right: Tree[A]) extends Tree[A]

val tree: Tree[String] =
  TreeNode(TreeNode(Leaf("To"), Leaf("iterate")),
    TreeNode(TreeNode(Leaf("is"), Leaf("human,")),
      TreeNode(Leaf("to"), TreeNode(Leaf("recurse"), Leaf("divine")))))

// not sure why I can;t have this function above in Tree class
tree.fold[String](str => str)((l,r) => l + " " + r)

/*
5.4.1.1 Exercise Pairs
 */
case class GenericPair[A, B](one: A, two: B)
val pair = GenericPair[String, Int]("hi", 2)
val pairWOType = GenericPair("h1", 2)
pair.one
pair.two

/*
Generic Sum Types
Also Implementing Fold for the sum -- as part of 5.4.6.3 exercise
 */
sealed trait Sum[A, B]{
  def fold[T](left: A => T, right: B => T): T = {
    this match {
      case Left(value) => left(value)
      case Right(value) => right(value)
    }
  }
}
case class Left[A, B](value:A) extends Sum[A, B]
case class Right[A, B](value:B) extends Sum[A, B]

Left[Int, String](1).value
Right[Int, String]("foo").value
val sum: Sum[Int, String] = Right("foo")
sum match {
  case Left(x) => x.toString
  case Right(x) => x
}

/*
Begin the Option we have
 */
sealed trait Maybe[A]{
  def fold[B](emp: B, full: (A) => B): B ={
    this match {
      case Empty() => emp
      case Full(value) => full(value)
    }
  }
}
final case class Full[A](value:A) extends Maybe[A]
final case class Empty[A]() extends Maybe[A]

val perhaps: Maybe[Int] = Empty[Int]
val perhaps: Maybe[Int] = Full(1)

/*
Create map fucntion using recursion structure to map from CustomList[A] to CustomList[B
 */

sealed trait NewLinkedCustomList[A]{
  // implement map using logic of fold
  def map[B](f: A => B): NewLinkedCustomList[B] = {
    this match{
      case End2() => End2()
      case Pair2(hd, tl) => Pair2(f(hd), tl.map(f))
    }
  }
}
final case class End2[A]() extends NewLinkedCustomList[A]
final case class Pair2[A](head: A, tail: NewLinkedCustomList[A]) extends NewLinkedCustomList[A]

/*
Implement a flatmap for Maybe sum class that takes Maybe[A] and give Maybe[B
by applyin the A => Maybe[B]
 */
sealed trait MyMaybe[A]{
  def map[B](fn: A => B): MyMaybe[B] = {
    this match {
      case Empty2() => Empty2[B]()
      case Full2(value) => Full2(fn(value))
    }
  }

  def flatMap[B](fn: A => MyMaybe[B]): MyMaybe[B] = {
    this match {
      case Empty2() => Empty2[B]()
      case Full2(value) => fn(value)
    }
  }

  def mapInFlatM[B](fn: A => B): MyMaybe[B] = this.flatMap[B](v => Full2(fn(v)))

}
final case class Empty2[A]() extends MyMaybe[A]
final case class Full2[A](value: A) extends MyMaybe[A]


val CustomList: LinkedCustomList[Int] = Node(1, Node(2, Node(3, End())))
//double all element in the CustomList
CustomList.fold[LinkedCustomList[Int]](End(), (hd, tl) => Node(2*hd, tl))
CustomList.fold[LinkedCustomList[Int]](End(), (hd, tl) => Node(hd+1, tl))
CustomList.fold[LinkedCustomList[Int]](End(), (hd, tl) => Node(hd/3, tl))
// abov uses generic fold function, this could alos be simplified using map


/*
Using scala flatmap now that we undersatn d the basicas of it
 */
 val CustomList2 = CustomList(1, 2, 3)
CustomList2.flatMap(v => CustomList(v, -v))

// using flatmap return a CustomList[Maybe[Int]] containing None for the odd elements. Hint: If x % 2 == 0thenxis even.
val CustomList3: CustomList[MyMaybe[Int]] = CustomList(Full2(3), Full2(2), Full2(1))
CustomList3.flatMap(v => v match {
  case Empty2() => CustomList(Empty2())
  case Full2(item) => if (item % 2 == 0) CustomList(Full2(item)) else CustomList(None)
})
CustomList3.map(maybe => maybe.flatMap[Int](v => if (v%2 == 0) Full2(v) else Empty2()))

sealed trait Sum[A, B]{
  def fold[C](failure: A => C, success: B => C): C =
    this match {
      case Success(v) => success(v)
      case Failure(v) => failure(v)
    }
  // the point of map is to map sucesses not failures so the f shold convert
  // the appropriate Type i.e A or B whichever represents failure
  def map[C](f: B => C ): Sum[A, C] =
    this match {
      case Failure(v) => Failure(v)
      case Success(v) => Success(f(v))
    }

  def flatMap[C](f: B => Sum[A, C]): Sum[A, C] =
    this match {
      case Failure(v) => Failure(v)
      case Success(v) => f(v)
    }
}
final case class Failure[A, B](value: A) extends Sum[A, B]
final case class Success[A, B](value: B) extends Sum[A, B]

/*
covariant sum
 */
sealed trait covSum[+A, +B]{
  def flatMap[A, C](f: B => covSum[A, C]): covSum[A, C] = {
    this match {
      case covFailure(value) => covFailure(value)
      case covSuccess(value) => f(value)
    }
  }
}
final case class covFailure[A, Nothing](value: A) extends covSum[A, Nothing]
final case class covSuccess[Nothing, B](value: B) extends covSum[Nothing, B]
