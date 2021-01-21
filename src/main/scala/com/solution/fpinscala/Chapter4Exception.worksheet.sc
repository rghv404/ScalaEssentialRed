// Exercise 4.1 - Welcome to Options

sealed trait CustomOption[+A]{
    def map[B](f: A => B): CustomOption[B] = this match {
        case None => None
        case Some(value) => Some(f(value))
    }

    def flatMap[B](f: A => CustomOption[B]): CustomOption[B] = this match {
        case None => None
        case Some(value) => f(value)
    }

    def getOrElse[B >: A](default: => B): B = this match {
        case None => default
        case Some(b) => b
    }

    def orElse[B >: A](ob: => CustomOption[B]): CustomOption[B] = this match {
        case None => ob
        case _ => this
    }

    def filter(f: A => Boolean): CustomOption[A] = flatMap(a => if (f(a)) Some(a) else None)

    def filter_Orig(f: A => Boolean): CustomOption[A] = this match {
        case Some(value) if f(value) => this
        case _ => None
    }
}

case object None extends CustomOption[Nothing]
case class Some[+A](get: A) extends CustomOption[A]

def mean(xs: Seq[Double]): CustomOption[Double] = if (xs.isEmpty) None else Some(xs.sum / xs.length)
def variance(xs: Seq[Double]): CustomOption[Double] = mean(xs) flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
val sampleList: Seq[Double] = Seq(1,2,3,4)
variance(sampleList)


// Exercise 4.3 -- map2 that combines two Option values using a binary function
// If Either value is None then the result is too

def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a flatMap (aa => b map (bb => f(aa, bb)))
// I can comfortably write using pattern matching but the above  flatmap and map chain still fucks me up
def map2Orig[A, B, C](a: CustomOption[A], b: CustomOption[B])(f: (A, B) => C): CustomOption[C] = a match {
    case None => None
    case Some(aa) => b match {
        case None => None
        case Some(bb) => Some(f(aa, bb))
    }
}

// Imaginign map2 as a map of seq of elements which gives out None if even one element is None
def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case head :: next => head flatMap(hh => sequence(next) map (hh :: _) )
}

// now use above function in practice to map a list of strings 
def parseInt(a: List[String]): Option[List[Int]] = sequence(a map(aa => Try(aa.toInt)))