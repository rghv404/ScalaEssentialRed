/*
2.2 Implement isSorted, which checks whether an Array[A] is sorted according to a given comparison function:
 */

def isSorted[A](arr: Array[A], f: (A, A) => Boolean): Boolean = {
  def loop[A](idx:Int): Boolean = {
    if (idx >= arr.length - 1) true
    else if (f(arr(idx), arr(idx+1))) false
    else loop(idx + 1)
  }
  loop(0)
}

val lst: Array[Int] = Array(1,2,3,6,7,4)
isSorted(lst, (a:Int, b:Int) => a > b)