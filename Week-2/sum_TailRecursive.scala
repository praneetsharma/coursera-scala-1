/**
  * Created by prsharma on 1/29/2017.
  */
object sum_TailRecursive {

  def sum(f: Int => Int, a: Int, b: Int): Int = {
    def sumAux(acc: Int, a: Int): Int = {
      if (a > b) acc
      else sumAux(acc + f(a), a + 1)
    }
    sumAux(0, a)
  }

  def sumNumbers(a: Int, b: Int): Int = sum(x => x, a, b)

  def main(args: Array[String]): Unit = {
    println(sumNumbers(1,5))
  }

}
