// WEEK 2 LECTURE

// CURRYING

def product(f: Int => Int)(a: Int, b: Int): Int = {
  def productAux(acc: Int, a: Int): Int = {
    if (a > b) acc
    else productAux(acc * f(a), a + 1)
  }
  productAux(1, a)
}
def factorial(a: Int) = product((x: Int) => x)(1, a)
factorial(5)

def genericOperation(operatorFunc: (Int, Int) => Int, defaultValue: Int)(f: Int => Int)(a: Int, b: Int): Int = {
  if (a > b) defaultValue
  else operatorFunc(f(a), genericOperation(operatorFunc, defaultValue)(f)(a + 1, b))
}
def mySum(a: Int, b: Int) = genericOperation((a,b) => a + b, 0)(x => x)(a, b)
mySum(1,5)


// Fixed Points

val tolerance: Double = 0.001
def abs (x: Double): Double = if (x < 0) -1*x else x
def isCloseEnough (x: Double, y: Double): Boolean = {
  if (abs((x - y) / x) < tolerance * x) true
  else false
}
def fixedPoint (f: Double => Double)(firstGuess: Double): Double = {
  def fixedPointAux(guess: Double): Double = {
    val nextGuess = f(guess)
    if (isCloseEnough(guess, nextGuess)) nextGuess
    else fixedPointAux(nextGuess)
  }
  fixedPointAux(firstGuess)
}
def averageDamp(f: Double => Double)(x: Double): Double = (x + f(x)) / 2
def sqrt(x: Double): Double = fixedPoint(averageDamp(y => x/y))(1)

fixedPoint(x => 1 + x / 2)(1)

sqrt(2)