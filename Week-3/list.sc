trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

class Cons[T](val _head: T, val _tail: List[T]) extends List[T] {
  def isEmpty = false
  def head = _head
  def tail = _tail
  override def toString = head + " -> " + tail
}

class Nil[T] extends List[T] {
  def isEmpty = true
  def head = throw new NoSuchElementException("nil.head")
  def tail = throw new NoSuchElementException("nil.tail")
  override def toString = "Nil"
}

def singleton[T](x: T) = new Cons[T](x, new Nil[T])


def nth[T](idx: Int, list: List[T]): T = {
  if (idx != 0 && list.isEmpty) throw new IndexOutOfBoundsException;
  else if (idx == 0) list.head
  else nth[T](idx - 1, list.tail)
}

singleton[Int](1)
singleton[Boolean](true)

val list = new Cons(1, new Cons(2, new Cons(3, new Nil)))

nth(2, list)
nth(-1, list)
