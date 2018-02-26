class Queue[+T] private (
    private[this] var leading: List[T],
    private[this] var trailing: List[T]
  ) {
  
  def enqueue[U >: T](x: U) = {
    new Queue[U](leading, x :: trailing)
  }

  private def mirror: Unit = {
    if (leading.isEmpty) {
      leading = trailing.reverse
      trailing = Nil
    }
  }

  def head: T = {
    require(!isEmpty, "Queue.head on empty queue")
    mirror
    leading.head
  }
  
  def dequeue: (T, Queue[T]) = {
    require(!isEmpty, "Queue.dequeue on empty queue")
    mirror
    val x :: leading1 = leading 
    (x, new Queue(leading1, trailing))
  }

  def isEmpty: Boolean = trailing.isEmpty && leading.isEmpty

  override def toString: String = {
    s"Queue${(leading ::: trailing.reverse).toString.drop(4)}"
  }
}

object Queue {
  def empty[T]: Queue[T] = new Queue(Nil, Nil)

  def apply[T](xs: T*): Queue[T] = new Queue(xs.toList, Nil)
}

object QueueTest extends App {
  val q: Queue[Int] = Queue.empty
  
  val q1 = q.enqueue(1).enqueue(2).enqueue(3).enqueue(4)
  
  val qAny: Queue[Any] = q1
  
  println(qAny.enqueue("Hello"))
  
  println(q1.dequeue._2.dequeue)
  println(q1)
}