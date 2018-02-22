class Cell[+T](private[this] var content: T) {
  def read: T = content
  
 def write(x: T): Unit = {
    content = x
 }
}

object CellTest extends App {
  val c = new Cell("Hello")
  
  val c1: Cell[AnyRef] = c
  
  c1.write(new AnyRef)
  
  //val c = new Cell("Hello")
  
  //val o: Cell[AnyRef] = c
  
  //o.write(new AnyRef)
  
  //println(o.read)
  
  c.read.charAt(0)
}