object Test extends App {
  val result = "hello world"
  val test = TheObject.sing("hi")
  println(test)
  println(test)
  Thread.sleep(1000)
  println(test)
  Thread.sleep(1000)
  val test2 = TheObject.sing("hi")
  println(test2)
  Thread.sleep(1000)
  println(test2)
  println(test2)
  def something = TheObject.sing("hi")
  println(something)
  println(something)
  println(something)
  Thread.sleep(1000)
  println(something)
}

object TheObject {
  val sing = Macros.singleton{ (test: String) => { println(test); test } }
}
