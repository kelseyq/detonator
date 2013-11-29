object Test extends App {
  val result = "hello world"
  val test = TheObject.sing("hi")
  println(test)
  println(test)
  Thread.sleep(1000)
  println(test)
  Thread.sleep(1000)
  val test2 = TheObject.sing("hi3")
  println(test2)
  Thread.sleep(1000)
  println(test2)
  println(test2)
  def intTest = TheObject.intSing("hi4")
  println(intTest)
  println(intTest)
  println(intTest)
  Thread.sleep(1000)
  println(intTest)
}

object TheObject {
  val sing = Macros.singleton{ (test: String) => { val time = System.currentTimeMillis().toString; println(time); time } }
  val intSing = Macros.singleton { (test: String) => { val time = System.currentTimeMillis(); println(test + " " + time); time } }
}
