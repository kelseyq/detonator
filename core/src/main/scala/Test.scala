object Test extends App {
  val result = "hello world"
  val test: String = TheObject.sing("hi")
  println(test.asInstanceOf[AnyRef].getClass.getSimpleName)
  println(test)
  println(test)
  Thread.sleep(1000)
  println(test)
  Thread.sleep(1000)
  val test2: String = TheObject.sing("hi3")
  println(test2.asInstanceOf[AnyRef].getClass.getSimpleName)
  println(test2)
  Thread.sleep(1000)
  println(test2)
  println(test2)
  def intTest: Long = TheObject.intSing("hi4")
  println(intTest.asInstanceOf[AnyRef].getClass.getSimpleName)
  println(intTest)
  println(intTest)
  println(intTest)
  Thread.sleep(1000)
  println(intTest)
  val doubleTest: String = TheObject.doubleSing("hi5", 7)
  println(doubleTest.asInstanceOf[AnyRef].getClass.getSimpleName)
  println(doubleTest)
  println(doubleTest)
  println(doubleTest)
  Thread.sleep(1000)
  println(doubleTest)
}

object TheObject {
  val sing = Macros.singleton{ (test: String) => { val time = System.currentTimeMillis().toString; println(time); time } }
  val intSing = Macros.singleton { (test: String) => { val time = System.currentTimeMillis(); println(test + " " + time); time } }
  val doubleSing = Macros.singleton{ (test1: String, test2: Int) => { val time = System.currentTimeMillis().toString; println(test1 + " " + time + " " + test2); time } }
}
