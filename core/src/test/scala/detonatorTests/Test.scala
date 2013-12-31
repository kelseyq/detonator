package detonatorTests

import com.kelseyinnis.detonator.Macros

import org.specs2.mutable._

class PrimitivesSpec extends Specification {

  "Primitive singletons" should {
    "give the same value over multiple invocations" in {
        val test1: String = PrimitiveObject.sing("hello")
        Thread.sleep(1000)
        val test2: String = PrimitiveObject.sing("cruel")
      Thread.sleep(1000)
        val test3: String = PrimitiveObject.sing("world")
      (test1 must_== test2) and (test2 must_== test3)
    }
  }
}

//object Test extends App {
//  val result = "hello world"
//  val test: String = PrimitiveObject.sing("hi")
//  println(test.asInstanceOf[AnyRef].getClass.getSimpleName)
//  println(test)
//  println(test)
//  Thread.sleep(1000)
//  println(test)
//  Thread.sleep(1000)
//  val test2: String = PrimitiveObject.sing("hi3")
//  println(test2.asInstanceOf[AnyRef].getClass.getSimpleName)
//  println(test2)
//  Thread.sleep(1000)
//  println(test2)
//  println(test2)
//  def intTest: Long = PrimitiveObject.intSing("hi4")
//  println(intTest.asInstanceOf[AnyRef].getClass.getSimpleName)
//  println(intTest)
//  println(intTest)
//  println(intTest)
//  Thread.sleep(1000)
//  println(intTest)
//  val doubleTest: String = PrimitiveObject.doubleSing("hi5", 7)
//  println(doubleTest.asInstanceOf[AnyRef].getClass.getSimpleName)
//  println(doubleTest)
//  println(doubleTest)
//  println(doubleTest)
//  Thread.sleep(1000)
//  println(doubleTest)
//  val listTest: List[Int] = PrimitiveObject.listSing(Some(365))
//  println(listTest.asInstanceOf[AnyRef].getClass.getSimpleName)
//  println(listTest)
//  println(listTest)
//
//  val ccTest = ComplexObject.caseClassParam(Banana("hi", 5))
//  println(ccTest.asInstanceOf[AnyRef].getClass.getSimpleName)
//  println(ccTest)
//  println(ccTest)
//
//  val caseClassReturned = ComplexObject.caseClassReturned("apple")
//  println(caseClassReturned.asInstanceOf[AnyRef].getClass.getSimpleName)
//  println(caseClassReturned)
//  println(caseClassReturned)
//
//  val functionTest = ComplexObject.functionDef(100)
//  println(functionTest.asInstanceOf[AnyRef].getClass.getSimpleName)
//  println(functionTest)
//  println(functionTest)
//
//  val valAsFunctionTest = ComplexObject.functionVal(List(1,2,3))
//  println(valAsFunctionTest.asInstanceOf[AnyRef].getClass.getSimpleName)
//  println(valAsFunctionTest)
//  println(valAsFunctionTest)
//
//  val curriedTest = ComplexObject.curried("this is a string")
//  println(curriedTest.asInstanceOf[AnyRef].getClass.getSimpleName)
//  println(curriedTest)
//  println(curriedTest)
//}

object PrimitiveObject {
  val sing = Macros.singleton{ (test: String) => { val time = System.currentTimeMillis().toString; time } }
  lazy val intSing = Macros.singleton { (test: String) => { val time = System.currentTimeMillis(); println(test + " " + time); time } }
  val doubleSing = Macros.singleton{ (test1: String, test2: Int) => { val time = System.currentTimeMillis().toString; println(test1 + " " + time + " " + test2); time } }
  val listSing = Macros.singleton { (testParam: Option[Int]) => println("listTest " + System.currentTimeMillis()); testParam.map(v => List(v, v, v)).getOrElse(List(0,0,0))}
}

object ComplexObject {

  val caseClassParam = Macros.singleton{ (ccParam: Banana) => { println(System.currentTimeMillis().toString); ccParam.toString } }
  val caseClassReturned = Macros.singleton{ (test: String) => { println(System.currentTimeMillis().toString); Banana(test, 200) } }

  def newFunction(something: Int): String = (something * 2).toString
  val functionDef = Macros.singleton {newFunction _}

  def curryable(oneParam: Banana, twoParam: String) = oneParam.test2 + twoParam.length
  val curried = Macros.singleton(curryable(Banana("hola", 1000), _: String))

  val valFunction = (something: List[Int]) => (something.map(_ * 3)).toString + System.currentTimeMillis()
  val functionVal = Macros.singleton(valFunction)

//  //TODO: should fail
//  val wrappedString = "hello"
//  val stringAsFunction = Macros.singleton(wrappedString)
//
//  //TODO: should fail
//  val nonPrimitive = Banana("yo", 10)
//  val nonFunction = Macros.singleton(nonPrimitive)

  val fakeFunction = System.currentTimeMillis()
  val valAsFunction = Macros.singleton {fakeFunction _}
}

case class Banana(test: String, test2: Int)
