package detonatorTests

import com.kelseyinnis.detonator.Macros

import org.specs2.mutable._

class PrimitivesSpec extends Specification {

  "Primitive singletons" should {
    "give the same String value over multiple invocations" in {
        val test1: String = PrimitiveObject.sing("hello")
        Thread.sleep(1000)
        val test2: String = PrimitiveObject.sing("cruel")
      Thread.sleep(1000)
        val test3: String = PrimitiveObject.sing("world")
      (test1 must_== test2) and (test2 must_== test3)
    }

    "give the same Long value over multiple invocations" in {
      val test1: Long = PrimitiveObject.longSing("alphabet")
      val test2: Long = PrimitiveObject.longSing("soup")
      (test1 must_== test2) and (test2 must_== 8)
    }

    "give the same value over multiple invocations given multiple parameters" in {
      val test1: String = PrimitiveObject.twoParamSing("mach", 5)
      Thread.sleep(1000)
      val test2: String = PrimitiveObject.twoParamSing("fantastic", 4)
      (test1 must_== test2)
    }
  }

  "Nonprimitive singletons" should {
      "work with type parameters" in {
        val test1 = ComplexObject.listSing(Some(10))
        Thread.sleep(1000)
        val test2: List[Int] = ComplexObject.listSing(None)
        (test1 must_== test2) and (test2(1) must_== 10)
      }

    "work with a case class parameter" in {
      val bananaParam = Banana("elite", 8)
      val test1 = ComplexObject.caseClassParam(bananaParam)
      Thread.sleep(1000)
      val test2 = ComplexObject.caseClassParam(Banana("only", 1))
      (test1 must_== test2) and (test2 must contain(bananaParam.toString))
    }

    "work with a returned case class" in {
      val bananaParam = Banana("elite", 8)
      val test1 = ComplexObject.caseClassReturned("flipadelphia")
      Thread.sleep(1000)
      val test2 = ComplexObject.caseClassReturned("chardee macdennis")
      (test1 must_== test2) and (test2.test must_== "flipadelphia")
    }

    "take a function def" in {
      val test1 = ComplexObject.functionDef(100)
      val test2 = ComplexObject.functionDef(1)
      (test1 must_== test2) and (test2 must_== "200")
    }

    "take a curried function" in {
      val test1 = ComplexObject.curried("spca")
      val test2 = ComplexObject.curried("mississippi")
      (test1 must_== test2) and (test2 must_== 1004)
    }

    "take a function val" in {
      val test1 = ComplexObject.functionVal(List(1,2,3))
      val test2 = ComplexObject.functionVal(List(0,0,0))
      (test1 must_== test2) and (test2(1) must_== 3)
    }

    "take a val as a function" in {
      val test1 = ComplexObject.valAsFunction()
      val test2 = ComplexObject.valAsFunction()
      (test1 must_== test2)
    }
  }
}

object PrimitiveObject {
  val sing = Macros.singleton{ (test: String) => { val time = System.currentTimeMillis().toString; time } }
  lazy val longSing = Macros.singleton { (test: String) => test.length }
  val twoParamSing = Macros.singleton{ (test1: String, test2: Int) => System.currentTimeMillis().toString + " " + test1 + " " + test2 }
}

object ComplexObject {
  val listSing = Macros.singleton { (testParam: Option[Int]) => testParam.map(v => List(v + System.currentTimeMillis().toInt, v, v)).getOrElse(List(0,0,0))}

  val caseClassParam = Macros.singleton{ (ccParam: Banana) => { ccParam.toString + " " + System.currentTimeMillis().toString } }
  val caseClassReturned = Macros.singleton{ (test: String) => { Banana(test, System.currentTimeMillis().toInt) } }

  def newFunction(something: Int): String = (something * 2).toString
  val functionDef = Macros.singleton {newFunction _}

  def curryable(oneParam: Banana, twoParam: String) = oneParam.test2 + twoParam.length
  val curried = Macros.singleton(curryable(Banana("hola", 1000), _: String))

  val valFunction = (something: List[Int]) => System.currentTimeMillis().toInt :: something.map(_ * 3)
  val functionVal = Macros.singleton(valFunction)

//  //TODO: should translate to lazy val
//  val wrappedString = "hello"
//  val stringAsFunction = Macros.singleton(wrappedString)
//
//  //TODO: should translate to lazy val
//  val nonPrimitive = Banana("yo", 10)
//  val nonFunction = Macros.singleton(nonPrimitive)

  val fakeFunction = System.currentTimeMillis()
  val valAsFunction = Macros.singleton {fakeFunction _}
}

case class Banana(test: String, test2: Int)
