import language.experimental.macros
import scala.reflect.macros.Context

object Macros {

  def singleton[A, B](param: A => B) = macro singletonImpl[A, B]

  def singletonImpl[A: c.WeakTypeTag, B: c.WeakTypeTag](c: Context)(param: c.Expr[A => B]) = {
    import c.universe._
    import nme.CONSTRUCTOR
    import tpnme.EMPTY

    val constructorBody = Block(List(Apply(Select(Super(This(EMPTY), EMPTY), CONSTRUCTOR), List())), Literal(Constant(())))
    val constructor = DefDef(Modifiers(), CONSTRUCTOR, List(), List(List()), TypeTree(), constructorBody)
    val objInheritance = List(Ident(newTypeName("AnyRef")))

    param.tree match {
      case Function(params, body) => {
        println("it's a function with params: " + params)

        val inst = ValDef(Modifiers(Flag.MUTABLE), newTermName("_inst"), TypeTree(appliedType(
          typeOf[Option[_]].typeConstructor, List(body.tpe))), reify {
          None
        }.tree)

        val declareInst = DefDef(Modifiers(), newTermName("_declareInst"), List(),
          List(params),
          TypeTree(),
          Block(List(
            ValDef(Modifiers(), newTermName("_res"), TypeTree(), body),
            Assign(Ident(newTermName("_inst")),
              Apply(Select(Select(Ident("scala"), newTermName("Some")), newTermName("apply")),
                List(Ident(newTermName("_res"))))
            )), Ident(newTermName("_res"))))

        val applyDef = DefDef(Modifiers(), newTermName("apply"), List(),
          List(params),
          TypeTree(), Apply(Select(Ident(newTermName("_inst")), newTermName("getOrElse")),
            List(Apply(Ident(newTermName("_declareInst")), params.map {
              p => Ident(p.name)
            }))))

        val obj = ModuleDef(Modifiers(), newTermName("REGEXES"), Template(objInheritance, emptyValDef, List(constructor, inst, declareInst, applyDef)))

        val partiallyApplied = Function(params,
          Apply(Select(Ident(newTermName("REGEXES")), newTermName("apply")), params.map {
            p => Ident(p.name)
          }))


        val block = Block(List(obj), partiallyApplied)

        println(show(block))

        c.Expr[A => B](block)
      }
      case _ => println("no match")
        //TODO: apply accesses lazy val
        c.Expr[Any](Literal(Constant(())))
    }
  }
}
