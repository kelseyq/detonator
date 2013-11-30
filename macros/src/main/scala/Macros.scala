import language.experimental.macros
import scala.reflect.macros.Context

object Macros {

  def singleton[R](param: Any) = macro singletonImpl[R]

  def singletonImpl[R](c: Context)(param: c.Expr[Any]) = {
    import c.universe._
    import nme.CONSTRUCTOR
    import tpnme.EMPTY

    val constructorBody = Block(List(Apply(Select(Super(This(EMPTY), EMPTY), CONSTRUCTOR), List())), Literal(Constant(())))
    val constructor = DefDef(Modifiers(), CONSTRUCTOR, List(), List(List()), TypeTree(), constructorBody)
    val objInheritance = List(Ident(newTypeName("AnyRef")))

    //TODO: verify return type, throw error if no match
    param.tree match {
      case Function(params, body) => {
        println("it's a function with params: " + params)

        val inst = ValDef(Modifiers(Flag.MUTABLE), newTermName("_inst"), TypeTree(appliedType(
          typeOf[Option[_]].typeConstructor, List(body.tpe))), reify {
          None
        }.tree)

        val declareParams = params.map { p =>
          ValDef(Modifiers(Flag.PARAM), p.name, p.tpt, EmptyTree)
        }

        val declareInst = DefDef(Modifiers(), newTermName("_declareInst"), List(),
          List(declareParams),
          TypeTree(),
          Block(List(
            ValDef(Modifiers(), newTermName("_res"), TypeTree(), c.resetAllAttrs(body)),
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

        val objectName = newTermName(c.fresh("TheObject"))

        val obj = ModuleDef(Modifiers(), objectName, Template(objInheritance, emptyValDef, List(constructor, inst, declareInst, applyDef)))

        val applyParams =  params.map { p =>
          ValDef(Modifiers(Flag.PARAM), c.fresh(p.name), p.tpt, EmptyTree)
        }

        val partiallyApplied = Function(
          applyParams,
          Apply(Select(Ident(objectName), newTermName("apply")), applyParams.map {
            p => Ident(p.name)
          }))


        val block = Block(List(obj), partiallyApplied)

        println(show(block))

        c.Expr[R](block)
      }
      case _ => println("no match")
        //TODO: no param functions turn into lazy val, function references?
        c.Expr[Any](Literal(Constant(())))
    }
  }
}
