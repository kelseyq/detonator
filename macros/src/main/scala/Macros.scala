package com.kelseyinnis.detonator

import language.experimental.macros
import scala.reflect.macros.Context

object Macros {

  def singleton[T](param: T) = macro singletonImpl[T]

  def singletonImpl[T](c: Context)(param: c.Expr[T])(implicit tt: c.WeakTypeTag[T]) = {
    import c.universe._
    import nme.CONSTRUCTOR
    import tpnme.EMPTY

    val constructorBody = Block(List(Apply(Select(Super(This(EMPTY), EMPTY), CONSTRUCTOR), List())), Literal(Constant(())))
    val constructor = DefDef(Modifiers(), CONSTRUCTOR, List(), List(List()), TypeTree(), constructorBody)
    val objInheritance = List(Ident(newTypeName("AnyRef")))

    def convertFunctionToSingletonObject(func: Function, returnType: Option[Type] = None) = {

      val body = func.body
      val params = func.vparams

      val inst = ValDef(Modifiers(Flag.MUTABLE), newTermName("_inst"), TypeTree(appliedType(
        typeOf[Option[_]].typeConstructor, List(returnType getOrElse body.tpe))), reify {
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

     // println(show(block))

      c.Expr[T](block)
    }
//    println(param.tree)
//    println(param.actualType)
//    println("T: " + tt.tpe)
    //   println("U: " + ut.tpe)

    val funcs = (0 to 22).map { c.universe.definitions.FunctionClass(_) }.toList

    //TODO: verify return type, throw error if no match
    param.tree match {
      case f @ Function(body, params) =>
        convertFunctionToSingletonObject(f)
      case Block(stats, expr @ Function(_,_)) => 
        //todo: handle stats
        convertFunctionToSingletonObject(expr)
      case Block(stats, expr) =>
        c.abort(c.enclosingPosition, "No singleton function found, got block w/o ending function: " + stats + " " + expr)
      case other if (funcs.exists { tt.tpe.typeSymbol == _ }) =>
          val targs = tt.tpe match {
            case TypeRef(_, _, args) => args
            case _ => c.abort(c.enclosingPosition, "No function found, got value of type: " + other.tpe)}
          val params = targs.slice(0, targs.length - 1).map(arg => ValDef(Modifiers(Flag.PARAM), c.fresh(arg.toString.replaceAll("[^a-zA-Z0-9]", "")), TypeTree(arg), EmptyTree))
          val body = Apply(other, params.map {p => Ident(p.name)})
          convertFunctionToSingletonObject(Function(params, body), Option(targs.last))
      case nonFunction => c.abort(c.enclosingPosition, "No function found, got value of type: " + nonFunction.tpe)
    }
  }
}
