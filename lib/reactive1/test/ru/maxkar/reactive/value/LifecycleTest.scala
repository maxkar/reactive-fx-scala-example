package ru.maxkar.reactive.value

import org.scalatest.FunSuite

import ru.maxkar.fun.syntax._
import ru.maxkar.reactive.value.syntax._


/**
 * Tests dedicated to a lifecycle management.
 */
final class LifecycleTests extends FunSuite {
  import scala.language.implicitConversions


  private var rootCtx = permanentBind


  /** Checks number of dependencies on the behaviour. */
  private def checkRefs(values : Behaviour[_]*) : Unit =
    values.foreach(value ⇒ {
      val proc = value.change.procedure
      val deps = ru.maxkar.reactive.proc.Introspector.activationDepCount(proc)
      assert(deps === 0)
    })



  test("On-behaviour map lives correctly") {
    val v = variable(3)
    def fn(x : Int) = x + 1

    val session = rootCtx.sub()
    implicit val ctx = session._1
    val vv = v.behaviour ≺ fn

    v.set(5)
    assert(6 === vv.value)

    session._2.dispose()
    v.set(8)
    assert(6 === vv.value)

    checkRefs(v)
  }



  test("Map function uplift lives correctly") {
    val v = variable(3)
    def fn(x : Int) = x + 1

    val session = rootCtx.sub()
    implicit val ctx = session._1
    val vv = fn _ ≻ v.behaviour

    v.set(5)
    assert(6 === vv.value)

    session._2.dispose()
    v.set(8)
    assert(6 === vv.value)

    checkRefs(v)
  }



  test("Applicative application lives correctly") {
    val v = variable(1)
    val w = variable(2)
    def fn(x : Int)(y : Int) = x + y

    val session = rootCtx.sub()
    implicit val ctx = session._1
    val vv = fn _ ≻ v.behaviour ≻ w.behaviour

    v.set(6)
    w.set(8)
    assert(14 === vv.value)

    session._2.dispose()
    checkRefs(v, w)
  }



  test("Applicative application to value lives correctly") {
    val v = variable(1)
    def fn(x : Int)(y : Int) = x + y

    val session = rootCtx.sub()
    implicit val ctx = session._1
    val vv = fn _ ≻ v.behaviour ≻ 4

    v.set(6)
    assert(10 === vv.value)

    session._2.dispose()
    checkRefs(v)
  }



  test("Monadic-like function uplift live correctly") {
    val v1 = variable(1)
    val v2 = variable(2)
    val v3 = variable(true)

    def fn(v : Boolean) = if (v) v1.behaviour else v2.behaviour

    val session = rootCtx.sub()
    implicit val ctx = session._1
    val vv = fn _ ≽ v3.behaviour

    v3.set(false)
    checkRefs(v1)

    v3.set(true)
    checkRefs(v2)

    v2.set(8)

    session._2.dispose()
    checkRefs(v1, v2, v3)

    v3.set(false)
    checkRefs(v1, v2, v3)
  }



  test("Monadic-like function application can be deconstructed on the fly") {
    val v1 = variable(1)
    val v2 = variable(2)
    val v3 = variable(6)

    def fn(v : Int) = if (v < 10) v1.behaviour else v2.behaviour

    val session = rootCtx.sub()
    implicit val ctx = session._1

    v3.behaviour ≺ (t ⇒ (if (t == 5) session._2.dispose()))
    val vv = fn _ ≽ v3.behaviour

    v3.set(11)
    checkRefs(v1)
    v3.set(5)
    checkRefs(v1, v2, v3)
  }



  test("Monadic-like function application in behaviour live correctly") {
    val v1 = variable(1)
    val v2 = variable(2)
    val v3 = variable(6)
    val v4 = variable(10)

    def fn(x : Int)(y : Int) = if (x < y) v1.behaviour else v2.behaviour

    val session = rootCtx.sub()
    implicit val ctx = session._1
    val vv = fn _ ≻ v3.behaviour ≽ v4.behaviour

    v3.set(20)
    checkRefs(v1)

    v3.set(5)
    checkRefs(v2)

    v4.set(3)

    session._2.dispose()
    checkRefs(v1, v2, v3, v4)

    v3.set(0)
    checkRefs(v1, v2, v3, v4)
  }



  test("Monadic-like function application in behaviour can be deconstructed on the fly") {
    val v1 = variable(1)
    val v2 = variable(2)
    val v3 = variable(6)
    var v4 = variable(10)

    def fn(x : Int)(y : Int) = if (x < y) v1.behaviour else v2.behaviour

    val session = rootCtx.sub()
    implicit val ctx = session._1

    v3.behaviour ≺ (t ⇒ (if (t == 5) session._2.dispose()))
    val vv = fn _ ≻ v3.behaviour ≽ v4.behaviour

    v3.set(20)
    checkRefs(v1)
    v4.set(30)
    checkRefs(v2)
    v4.set(6)
    checkRefs(v1)

    v3.set(5)
    checkRefs(v1, v2, v3, v4)
  }



  test("Monadic-like function application to value in behaviour lives correctly") {
    val v1 = variable(1)
    val v2 = variable(2)
    val v3 = variable(6)

    def fn(x : Int)(y : Int) = if (x < y) v1.behaviour else v2.behaviour

    val session = rootCtx.sub()
    implicit val ctx = session._1
    val vv = fn _ ≻ v3.behaviour ≽ 10

    v3.set(20)
    checkRefs(v1)

    v3.set(5)
    checkRefs(v2)

    session._2.dispose()
    checkRefs(v1, v2, v3)

    v3.set(100)
    checkRefs(v1, v2, v3)
  }



  test("Monadic-like function application to value in behaviour can be deconstructed on the fly") {
    val v1 = variable(1)
    val v2 = variable(2)
    val v3 = variable(6)

    def fn(x : Int)(y : Int) = if (x < y) v1.behaviour else v2.behaviour

    val session = rootCtx.sub()
    implicit val ctx = session._1

    v3.behaviour ≺ (t ⇒ (if (t == 5) session._2.dispose()))
    val vv = fn _ ≻ v3.behaviour ≽ 10

    v3.set(20)
    checkRefs(v1)
    v3.set(0)
    checkRefs(v2)
    v3.set(16)
    checkRefs(v1)

    v3.set(5)
    checkRefs(v1, v2, v3)
  }



  test("Join lives correctly") {
    val v1 = variable(2)
    val v2 = variable(4)
    val v3 = variable(v1.behaviour)

    val session = rootCtx.sub()
    implicit val ctx = session._1
    val vv = flatten(v3.behaviour)

    v3.set(v2.behaviour)
    checkRefs(v1)
    v3.set(v1.behaviour)
    checkRefs(v2)

    session._2.dispose()
    checkRefs(v1, v2, v3)
  }



  test("Join can be deconstructed on the fly") {
    val v1 = variable(2)
    val v2 = variable(4)
    val v3 = variable(v1.behaviour)

    val session = rootCtx.sub()
    implicit val ctx = session._1
    v3.behaviour ≺ (x ⇒ if (x `eq` v2.behaviour) session._2.dispose())
    val vv = flatten(v3.behaviour)

    v3.set(v2.behaviour)

    checkRefs(v1, v2, v3)
  }



  test("Value-based deconstruction work") {
    val v1 = variable(2)
    val v2 = variable(6)
    val v3 = variable(6)
    val v4 = variable(-1)

    val session = rootCtx.sub()
    implicit val ctx = session._1

    def fn(base : Int, scope : BindContext) : Behaviour[Int] = {
      implicit val ctx = scope
      if (base > 0)
        ((_ : Int) + (_ : Int)).curried ≻ v1.behaviour ≻ v2.behaviour
      else
        ((_  : Int) - (_ : Int)).curried ≻ v1.behaviour ≻ v3.behaviour
    }

    val res = fn _ ~≽ v4.behaviour

    assert(-4 === res.value)

    v3.set(2)
    assert(0 === res.value)

    v4.set(7)
    assert(8 === res.value)
    checkRefs(v3)


    v2.set(6)
    assert(8 === res.value)
    v1.set(10)
    assert(16 === res.value)

    v4.set(-6)
    assert(8 === res.value)
    checkRefs(v2)

    session._2.dispose()
    checkRefs(v1, v2, v3, v4)
  }

}

