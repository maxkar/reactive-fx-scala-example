package ru.maxkar.reactive.value

import org.scalatest.FunSuite

import ru.maxkar.fun.syntax._
import ru.maxkar.reactive.value.syntax._
import ru.maxkar.reactive.value.event.Event
import ru.maxkar.reactive.value.Lifespan._
import ru.maxkar.reactive.wave.Participant
import ru.maxkar.reactive.wave.Participable

/**
 * Tests dedicated to a lifecycle management.
 */
final class LifecycleTests extends FunSuite {
  import scala.language.implicitConversions

  implicit val lspan = Lifespan.forever


  /** Ref-counting item. */
  private final class RefCount[+T](peer : Behaviour[T]) {
    private val corrs = new scala.collection.mutable.ArrayBuffer[Participant]

    /** "No listeners" assert. */
    def checkEmpty() : Unit = assert(corrs.isEmpty, "Correlation set is not empty")


    val b : Behaviour[T] = new Behaviour[T] {
      override def value() : T = peer.value
      override val change = new Event[Boolean] {
        override def addCorrelatedNode(node : Participant) = {
          peer.change.addCorrelatedNode(node)
          corrs += node
        }

        override def removeCorrelatedNode(node : Participant) = {
          peer.change.removeCorrelatedNode(node)
          corrs -= node
        }

        override def defer(target : Participant) =
          peer.change.defer(target)

        override def value() = peer.change.value()
      }
    }
  }



  def checkRefs(refs : RefCount[Any]*) : Unit =
    refs.foreach(ref ⇒ ref.checkEmpty())



  test("On-behaviour map lives correctly") {
    val v = variable(3)
    val rv = new RefCount(v)
    def fn(x : Int) = x + 1

    val session = mkSession()
    implicit val ctx = new BindContext(session, Participable.DefaultParticipable)
    val vv = rv.b ≺ fn

    v.set(5)
    assert(6 === vv.value)

    session.destroy()
    v.set(8)
    assert(6 === vv.value)

    checkRefs(rv)
  }



  test("Map function uplift lives correctly") {
    val v = variable(3)
    val rv = new RefCount(v)
    def fn(x : Int) = x + 1

    val session = mkSession()
    implicit val ctx = new BindContext(session, Participable.DefaultParticipable)
    val vv = rv.b ≺ fn

    v.set(5)
    assert(6 === vv.value)

    session.destroy()
    v.set(8)
    assert(6 === vv.value)

    checkRefs(rv)
  }



  test("Applicative application lives correctly") {
    val v = variable(1)
    val rv = new RefCount(v)
    val w = variable(2)
    var rw = new RefCount(w)
    def fn(x : Int)(y : Int) = x + y

    val session = mkSession()
    implicit val ctx = new BindContext(session, Participable.DefaultParticipable)
    val vv = fn _ ≻ rv.b ≻ rw.b

    v.set(6)
    w.set(8)
    assert(14 === vv.value)

    session.destroy()
    checkRefs(rv, rw)
  }



  test("Applicative application to value lives correctly") {
    val v = variable(1)
    val rv = new RefCount(v)
    def fn(x : Int)(y : Int) = x + y

    val session = mkSession()
    implicit val ctx = new BindContext(session, Participable.DefaultParticipable)
    val vv = fn _ ≻ rv.b ≻ 4

    v.set(6)
    assert(10 === vv.value)

    session.destroy()
    checkRefs(rv)
  }



  test("Monadic-like function uplift live correctly") {
    val v1 = variable(1)
    val v2 = variable(2)
    val v3 = variable(true)
    val rv1 = new RefCount(v1)
    val rv2 = new RefCount(v2)
    val rv3 = new RefCount(v3)

    def fn(v : Boolean) = if (v) rv1.b else rv2.b

    val session = mkSession()
    implicit val ctx = new BindContext(session, Participable.DefaultParticipable)
    val vv = fn _ ≽ rv3.b

    v3.set(false)
    checkRefs(rv1)

    v3.set(true)
    checkRefs(rv2)

    v2.set(8)

    session.destroy()
    checkRefs(rv1, rv2, rv3)

    v3.set(false)
    checkRefs(rv1, rv2, rv3)
  }



  test("Monadic-like function application can be deconstructed on the fly") {
    val v1 = variable(1)
    val v2 = variable(2)
    val v3 = variable(6)
    val rv1 = new RefCount(v1)
    val rv2 = new RefCount(v2)
    val rv3 = new RefCount(v3)

    def fn(v : Int) = if (v < 10) rv1.b else rv2.b

    val session = mkSession()
    implicit val ctx = new BindContext(session, Participable.DefaultParticipable)

    rv3.b ≺ (t ⇒ (if (t == 5) session.destroy()))
    val vv = fn _ ≽ rv3.b

    v3.set(11)
    checkRefs(rv1)
    v3.set(5)
    checkRefs(rv1, rv2, rv3)
  }



  test("Monadic-like function application in behaviour live correctly") {
    val v1 = variable(1)
    val v2 = variable(2)
    val v3 = variable(6)
    val v4 = variable(10)
    val rv1 = new RefCount(v1)
    val rv2 = new RefCount(v2)
    val rv3 = new RefCount(v3)
    var rv4 = new RefCount(v4)

    def fn(x : Int)(y : Int) = if (x < y) rv1.b else rv2.b

    val session = mkSession()
    implicit val ctx = new BindContext(session, Participable.DefaultParticipable)
    val vv = fn _ ≻ rv3.b ≽ rv4.b

    v3.set(20)
    checkRefs(rv1)

    v3.set(5)
    checkRefs(rv2)

    v4.set(3)

    session.destroy()
    checkRefs(rv1, rv2, rv3, rv4)

    v3.set(0)
    checkRefs(rv1, rv2, rv3, rv4)
  }



  test("Monadic-like function application in behaviour can be deconstructed on the fly") {
    val v1 = variable(1)
    val v2 = variable(2)
    val v3 = variable(6)
    var v4 = variable(10)
    val rv1 = new RefCount(v1)
    val rv2 = new RefCount(v2)
    val rv3 = new RefCount(v3)
    val rv4 = new RefCount(v4)

    def fn(x : Int)(y : Int) = if (x < y) rv1.b else rv2.b

    val session = mkSession()
    implicit val ctx = new BindContext(session, Participable.DefaultParticipable)

    v3.behaviour ≺ (t ⇒ (if (t == 5) session.destroy()))
    val vv = fn _ ≻ rv3.b ≽ rv4.b

    v3.set(20)
    checkRefs(rv1)
    v4.set(30)
    checkRefs(rv2)
    v4.set(6)
    checkRefs(rv1)

    v3.set(5)
    checkRefs(rv1, rv2, rv3, rv4)
  }



  test("Monadic-like function application to value in behaviour lives correctly") {
    val v1 = variable(1)
    val v2 = variable(2)
    val v3 = variable(6)
    val rv1 = new RefCount(v1)
    val rv2 = new RefCount(v2)
    val rv3 = new RefCount(v3)

    def fn(x : Int)(y : Int) = if (x < y) rv1.b else rv2.b

    val session = mkSession()
    implicit val ctx = new BindContext(session, Participable.DefaultParticipable)
    val vv = fn _ ≻ rv3.b ≽ 10

    v3.set(20)
    checkRefs(rv1)

    v3.set(5)
    checkRefs(rv2)

    session.destroy()
    checkRefs(rv1, rv2, rv3)

    v3.set(100)
    checkRefs(rv1, rv2, rv3)
  }



  test("Monadic-like function application to value in behaviour can be deconstructed on the fly") {
    val v1 = variable(1)
    val v2 = variable(2)
    val v3 = variable(6)
    val rv1 = new RefCount(v1)
    val rv2 = new RefCount(v2)
    val rv3 = new RefCount(v3)

    def fn(x : Int)(y : Int) = if (x < y) rv1.b else rv2.b

    val session = mkSession()
    implicit val ctx = new BindContext(session, Participable.DefaultParticipable)

    v3.behaviour ≺ (t ⇒ (if (t == 5) session.destroy()))
    val vv = fn _ ≻ rv3.b ≽ 10

    v3.set(20)
    checkRefs(rv1)
    v3.set(0)
    checkRefs(rv2)
    v3.set(16)
    checkRefs(rv1)

    v3.set(5)
    checkRefs(rv1, rv2, rv3)
  }



  test("Join lives correctly") {
    val v1 = variable(2)
    val v2 = variable(4)
    val rv1 = new RefCount(v1)
    val rv2 = new RefCount(v2)
    val v3 = variable(rv1.b)
    val rv3 = new RefCount(v3)

    val session = mkSession()
    implicit val ctx = new BindContext(session, Participable.DefaultParticipable)
    val vv = flatten(rv3.b)

    v3.set(rv2.b)
    checkRefs(rv1)
    v3.set(rv1.b)
    checkRefs(rv2)

    session.destroy()
    checkRefs(rv1, rv2, rv3)
  }



  test("Join can be deconstructed on the fly") {
    val v1 = variable(2)
    val v2 = variable(4)
    val rv1 = new RefCount(v1)
    val rv2 = new RefCount(v2)
    val v3 = variable(rv1.b)
    val rv3 = new RefCount(v3)

    val session = mkSession()
    implicit val ctx = new BindContext(session, Participable.DefaultParticipable)
    rv3.b ≺ (x ⇒ if (x `eq` rv2.b) session.destroy())
    val vv = flatten(rv3.b)

    v3.set(rv2.b)

    checkRefs(rv1, rv2, rv3)
  }



  test("Value-based deconstruction work") {
    val v1 = variable(2)
    val v2 = variable(6)
    val v3 = variable(6)
    val v4 = variable(-1)
    val rv1 = new RefCount(v1)
    val rv2 = new RefCount(v2)
    val rv3 = new RefCount(v3)
    val rv4 = new RefCount(v4)

    val session = mkSession()
    implicit val ctx = new BindContext(session, Participable.DefaultParticipable)

    def fn(base : Int, scope : BindContext) : Behaviour[Int] = {
      implicit val ctx = scope
      if (base > 0)
        ((_ : Int) + (_ : Int)).curried ≻ rv1.b ≻ rv2.b
      else
        ((_  : Int) - (_ : Int)).curried ≻ rv1.b ≻ rv3.b
    }

    val res = fn _ ~≽ rv4.b

    assert(-4 === res.value)

    v3.set(2)
    assert(0 === res.value)

    v4.set(7)
    assert(8 === res.value)
    checkRefs(rv3)


    v2.set(6)
    assert(8 === res.value)
    v1.set(10)
    assert(16 === res.value)

    v4.set(-6)
    assert(8 === res.value)
    checkRefs(rv2)

    session.destroy()
    checkRefs(rv1, rv2, rv3, rv4)
  }

}
