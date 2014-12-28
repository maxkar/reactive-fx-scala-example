package ru.maxkar.reactive.value

import org.scalatest.FunSuite

import ru.maxkar.fun.syntax._
import ru.maxkar.reactive.value.syntax._
import ru.maxkar.reactive.wave.Wave

final class ValueTest extends FunSuite {
  implicit val ctx = permanentBind

  /** Creates a function which counts number of changes of peer behaviour. */
  private def count[T](v : Behaviour[T]) : () ⇒ Int = {
    /* -1 because initial setup will increment update count. */
    var updates = -1
    v ≺ (_ ⇒ updates += 1)
    () ⇒ updates
  }



  test("Test value of the variable") {
    val v1 = variable(44)
    assert(44 === v1.value)

    v1.set(55)
    assert(55 === v1.value)
  }




  test("Test batch update of the variables") {
    val v1 = variable("AOE")
    val v2 = variable("EOA")

    assert("AOE" === v1.value)
    assert("EOA" === v2.value)

    Wave.group(txn ⇒ {
      v1.wavedSet("35", txn)
      v2.wavedSet("TT", txn)
    })

    assert("35" === v1.value)
    assert("TT" === v2.value)
  }



  test("Test that no extra events are fired on vars") {
    val v = variable(3)
    val ups = count(v)

    assert(0 === ups())

    v.set(4)
    assert(1 === ups())

    v.set(4)
    assert(1 === ups())
  }



  test("Test one-arg function lifting") {
    def fn(x : Int) : Int = x + 5

    val v = variable(6)
    val vv = fn _ ≻ v.behaviour

    assert(11 === vv.value)

    v.set(77)
    assert(82 === vv.value)
  }



  test("One-arg lifting ignores duplicates on update.") {
    def fn(x : Int) : Int = if (x > 6) 77 else x - 3

    val v = variable(33)
    val vv = fn _ ≻ v.behaviour
    val ups = count(vv)

    assert(vv.value === 77)
    assert(0 === ups())

    v.set(55)
    assert(77 === vv.value)
    assert(0 === ups())

    v.set(5)
    assert(2 === vv.value)
    assert(1 === ups())
  }



  test("Test basic multi-arg application.") {
    def fn(x : Int)(y : Int) = 2 * x + y

    val v1 = variable(10)
    val v2 = variable(3)

    val v = fn _ ≻ v1.behaviour ≻ v2.behaviour
    assert(23 === v.value)

    v1.set(5)
    assert(13 === v.value)

    v2.set(0)
    assert(10 === v.value)
  }



  test("Test mutliarg update count.") {
    def fn(x : Int)(y : Int)(z : Int) = x * y + z

    val v1 = variable(9)
    val v2 = variable(0)
    val v3 = variable(5)

    val v = fn _ ≻ v1.behaviour ≻ v2.behaviour≻ v3.behaviour
    val ups = count(v)
    assert(5 === v.value)
    assert(0 === ups())

    v1.set(6)
    assert(5 === v.value)
    assert(0 === ups())


    Wave.group(w ⇒ {
      v1.wavedSet(3, w)
      v2.wavedSet(2, w)
      v3.wavedSet(-1, w)
    })
    assert(5 === v.value)
    assert(0 === ups())


    v3.set(0)
    assert(6 === v.value)
    assert(1 === ups())
  }



  test("Test join functionality.") {
    val v1 = variable("Abc")
    val v2 = variable("Def")
    val v3 = variable(v1.behaviour)
    val v = flatten(v3.behaviour)

    val ups = count(v)

    assert("Abc" === v.value)
    assert(0 === ups())

    v1.set("XyZ")
    assert("XyZ" === v.value)
    assert(1 === ups())

    v3.set(v2.behaviour)
    assert("Def" === v.value)
    assert(2 === ups())

    v1.set("XZXX")
    assert("Def" === v.value)
    assert(2 === ups())

    v2.set("Fed")
    assert("Fed" === v.value)
    assert(3 === ups())
  }



  test("Test monadic lift. ") {
    val v1 = variable("Abc")
    val v2 = variable("Def")
    val v3 = variable(true)

    def x(v : Boolean) = if (v) v1.behaviour else v2.behaviour

    val v = x _ ≽ v3.behaviour
    val ups = count(v)

    assert("Abc" === v.value)
    assert(0 === ups())

    v1.set("Cba")
    assert("Cba" === v.value)
    assert(1 === ups())

    v3.set(false)
    assert("Def" === v.value)
    assert(2 === ups())

    v1.set("XXX")
    assert("Def" === v.value)
    assert(2 === ups())

    v2.set("XZA")
    assert("XZA" === v.value)
    assert(3 === ups())

    v3.set(true)
    assert("XXX" === v.value)
    assert(4 === ups())
  }



  test("Test flipping order, see comment. ") {
    /*
     * Tests a "flipping" of the order. Propagations must run successfully
     * when there are some valid update order. This must be the case even
     * when new dependency graph is built during the update. This test checks
     * that update is running succesfully even partial order between to elements
     * changes on the opposite.
     *
     * NOTE!!!
     * This is the example why "engage" dependency cannot define an evaluation
     * order. Attempt to do so will force you to remove dependecy "on the fly"
     * for the "dependent" node.
     */
    object FlipTest {
      val a = variable(false)
      val c = f _ ≽ a.behaviour
      val b = g _ ≽ a.behaviour

      def f(v : Boolean) : Behaviour[Boolean] = if (v) b else a
      def g(v : Boolean) : Behaviour[Boolean] = if (v) a else c
    }

    def m(v1 : Boolean)(v2 : Boolean) =
      (if (v1) 2 else 0) + (if (v2) 1 else 0)

    val r = m _ ≻ FlipTest.b ≻ FlipTest.c
    val ups = count(r)

    assert(0 === r.value)
    assert(0 === ups())

    FlipTest.a.set(true)
    assert(3 === r.value)
    assert(1 === ups())

    FlipTest.a.set(false)
    assert(0 === r.value)
    assert(2 === ups())

    FlipTest.a.set(true)
    assert(3 === r.value)
    assert(3 === ups())
  }



  test("Value-based dispatch work") {
    val v1 = variable(2)
    val v2 = variable(6)
    val v3 = variable(-1)
    var ib = 0

    def fn(base : Int, scope : BindContext) : Behaviour[Int] = {
      implicit val ctx = scope
      v1.behaviour ≺ (_ ⇒ ib += 1)
      if (base > 0)
        ((_ : Int) + (_ : Int)).curried ≻ v1.behaviour ≻ v2.behaviour
      else
        ((_  : Int) - (_ : Int)).curried ≻ v1.behaviour ≻ v2.behaviour
    }

    val res = fn _ ~≽ v3.behaviour
    val ups = count(res)

    assert(-4 === res.value)
    assert(0 === ups())
    /* One update during construction. */
    assert(1 === ib)

    v2.set(2)
    assert(0 === res.value)
    assert(1 === ups())
    assert(1 === ib)

    v3.set(7)
    assert(4 === res.value)
    assert(2 === ups())
    /* One update during re-construction. */
    assert(2 === ib)


    v2.set(6)
    assert(8 === res.value)
    assert(3 === ups())
    assert(2 === ib)

    v1.set(10)
    assert(16 === res.value)
    assert(4 === ups())
    /* Only one update in active scope, previous scope shold be destroyed.
     * Monadic application gives 4 here!
     */
    assert(3 === ib)
  }
}

