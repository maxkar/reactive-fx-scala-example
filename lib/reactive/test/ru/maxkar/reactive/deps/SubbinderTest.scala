package ru.maxkar.reactive.deps

import org.scalatest.FunSuite

/** Tests for the temporary binder. */
final class SubbinderTest extends FunSuite {
  import DLTestSyntax._


  /** Test binder. */
  val base = PermanentBinder


  /** Creates a new dependency list. */
  private def mk[T:Manifest](b : Binder, items : T*) : DependencyList[T] = {
    val res = new DependencyList[T]
    items.foreach(b.bind(res, _))
    res
  }



  /* TESTS. */
  test("Test basic creation") {
    val binder = base.sub._1
    val x = mk(binder, 1, 6, 7)
    fm(x, 1, 6, 7)

    binder.bind(x, 8)
    fm(x, 1, 6, 7, 8)
  }



  test("Test removal using binder's disposable") {
    val binder = base.sub._1
    val x = mk(binder, 1, 6, 7)
    val d = binder.bind(x, 5)
    binder.bind(x, 8)

    fm(x, 1, 6, 7, 5, 8)
    d.dispose()
    fm(x, 1, 6, 7, 8)
  }



  test("Test disposal of subbinders using subbinder disposable") {
    val binder = base.sub._1
    val x = mk(binder, 1, 2, 3)
    val (s1, d1) = binder.sub()

    s1.bind(x, 4)
    val i = s1.bind(x, 5)
    s1.bind(x, 6)

    binder.bind(x, 7)

    fm(x, 1, 2, 3, 4, 5, 6, 7)
    i.dispose()
    fm(x, 1, 2, 3, 4, 6, 7)
  }



  test("Test disposal of subbinders on temp binder") {
    val binder = base.sub._1
    val x = mk(binder, 1, 2, 3)
    val (s1, d1) = binder.sub()

    Seq(4, 5, 6).foreach(s1.bind(x, _))

    binder.bind(x, 7)
    fm(x, 1, 2, 3, 4, 5, 6, 7)
    d1.dispose()
    fm(x, 1, 2, 3, 7)
  }



  test("Test double disposal of subbinders on temp binder") {
    val binder = base.sub._1
    val x = mk(binder, 1, 2, 3)
    val (s1, d1) = binder.sub()

    Seq(4, 5, 6).foreach(s1.bind(x, _))

    binder.bind(x, 7)
    fm(x, 1, 2, 3, 4, 5, 6, 7)
    d1.dispose()
    fm(x, 1, 2, 3, 7)
    d1.dispose()
    fm(x, 1, 2, 3, 7)
  }



  test("Test sub-subbinders and dispose") {
    val x = mk(base, 1, 2)
    val (s1, d1) = base.sub()

    Seq(3, 4, 5).foreach(s1.bind(x, _))
    val (s2, d2) = s1.sub()

    Seq(6, 7, 8).foreach(s2.bind(x, _))

    fm(x, 1, 2, 3, 4, 5, 6, 7, 8)
    d1.dispose()
    fm(x, 1, 2)
    d2.dispose()
    fm(x, 1, 2)
  }
}

