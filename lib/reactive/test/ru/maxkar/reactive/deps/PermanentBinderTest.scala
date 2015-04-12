package ru.maxkar.reactive.deps

import org.scalatest.FunSuite


/** Tests for the permanent binder. */
final class PermanentBinderTest extends FunSuite {
  import DLTestSyntax._


  /** Test binder. */
  val binder = PermanentBinder


  /** Creates a new dependency list. */
  private def mk[T:Manifest](items : T*) : DependencyList[T] = {
    val res = new DependencyList[T]
    items.foreach(binder.bind(res, _))
    res
  }



  /* TESTS. */
  test("Test basic creation") {
    val x = mk(1, 6, 7)
    fm(x, 1, 6, 7)

    binder.bind(x, 8)
    fm(x, 1, 6, 7, 8)
  }



  test("Test removal using binder's disposable") {
    val x = mk(1, 6, 7)
    val d = binder.bind(x, 5)
    binder.bind(x, 8)

    fm(x, 1, 6, 7, 5, 8)
    d.dispose()
    fm(x, 1, 6, 7, 8)
  }



  test("Test disposal of subbinders on the permanent binder") {
    val x = mk(1, 2, 3)
    val (s1, d1) = binder.sub()

    s1.bind(x, 4)
    val i = s1.bind(x, 5)
    s1.bind(x, 6)

    binder.bind(x, 7)

    fm(x, 1, 2, 3, 4, 5, 6, 7)
    i.dispose()
    fm(x, 1, 2, 3, 4, 6, 7)
  }



  test("Test double removal on subbinder of permanent binder") {
    val x = mk(1, 2, 3)
    val (s1, d1) = binder.sub()

    s1.bind(x, 4)
    val i = s1.bind(x, 5)
    s1.bind(x, 6)

    binder.bind(x, 7)

    fm(x, 1, 2, 3, 4, 5, 6, 7)
    i.dispose()
    fm(x, 1, 2, 3, 4, 6, 7)
    i.dispose()
    fm(x, 1, 2, 3, 4, 6, 7)
  }



  test("Test disposal of subbinders on permanent binder") {
    val x = mk(1, 2, 3)
    val (s1, d1) = binder.sub()

    Seq(4, 5, 6).foreach(s1.bind(x, _))

    binder.bind(x, 7)
    fm(x, 1, 2, 3, 4, 5, 6, 7)
    d1.dispose()
    fm(x, 1, 2, 3, 7)
  }



  test("Test double disposal of subbinders on permanent binder") {
    val x = mk(1, 2, 3)
    val (s1, d1) = binder.sub()

    Seq(4, 5, 6).foreach(s1.bind(x, _))

    binder.bind(x, 7)
    fm(x, 1, 2, 3, 4, 5, 6, 7)
    d1.dispose()
    fm(x, 1, 2, 3, 7)
    d1.dispose()
    fm(x, 1, 2, 3, 7)
  }



  test("Test late disposal of subbinder disposable") {
    val x = mk(1, 2, 3)
    val (s1, d1) = binder.sub()

    Seq(4, 5, 6).foreach(s1.bind(x, _))

    val d = s1.bind(x, 7)

    binder.bind(x, 8)
    fm(x, 1, 2, 3, 4, 5, 6, 7, 8)
    d1.dispose()
    fm(x, 1, 2, 3, 8)
    d.dispose()
    fm(x, 1, 2, 3, 8)
  }



  test("Test late disposal of subbinder after some disposables") {
    val x = mk(1, 2, 3)
    val (s1, d1) = binder.sub()

    Seq(4, 5, 6).foreach(s1.bind(x, _))

    val d = s1.bind(x, 7)

    binder.bind(x, 8)
    fm(x, 1, 2, 3, 4, 5, 6, 7, 8)
    d.dispose()
    fm(x, 1, 2, 3, 4, 5, 6, 8)
    d1.dispose()
    fm(x, 1, 2, 3, 8)
  }
}
