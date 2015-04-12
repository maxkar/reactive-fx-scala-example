package ru.maxkar.reactive.deps

import org.scalatest.FunSuite

/**
 * Tests for the dependency list class.
 */
final class DependencyListTest extends FunSuite {
  import scala.language.implicitConversions
  import DependencyListTest._
  import DLTestSyntax._


  /* SYNTAX/DSL */

  /** Creates a new dependency list. */
  private def mk[T:Manifest](items : T*) : DependencyList[T] =
    new DependencyList[T]() ++= (items : _*)



  /* TESTS */

  test("Test item addition") {
    val x = mk(1, 2, 3)
    fm(x, 1, 2, 3)
    x += 8
    fm(x, 1, 2, 3, 8)
  }



  test("Test basic removal") {
    val x = mk(1, 2, 3, 4)

    val i1 = x += 5
    x ++= (9, 6)

    val i2 = x += 7
    x ++= (10, 16)

    val i3 = x += 31
    x ++= (43, 51, 15)

    fm(x, 1, 2, 3, 4, 5, 9, 6, 7, 10, 16, 31, 43, 51, 15)

    i1.dispose()
    fm(x, 1, 2, 3, 4, 9, 6, 7, 10, 16, 31, 43, 51, 15)

    i3.dispose()
    fm(x, 1, 2, 3, 4, 9, 6, 7, 10, 16, 43, 51, 15)

    i2.dispose()
    fm(x, 1, 2, 3, 4, 9, 6, 10, 16, 43, 51, 15)
  }



  test("Test double dispose") {
    val x = mk(1, 2, 3)
    val i = x += 6
    x ++= (5, 8)

    fm(x, 1, 2, 3, 6, 5, 8)

    i.dispose()
    fm(x, 1, 2, 3, 5, 8)
    i.dispose()
    fm(x, 1, 2, 3, 5, 8)
  }



  test("Test removals of leading element") {
    val x = mk[Int]()
    val i1 = x += 1
    val i2 = x += 4
    x ++= (5, 6)

    fm(x, 1, 4, 5, 6)

    i1.dispose()
    fm(x, 4, 5, 6)
    i2.dispose()
    fm(x, 5, 6)
  }



  test("Test removal of tail element") {
    val x = mk[Int](6, 3, 7)
    val i1 = x += 1
    val i2 = x += 2

    fm(x, 6, 3, 7, 1, 2)

    i2.dispose()
    fm(x, 6, 3, 7, 1)
    i1.dispose()
    fm(x, 6, 3, 7)
  }



  test("Test removal of single element") {
    val x = mk[Int]()

    for (i ← Seq(1, 3, 6, 8, 5)) {
      val d = x += i
      fm(x, i)
      d.dispose()
      fm(x)
    }
  }



  test("Test iteration after removal of last returned element") {
    val x = mk(1, 2, 3)
    val d = x += 4
    x ++= (5, 6, 7)
    val itr = x.iterator

    pm(itr, 1, 2, 3, 4)
    d.dispose()
    fm(itr, 5, 6, 7)
  }



  test("Test iteration after removal of next element") {
    val x = mk(1, 2, 3)
    val d = x += 4
    x ++= (5, 6, 7)
    val itr = x.iterator

    pm(itr, 1, 2, 3)
    d.dispose()
    fm(itr, 4, 5, 6, 7)
  }




  test("Test iteration after removal of second-next element") {
    val x = mk(1, 2, 3)
    val d = x += 4
    x ++= (5, 6, 7)
    val itr = x.iterator

    pm(itr, 1, 2)
    d.dispose()
    fm(itr, 3, 5, 6, 7)
  }



  test("Test iteration after batch removal") {
    val x = mk(1, 2, 3)
    val ds = Seq(4, 5, 6, 7).map(x += _)
    x ++= (8, 9, 10)
    val itr = x.iterator

    pm(itr, 1, 2, 3, 4)
    ds.foreach(_.dispose())
    fm(itr, 5, 8, 9, 10)
  }
}



object DependencyListTest {
  /** List extensions. */
  private implicit class DLExtension[T](
        val v : DependencyList[T])
      extends AnyVal {
    def ++= (items : T*) : DependencyList[T] = {
      items.foreach(v += _)
      v
    }
  }
}
