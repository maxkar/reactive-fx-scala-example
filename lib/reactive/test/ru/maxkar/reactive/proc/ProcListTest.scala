package ru.maxkar.reactive.proc

import org.scalatest.FunSuite

import ru.maxkar.reactive.deps.PermanentBinder
import ru.maxkar.reactive.proc.{Specification ⇒ Specs}

/**
 * Tests for the process list.
 */
final class ProcListTest extends FunSuite {
  /** Creates a new sample procedure. */
  private def mkProc() : Procedure =
    Procedure.compile(Specs.noop, PermanentBinder)



  /** Checks that process list equals to specified procedures. */
  private def checkList(list : ProcList, items : Procedure*) : Unit = {
    val itr = items.iterator

    while (itr.hasNext) {
      assert(list.empty === false)
      assert(itr.next === list.take)
    }


    assert(list.empty === true)
  }



  /** Puts item into the queue. */
  private def put(q : ProcList, items : Procedure*) : Unit =
    items.foreach(q += _)




  test("Test list addition and iteration") {
    val p1 = mkProc()
    val p2 = mkProc()
    val p3 = mkProc()

    val lst = new ProcList()
    checkList(lst)

    lst += p1
    checkList(lst, p1)
    checkList(lst)

    put(lst, p1, p2, p3)
    checkList(lst, p1, p2, p3)

    lst += p1
    checkList(lst, p1)

    put(lst, p2, p1, p3)
    checkList(lst, p2, p1, p3)
  }



  test("Tests interqueue movement") {
    val p1 = mkProc()
    val p2 = mkProc()
    val p3 = mkProc()
    val p4 = mkProc()


    val base = new ProcList()
    val sink = new ProcList()

    put(base, p1, p2, p3, p4)
    base.fillFrom(sink)
    checkList(base, p1, p2, p3, p4)
    checkList(sink)

    put(sink, p1, p2, p3, p4)
    base.fillFrom(sink)
    checkList(base, p1, p2, p3, p4)
    checkList(sink)


    put(base, p2)
    put(sink, p1, p3)
    base.fillFrom(sink)
    checkList(base, p2, p1, p3)
    checkList(sink)

    put(base, p2)
    put(sink, p1, p3)
    base.fillFrom(sink)
    put(base, p4)
    checkList(base, p2, p1, p3, p4)
    checkList(sink)
  }
}
