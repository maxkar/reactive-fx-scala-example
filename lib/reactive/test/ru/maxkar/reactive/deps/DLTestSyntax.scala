package ru.maxkar.reactive.deps


/** Extensions for Dependency list tests. */
private[deps] final object DLTestSyntax extends org.scalatest.Assertions {
  /** Partial match. */
  def pm[T](iter : Iterator[T], items : T*) : Unit = {
    val i = items.iterator
    while (i.hasNext) {
      assert(iter.hasNext === true)
      assert(iter.next === i.next)
    }
  }


  /** Full iterator match. */
  def fm[T](iter : Iterator[T], items : T*) : Unit = {
    pm(iter, items : _*)
    assert(iter.hasNext === false)
  }


  /** Full sequence match. */
  def fm[T](list : DependencyList[T], items : T*) : Unit =
    fm(list.iterator, items : _*)
}
