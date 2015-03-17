package ru.maxkar.reactive.iter


private[iter] class IterableIter[T](items : Seq[DepIterable[T]]) {
  val iiter = items.iterator
  var curIter : DepIterator[T] = iiter.next()()

  def next() : Option[T] = {
    var res = curIter()
    while (res == None) {
      if (!iiter.hasNext)
        return None
      curIter = iiter.next()()
      res = curIter()
    }
    res
  }
}
