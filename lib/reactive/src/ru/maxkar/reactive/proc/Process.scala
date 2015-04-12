package ru.maxkar.reactive.proc


/**
 * Process combinators and utilities.
 */
final object Process {
  /** Process without operations. */
  val noop : Process = () ⇒ null



  /** Awaits all items in the iterator. */
  def await(items : Iterator[Procedure]) : Process =
    () ⇒
      if (items.hasNext)
        items.next
      else
        null



  /** Awaits all items in the iterable. */
  def await(items : Iterable[Procedure]) : Process =
    await(items.iterator)




  /** Creates a new process from action iterator. */
  def fromActions(actions : Iterator[Action]) : Process =
    if (!actions.hasNext)
      noop
    else {
      var cur : Process = actions.next()()

      def next() : Procedure = {
        var res = cur()
        while (res == null) {
          if (!actions.hasNext)
            return null

          cur = actions.next()()
          res = cur()
        }

        res
      }

      next
    }



  /** Creates a new process from iterable actions. */
  def fromActions(actions : Iterable[Action]) : Process =
    fromActions(actions.iterator)
}
