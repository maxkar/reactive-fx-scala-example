package ru.maxkar.reactive.proc


/**
 * Procedure execution process. Something very similar to coroutine
 * or coprocedure. Proceeds sequentially until it have to wait until
 * next procedure is complete.
 */
trait Process {
  /**
   * Proceeds until this process have to wait until active process for
   * some procedure completes. Returns that procedure to await. Next call
   * to this method will be made only after given procedure is complete
   * for this tick (i.e. it could not be run or completed).
   * @return next procedure to await for or <code>null</code> when
   * this process/action is complete.
   */
  def proceedTillNextProcedure() : Procedure
}



/**
 * Process combinators and utilities.
 */
final object Process {
  /** Process without operations. */
  val noop : Process = new Process() {
    override def proceedTillNextProcedure() : Procedure = null
  }



  /** Awaits all items in the iterator. */
  def await(items : Iterator[Procedure]) : Process =
    new Process() {
      override def proceedTillNextProcedure() : Procedure =
        if (items.hasNext)
          items.next
        else
          null
    }



  /** Awaits all items in the iterable. */
  def await(items : Iterable[Procedure]) : Process =
    await(items.iterator)




  /** Creates a new process from action iterator. */
  def fromActions(actions : Iterator[Action]) : Process =
    if (!actions.hasNext)
      noop
    else
      new Process() {
        private var cur = actions.next().start()

        override def proceedTillNextProcedure() : Procedure = {
          var res = cur.proceedTillNextProcedure()
          while (res == null) {
            if (!actions.hasNext)
              return null

            cur = actions.next().start()
            res = cur.proceedTillNextProcedure()
          }

          res
        }
      }



  /** Creates a new process from iterable actions. */
  def fromActions(actions : Iterable[Action]) : Process =
    fromActions(actions.iterator)
}
