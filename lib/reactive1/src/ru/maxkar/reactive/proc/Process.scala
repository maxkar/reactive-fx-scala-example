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
