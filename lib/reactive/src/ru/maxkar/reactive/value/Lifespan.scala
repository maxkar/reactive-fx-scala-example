package ru.maxkar.reactive.value

/**
 * Base lifecycle management trait. Lifespan groups several bindings
 * togever and alllows them to be disposed simultaneously. This is
 * higly convenient for libraries where application parts have different
 * life cycles. For example, dialog window have life time less than
 * core application.
 * <p>This trait do not provide a "disposal" API. It only allows
 * to register binding to that lifespan so action would be invoked
 * on the span end.
 */
trait Lifespan {
  /**
   * Registers a function to be invoked when this lifespan is disposed.
   * @param listener listener to invoke when this lifespan is disposed.
   */
  def onDispose(listener : () ⇒ Unit) : Unit
}


/**
 * Lifespan companion.
 */
object Lifespan {
  /**
   * Lifespan which lasts forever. This lifespan cannot  be destroyed.
   * It is useful for providing "global" models and bindings. This lifespan
   * is used by default.
   */
  val forever : Lifespan = new Lifespan {
    override def onDispose(listener : () ⇒ Unit) : Unit = ()
  }



  /**
   * Creates a new (nested) session. Session created is child of the
   * provided lifespan. When parent lifespan is destroyed, this session
   * is also destroyed.
   * @return new proxying session.
   */
  def mkSession()(implicit lifespan : Lifespan) : Session = {
    val res = new Session()
    lifespan.onDispose(res.destroy)
    res
  }
}

