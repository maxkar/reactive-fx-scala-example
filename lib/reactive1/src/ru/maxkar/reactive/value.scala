package ru.maxkar.reactive


/**
 * Value manipulation package.
 */
package object value {
  /**
   * Permanent behaviour binding. Could not be disposed/detached.
   */
  val permanentBind = new Lifetime() {
    def onDispose(action : () ⇒ Unit) : Unit = ()
  }



  /**
   * Creates a new behaviour variable.
   * @param v initial value.
   */
  def variable[T](v : T) : Variable[T] = new Variable[T](v)
}
