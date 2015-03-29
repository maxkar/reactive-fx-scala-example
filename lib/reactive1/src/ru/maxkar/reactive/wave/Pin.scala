package ru.maxkar.reactive.wave

import scala.collection.mutable.ArrayBuffer
import ru.maxkar.reactive.iter._


/**
 * Wave propagation pin. This could be treated as "output" connectors
 * for each node.
 * @param T type of the pin value.
 * @param default default pin value (pin outside wave).
 * @param deps pin dependency iterator.
 * @param action action to execute when this node is ready.
 */
final class Pin[T] private[wave](
      default : T,
      deps : DepIterable[Pin[_]],
      action : () ⇒ T) {


  /**
   * Processing state of the node.
   */
  private var state : State[T] = Ready



  /**
   * Nodes which should be included in the wave when this pin
   * is included in the same wave.
   */
  private val engageDeps = new ArrayBuffer[Pin[_]]()



  /**
   * Registers a node as dependent on this one.
   */
  def addDependency(dep : Key) : Unit = engageDeps += dep.pin



  /**
   * Unregisters dependency from this pin.
   */
  def removeDependency(dep : Key) : Unit = engageDeps -= dep.pin



  /**
   * Returns pin value (signal) in current wave. If there is no wave or
   * pin is not involved in the wave, default value is returned.
   * @throws IllegalStateException if pin is involved in the wave but is not
   *   resolved yet.
   */
  def value() : T =
    state match {
      case Ready ⇒ default
      case Resolved(x) ⇒ x
      case _ ⇒ throw new IllegalStateException("Could not get value in " + state + " state")
    }



  /**
   * Activates this node and prerares it to act in the wave.
   * Returns <code>true</code> if this pin was activated by the call,
   * returns <code>false</code> if this pin was activated by another (previous) call.
   */
  private[wave] def activate() : Boolean =
    state match {
      case Ready ⇒
        state = Boot(new ArrayBuffer[Pin[_]])
        true
      case Boot(_) ⇒ false
      case _ ⇒ throw new IllegalStateException("Bad state before activation " + state)
    }



  /**
   * Boots item and attempts to resolve this if possible.
   * Returns <code>true</code> iff there are no dependencies and
   * node is ready to be "resolved". Returns <code>false</code>
   * otherwise and this means that node is waiting for some dependencies.
   */
  private[wave] def bootAndUpdate() : Boolean =
    state match {
      case Boot(x) ⇒
        val itr = deps()
        if (rollDeps(x, itr)) {
          state = Resolving(x)
          true
        } else {
          state = Waiting(x, itr)
          false
        }
      case _ ⇒ throw new IllegalStateException("Bad boot state " + state)
    }



  /**
   * Rolls to a next possible dependency and tries to update
   * this node. Returns <code>true</code> iff this node is ready
   * to be resolved. Returns <code>false</code> iff there are other
   * unsatisfied dependencies.
   */
  private [wave] def rollAndUpdate() : Boolean =
    state match {
      case Waiting(x, i) ⇒
        if (rollDeps(x, i)) {
          state = Resolving(x)
          true
        } else
          false
      case _ ⇒ throw new IllegalStateException("Bad roll/update state " + state)
    }




  /**
   * Resolves a node and processes node-related actions.
   * Returns list of runtime dependencies for this node
   * (i.e. list of nodes which could proceed its evaluation).
   */
  private[wave] def resolve() : Iterable[Pin[_]] =
    state match {
      case Resolving(x) ⇒
        state = Resolved(action())
        x
      case _ ⇒ throw new IllegalStateException("Bad resolve state " + state)
    }



  /**
   * Cleans this node after wave passed.
   */
  private[wave] def cleanup() : Unit =
    state match {
      case Resolved(_) ⇒ state = Ready
      case _ ⇒ throw new IllegalStateException("Bad cleanup state " + state)
    }



  /**
   * Runs a dependency processing.
   * Returns <code>true</code> if all deps are processed or
   * <code>false</code> if there is a missing dependency.
   * Sets current state to either Waiting or Resolving depending
   * on the result.
   * @param rtDeps dependency collector.
   * @param iter dependency iterator.
   * @return <code>true</code> iff all dependencies are met and
   * node is ready to update.
   */
  private def rollDeps(
        rtDeps : ArrayBuffer[Pin[_]],
        iter : DepIterator[Pin[_]])
      : Boolean = {
    var next = iter()
    while (next != None && !next.get.addRtDep(this))
      next = iter()

    next == None
  }



  /**
   * Adds a runtime (calculation) dependency for this node.
   * Returns <code>true</code> iff dependency was registered or
   * <code>false</code> otherwise. Dependency could be skipped if
   * it does not affect evaluation correctness. It is safe in
   * the following cases:
   * <ul>
   *   <li>this node is not engaged in the wave.
   *   <li>this node is already resolved.
   *   <li>this node is in "Resolving" state. It does not
   *     affect other nodes because they will go into "resolve"
   *     queue after this one.
   * </ul>
   */
  private def addRtDep(rtDep : Pin[_]) : Boolean =
    state match {
      case Ready | Resolving(_) | Resolved(_) ⇒ false
      case Boot(x) ⇒
        x += rtDep
        true
      case Waiting(x, _) ⇒
        x += rtDep
        true
    }




  /**
   * Returns dependend nodes (nodes which should be included in the wave
   * after this node is involved in the same wave.
   */
  private[wave] def getDeps() : Iterable[Pin[_]] = engageDeps
}
