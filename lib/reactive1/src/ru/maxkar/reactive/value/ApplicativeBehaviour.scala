package ru.maxkar.reactive.value

import ru.maxkar.reactive.deps.Binder
import ru.maxkar.reactive.proc.Procedure
import ru.maxkar.reactive.proc.spec.Specs


/** Implementation of the applicative behaviour. */
private[value] final class ApplicativeBehaviour[S, R](
      binder : Binder, fn : Behaviour[S ⇒ R], base : Behaviour[S])
    extends Behaviour[R] {

  /** Current value. */
  private var v = fn.value()(base.value())

  /** Change flag. */
  private var changed = false


  /** Update procedure. */
  private var proc =
    Procedure.compile(
      Specs.sequence(
        Specs.await(fn.change.procedure, base.change.procedure),
        Specs.exec { update() }),
      binder,
      () ⇒ changed = false)


  /** Updates current behaviour. */
  private def update() : Unit = {
    if (!fn.change.value() && !base.change.value())
      return

    val nv = fn.value()(base.value())
    if (nv != v) {
      changed = true
      v = nv
    }
  }



  /* IMPLEMENTATION */
  override def value() = v
  override val change = Signal(proc, changed)
}
