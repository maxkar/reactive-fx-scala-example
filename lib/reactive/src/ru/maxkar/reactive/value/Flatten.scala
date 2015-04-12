package ru.maxkar.reactive.value

import ru.maxkar.reactive.deps.Binder
import ru.maxkar.reactive.proc.Procedure
import ru.maxkar.reactive.proc.{Specification ⇒ Specs}


/** Behaviour flattening value. */
private[value] final class Flatten[T](
      binder : Binder, base : Behaviour[Behaviour[T]])
    extends Behaviour[T] {

  /** Current value. */
  private var v = base.value().value()

  /** Change flag. */
  private var changed = false


  /** Update procedure. */
  private val proc =
    Procedure.compile(
      Specs.seq(
        Specs.await(base.change.procedure),
        Specs.dynamicBindTo(base.value().change.procedure),
        Specs.forUnit { update() }),
      binder,
      () ⇒ changed = false)


  /** Updates current value. */
  private def update() : Unit = {
    if (!base.change.value() && !base.value().change.value())
      return

    val nv = base.value().value()
    if (nv != v) {
      changed = true
      v = nv
    }
  }


  /* IMPLEMENTATION. */
  override def value() = v
  override val change = Signal(proc, changed)
}
