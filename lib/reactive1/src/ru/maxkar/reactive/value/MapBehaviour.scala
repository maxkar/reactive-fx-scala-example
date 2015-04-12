package ru.maxkar.reactive.value

import ru.maxkar.reactive.deps.Binder
import ru.maxkar.reactive.proc.Procedure
import ru.maxkar.reactive.proc.{Specification ⇒ Specs}


/** Fmap function implementation. */
private[value] final class MapBehaviour[S, R](
      binder : Binder, fn : S ⇒ R, item : Behaviour[S])
    extends Behaviour[R] {

  /** Current value. */
  private var v = fn(item.value)

  /** Change flag. */
  private var changed = false


  /** Update procedure. */
  private val proc =
    Procedure.compile(
      Specs.seq(
        Specs.await(item.change.procedure),
        Specs.forUnit { update() }
      ),
      binder,
      () ⇒ changed = false)



  /** Updates current value. */
  private def update() : Unit = {
    if (!item.change.value)
      return
    val nv = fn(item.value)
    if (nv != v) {
      v = nv
      changed = true
    }
  }


  override def value() = v
  override val change = Signal(proc, changed)
}
