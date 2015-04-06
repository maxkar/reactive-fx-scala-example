package ru.maxkar.reactive.value

import ru.maxkar.reactive.deps.Binder
import ru.maxkar.reactive.proc.Procedure
import ru.maxkar.reactive.proc.spec.Specs


/** Implementation of submodel dispatch.  */
private[value] final class SubmodelDispatch[S, R](
      binder : Binder, fn : (S, BindContext) ⇒ Behaviour[R], base : Behaviour[S])
    extends Behaviour[R] {

  /** Sub-binder and its destructor. */
  private var (subBinder, subDtor) = binder.sub()

  /** Dependent model. */
  private var basem = fn(base.value(), new BindContext(subBinder))

  /** Current value. */
  private var v = basem.value()

  /** Change flag. */
  private var changed = false



  /** Update procedure. */
  private val proc =
    Procedure.compile(
      Specs.sequence(
        Specs.await(base.change.procedure),
        Specs.exec { reevalAfterBase },
        Specs.awaitDynamic(() ⇒ basem.change.procedure),
        Specs.exec { updateValue }
      ),
      binder,
      () ⇒ changed = false)



  /** Updates after base is ready. */
  private def reevalAfterBase() : Unit = {
    if (!base.change.value())
      return

    subDtor.dispose()
    val (nsb, nsd) = binder.sub()
    subBinder = nsb
    subDtor = nsd
    basem = fn(base.value(), new BindContext(subBinder))
  }



  /** Updates current value. */
  private def updateValue() : Unit = {
    if (!base.change.value() && !basem.change.value())
      return

    val nv = basem.value()
    if (nv != v) {
      v = nv
      changed = true
    }
  }



  /* IMPLEMENTATION */
  override def value() = v
  override val change = Signal(proc, changed)
}
