package ru.maxkar.reactive.proc.spec

import org.scalatest.FunSuite

import ru.maxkar.reactive.proc._
import ru.maxkar.reactive.deps.PermanentBinder

/**
 * Dependency management tests for different specs.
 */
final class SpecDepManagementTest extends FunSuite {
  import scala.language.implicitConversions


  /** Simple await compiler. */
  private implicit def proc2spec(proc : Procedure) : Specification =
    Specs.await(proc)



  /** Ensures procedure dependencies.  */
  private def checkDeps(base : Procedure, deps : Procedure*) : Unit =
    deps.foreach(p ⇒ {
      val di = Introspector.iterateActivationDeps(p)
      assert(di.hasNext === true)
      assert(di.next === base)
      assert(di.hasNext === false)
    })


  /** Ensures that there are no deps. */
  private def checkNoDeps(proc : Procedure) : Unit =
    assert(Introspector.activationDepCount(proc) === 0)



  /** Shortcut for the generator. */
  private def gen() : (Activator, Procedure) =
    Procedure.generator(() ⇒ Unit)


  /** Compiles a specification. */
  private def comp(spec : Specification) : Procedure =
    Procedure.compile(spec, PermanentBinder)



  test("Test sequence generators 1") {
    val p = gen()._2

    val s1 = comp(Specs.sequence(p))
    checkDeps(s1, p)
  }



  test("Test sequence generator of multiple items") {
    val p1 = gen()._2
    val (a1, p2) = gen()
    val p3 = gen()._2

    val s = comp(Specs.sequence(p1, p2, p3))
    checkDeps(s, p1, p2, p3)
    a1.activate()
    checkDeps(s, p1, p2, p3)
  }



  test("Test dynamic await specification") {
    val (a, p) = gen()

    val p1 = gen()._2
    val p2 = gen()._2

    var dp = p1

    val s = comp(Specs.sequence(Specs.awaitDynamic(() ⇒ dp), p))
    checkDeps(s, p1, p)
    checkNoDeps(p2)

    /** No update on proc invokation. */
    a.activate()
    checkDeps(s, p1, p)
    checkNoDeps(p2)

    dp = p2
    a.activate()
    checkDeps(s, p2, p)
    checkNoDeps(p1)

    dp = p1
    a.activate()
    checkDeps(s, p1, p)
    checkNoDeps(p2)
  }
}
