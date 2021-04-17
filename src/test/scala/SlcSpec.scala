import org.junit.Test
import org.junit.Assert._
import slc.Syntax._
import slc.Dsl._

class SlcSpec:
  @Test def test(): Unit =
    // there is a *syntactic* distinction
    // between standard vars and strict vars
    val x = StandardVar("x")
    val y = StandardVar("y")
    val z = StandardVar("z")

    val u = StrictVar("u")
    val v = StrictVar("v")
    val w = StrictVar("w")

    // there's one base type, B
    // in the paper: type `1`
    Bty
    // there's one ground term, `b` of type `B`
    // in the paper: term `*` of type `1`
    b

    object DslCheatSheet:
      // function abstraction
      assert(x ~> y == Lam(x, b))
      // function application
      assert(x @@ y == Apply(x, y))
      // Just as in the paper, there are 3 kinds of `let`
      val T_s: SynthTerm = b
      val T_a: AnalysisTerm = b
      val tau = Bty
      val S: Expr = b
      assert(
        // paper: `x = T_s; S`
        let(x, T_s)(S) == LetStd(x, T_s, S)
      )
      assert(
        // paper: `x : tau = T_a; S`
        let(x :: tau, T_a)(S) == LetAnno(x, tau, T_a, S)
      )
      assert(
        // paper: `u = T_a; S`
        let(u, T_a)(S) == LetStrict(u, T_a, S)
      )
    end DslCheatSheet

    // example from paper
    let(u, x ~> b)(u)
      . checkVerbose(Bty ~> Bty)

    // lets with type annotations
    let(x :: Bty ~> Bty, z ~> z) {
      let(y :: Bty, b)(x @@ y)
    }
      . check(Bty)

    // All and only valid SLC syntax is representable,
    // so scalac won't type-check the following
    // let(x, let(u, x ~> b)(u))(X)

    let(x, x)(x).checkThrows(Bty)







