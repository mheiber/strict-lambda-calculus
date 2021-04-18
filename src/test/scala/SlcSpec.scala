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
    val x1 = StandardVar("x1")
    val x2 = StandardVar("x2")
    val x3 = StandardVar("x3")
    val x4 = StandardVar("x4")
    val x5 = StandardVar("x5")
    val x6 = StandardVar("x6")
    val x7 = StandardVar("x7")


    val u = StrictVar("u")
    val v = StrictVar("v")
    val w = StrictVar("w")

    // there's one base type, `T`
    // in the paper: type `1`
    assert(T == T)
    // there's one ground term, of type `T`
    // in the paper this is also `*`
    val * : SynthTerm = Base

    object DslCheatSheet:
      // function abstraction. The variable must be a StandardVar
      assert(x ->: y == Lam(x, *))
      assert(x ->: y ->: z == Lam(x, Lam(y, z)))
      // function application. Restricted to `standardVar @@ var`
      assert(x @@ y == Apply(x, y))
      // Just as in the paper, there are 3 kinds of `let`
      val T_s: SynthTerm = *
      val T_a: AnalysisTerm = *
      val tau = T
      val S: Expr = *
      assert(
        // paper: `x = T_s; S`
        let(x, T_s){S} == LetStd(x, T_s, S)
      )
      assert(
        // paper: `x : tau = T_a; S`
        let(x :: tau, T_a){S} == LetAnno(x, tau, T_a, S)
      )
      assert(
        // paper: `u = T_a; S`
        let(u, T_a){S} == LetStrict(u, T_a, S)
      )
    end DslCheatSheet

//    // example from paper
//    let(u, x ~> *){u}
//      . check(Bty ~> Bty)
//
//    // lets with type annotations
    let(x :: T ->: T, z ->: z) {
      let(y :: T, *){x @@ y}
    }
      . check(T)

    let(x :: T ->: T, x1 ->: x1){
      let(y :: T ->: T ->: T, x2 ->: x){
        y
//        let(x3 :: Bty, *){
//          let(x3, x @@ u){
//            x3// let(x3, z){x3}
//          }
//        }
      }
    }.checkVerbose(T ->: T ->: T)

//    let(x :: Bty ~> Bty, u)(u).checkVerbose(Bty ~> Bty)

    // All and only valid SLC syntax is representable,
    // so scalac won't type-check the following
    // let(x, let(u, x ~> b)(u))(X)

//    let(x, x)(x).check(Bty)







