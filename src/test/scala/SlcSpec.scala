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
    // and a ground term of that type
    assert(* == *)


    object DslCheatSheet:
      // WARNING: the syntax is really restrictive, just like in the paper.
      // See the type definitions in Syntax.scala or section 5.1

      // function abstraction. The variable must be a StandardVar
      assert(x ->: y == Lam(x, *))
      // the following are not valid syntax, because a lambda is from standardVar to expr
      // (x ->: y) ->: z
      // x ->: (y ->: z)
      // u ->: u
      // function application. Restricted to `StandardVar @@ Var`
      assert(x @@ y == Apply(x, y))
      // Just as in the paper, there are 3 kinds of `let`
      val T_s: SynthTerm = *
      val T_a: AnalysisTerm = *
      val S: Expr = *
      assert(
        // paper: `x = T_s; S`
        let(x, T_s){S} == LetStd(x, T_s, S)
      )
      assert(
        // paper: `x : tau = T_a; S`
        let(x :: T, T_a){S} == LetAnno(x, T, T_a, S)
      )
      // notice that the ->: in arrow types is right-associative
      assert(
        // paper: `x : 1 -> 1 -> 1 = *; *`
        let(x :: T ->: T ->: T, *){*} == LetAnno(x, ArrowTy(T, ArrowTy(T, T)), *, *)
      )
      assert(
        // paper: `u = T_a; S`
        let(u, T_a){S} == LetStrict(u, T_a, S)
      )
    end DslCheatSheet

    // example from paper
    let(u, x ->: *){u}
      . check(T ->: T)

    // lets with annotations
    let(x :: T ->: T, z ->: z) {
      let(y :: T, *) {
        let(x1, x @@ y) {
          x1
        }
      }
    }
      . check(T)

    let(x :: T ->: T, x1 ->: x1){
      let(y :: T ->: (T ->: T), x2 ->: x) {
        y
      }
//        let(x3 :: T, *){
//          let(x3, x @@ u){
//            x3// let(x3, z){x3}
//          }
//        }
//      }
    }.checkVerbose(T ->: T ->: T)

//    let(x :: Bty ~> Bty, u)(u).checkVerbose(Bty ~> Bty)

    // All and only valid SLC syntax is representable,
    // so scalac won't type-check the following
    // let(x, let(u, x ~> b)(u))(X)

//    let(x, x)(x).check(Bty)







