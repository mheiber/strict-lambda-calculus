package slc
import Syntax._
import Checker.{analyze}

object Dsl:
  // using one-letter param names to stop IntelliJ param name hints
  def let(u: StrictVar, t: AnalysisTerm)(e: Expr): LetStrict =
    LetStrict(u, t, e)

  def let(x: StandardVar, t: SynthTerm)(e: Expr): LetStd =
    LetStd(x, t, e)

  def let(annotated: AnnoStdVar, term: AnalysisTerm)(e: Expr): LetAnno =
    LetAnno(annotated.x, annotated.ty, term, e)

  case class AnnoStdVar(x: StandardVar, ty: Ty)

  implicit class Annotate(ty: Ty):
    def ::(v: StandardVar): AnnoStdVar = AnnoStdVar(v, ty)

  implicit class ToArrowTy(ty1: Ty):
    def ~>(ty2: Ty): ArrowTy = ArrowTy(ty1, ty2)

  implicit class ToLambda(v: StandardVar):
    def ~>(expr: Expr): Lam = Lam(v, expr)

  implicit class toApply(x: StandardVar):
    def @@(arg: Var): Apply = Apply(x, arg)

  implicit class TypeCheck(e: Expr):
    def check(ty: Ty): Unit = analyze(e, ty, Map.empty, Map.empty)
    def checkThrows(ty: Ty): Unit = {
      try check(ty)
      catch {
        case e: Error => return
      }
      // $COVERAGE-OFF$
      throw Error("Expected exception")
      // $COVERAGE-ON$
    }


    def checkVerbose(ty: Ty): Unit = {
      println("\nstart checking")
      analyze(e, ty, Map.empty, Map.empty)(verbose = true)
      println("success\n")
    }


