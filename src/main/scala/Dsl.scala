package slc
import Syntax._
import Checker.{analyze, analyzeVerbose}

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
  
  implicit class ToArrowTy(returnTy: Ty):
    def ->:(paramTy: Ty): ArrowTy = ArrowTy(paramTy, returnTy)

  implicit class ToLambda(expr: Expr):
    def ->:(v: StandardVar): Lam = Lam(v, expr)

  implicit class toApply(x: StandardVar):
    def @@(arg: Var): Apply = Apply(x, arg)

  implicit class TypeCheck(e: Expr):
    def check(ty: Ty): Unit = analyze(e, ty)
    def checkVerbose(ty: Ty): Unit = analyzeVerbose(e, ty)


