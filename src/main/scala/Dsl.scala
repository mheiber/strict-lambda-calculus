package slc

object Dsl:
  import Syntax.*
  import Checker.{analyze, analyzeVerbose}
  // using one-letter param names to stop IntelliJ param name hints
  def let(u: StrictVar, t: AnalysisTerm)(e: Expr): LetStrict =
    LetStrict(u, t, e)

  def let(x: StandardVar, t: SynthTerm)(e: Expr): LetStd =
    LetStd(x, t, e)

  def let(annotated: AnnoStdVar, term: AnalysisTerm)(e: Expr): LetAnno =
    LetAnno(annotated.x, annotated.ty, term, e)

  case class AnnoStdVar(x: StandardVar, ty: Ty)

  extension (v: StandardVar)  def ::(annoTy: Ty): AnnoStdVar = AnnoStdVar(v, annoTy)
  extension (v: StandardVar) def ->:(expr: Expr): Lam = Lam(v, expr)
  extension (x: StandardVar) def @@(arg: Var): Apply = Apply(x, arg)

  extension (returnTy: Ty) def ->:(paramTy: Ty): ArrowTy = ArrowTy(paramTy, returnTy)

  extension (e: Expr) def check(ty: Ty): Unit = analyze(e, ty)
  extension (e: Expr) def checkVerbose(ty: Ty): Unit = analyzeVerbose(e, ty)
