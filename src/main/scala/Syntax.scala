package slc
object Syntax:
  sealed trait Ty

  type Var = StandardVar | StrictVar
  type SynthTerm = Var | Apply | *.type
  type AnalysisTerm = SynthTerm | StrictVar | Lam
  type Expr = Var | LetStd | LetAnno | LetStrict | *.type

  type Syn = AnalysisTerm | Expr

  // this corresponds to type 1 in the paper
  case object T extends Ty
  // The technical presentation in the paper does not have any ground terms
  case object *

  case class ArrowTy(paramTy: Ty, retTy: Ty) extends Ty:
    override def toString = s"($paramTy ~> $retTy)"

  case class StandardVar(name: String):
    override def toString = name

  case class StrictVar(name: String):
    override def toString = name

  case class Apply(f: StandardVar, arg: Var | *.type):
    override def toString = s"($f @@ $arg)"

  case class Lam(stdVar: StandardVar, s: Expr):
    override def toString = s"(${stdVar.name} ~> $s)"

  case class LetStd(stdVar: StandardVar, synthTerm: SynthTerm, s: Expr):
    override def toString = s"let($stdVar, $synthTerm){$s}"

  case class LetStrict(u: StrictVar, analysisTerm: AnalysisTerm, s: Expr):
    override def toString = s"let($u, $analysisTerm){$s}"

  case class LetAnno(x: StandardVar, anno: Ty, analysisTerm: AnalysisTerm, s: Expr):
    override def toString = s"let($x :: $anno, $analysisTerm){$s}"
