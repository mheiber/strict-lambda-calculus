package slc
object Syntax:
  sealed trait Ty

  /**
   * In the formal presentation, SLC has no ground terms, so it's not actually usable.
   * In the examples, there is a type `1` and a term `*` of that type.
   * Here, `b` is our ground term of type `Bty`
   */
  sealed trait BaseTy extends Ty
  case object Bty extends BaseTy
  case object b extends BaseTy with SynthTerm

  case class ArrowTy(paramTy: Ty, retTy: Ty) extends Ty:
    override def toString = s"$paramTy ~> $retTy"

  sealed trait Expr
  sealed trait SynthTerm extends AnalysisTerm
  sealed trait AnalysisTerm extends Expr
  sealed trait Var extends Expr

  case class StandardVar(name: String) extends Var with SynthTerm:
    override def toString = name
  
  case class StrictVar(name: String) extends Var with AnalysisTerm:
    override def toString = name

  case class Apply(f: StandardVar, arg: Var) extends SynthTerm:
    override def toString = s"$f @@ $arg"
  
  case class Lam(stdVar: StandardVar, s: Expr) extends AnalysisTerm:
    override def toString = s"${stdVar.name} ~> $s"
 
  case class LetStd(stdVar: StandardVar, synthTerm: SynthTerm, s: Expr) extends Expr:
    override def toString = s"let($stdVar, $synthTerm){$s}"
 
  case class LetStrict(u: StrictVar, analysisTerm: AnalysisTerm, s: Expr) extends Expr:
    override def toString = s"let($u, $analysisTerm){$s}"
 
  case class LetAnno(x: StandardVar, anno: Ty, analysisTerm: AnalysisTerm, s: Expr) extends Expr:
    override def toString = s"let($x :: $anno, $analysisTerm){$s}"
