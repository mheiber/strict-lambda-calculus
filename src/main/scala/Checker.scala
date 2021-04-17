package slc
import Syntax._

object Checker:
  type GammaT = Map[StandardVar, Ty]
  type DeltaT = Map[StrictVar, Ty]

  sealed trait TyError extends Error:
    def msg: String

  case class InconsistentTypes(v: StrictVar, t1: Ty, t2: Ty) extends TyError:
    override def msg = s"var $v is used inconsistently: got types $t1 and $t2"

  case class TypeMismatch(got: Ty, expected: Ty) extends TyError:
    override def msg = s"got $got, expected: $expected"

  case class ExpectedArrow(got: Ty) extends TyError:
    override def msg = s"got $got, expected: function type"

  case class Unbound(v: Var) extends TyError:
    override def msg = s"unbound var $v"

  private def unionDeltas(delta1: DeltaT, delta2: DeltaT): DeltaT =
    validateDeltasConsistent(delta1, delta2)
    delta1 ++ delta2

  private def validateDeltasConsistent(delta1: DeltaT, delta2: DeltaT): Unit =
    for (strictVar <- delta1.keySet.intersect(delta2.keySet)) {
      val t1 = lookup(delta1, strictVar)
      val t2 = lookup(delta2, strictVar)
      if (t1 != t2) throw InconsistentTypes(strictVar, t1, t2)
    }

  private def lookup[V <: Var](m: Map[V, Ty], v: V): Ty =
    m.getOrElse(v, throw Unbound(v))

  def synth(e: SynthTerm, gamma: GammaT)(implicit verbose: Boolean = false): (Ty, DeltaT) =
    if (verbose) debugIn(s"begin synthesize `$e`. gamma = $gamma")
    val res: (Ty, DeltaT) = e match {
      // SVar
      case stdVar: StandardVar =>
        // if (delta.nonEmpty) throw new Error("nonempty delta")
        (lookup(gamma, stdVar), Map.empty)
      // SApp
      case Apply(x, y: StandardVar) =>
        gamma(x) match {
          case ArrowTy(sigma, tau) =>
            val argTy = lookup(gamma, y)
            if (argTy != sigma) throw TypeMismatch(argTy, sigma)
            (tau, Map.empty)
          case got => throw ExpectedArrow(got)
        }
      // SAppS
      case Apply(x, u: StrictVar) =>
        gamma(x) match {
          case ArrowTy(sigma, tau) =>
            (tau, Map(u -> sigma))
          case got => throw ExpectedArrow(got)
        }
      // Not covered in paper
      case b =>
        (Bty, Map.empty)
    }
    if (verbose) debugOut(s"end synthesize `$e`. ty = ${res._1} delta = ${res._2} ")
    res
  end synth

  def analyze(e: Expr, tau: Ty, gamma: GammaT, delta: DeltaT)(implicit verbose: Boolean = false): DeltaT =
    if (verbose) debugIn(s"begin analyze `$e` as `$tau`. gamma = $gamma. deltaIn = $delta")
    val res = e match {
      // SVarS
      case u: StrictVar =>
        delta + (u -> tau)
      // SSy
      case synthTerm: SynthTerm =>
        val (actualTy, delta1) = synth(synthTerm, gamma)
        if (actualTy != tau) throw TypeMismatch(actualTy, tau)
        delta1
      // SLambda
      case Lam(x, s) =>
        tau match {
          case ArrowTy(sigma, tau1) =>
            analyze(s, tau1, gamma + (x -> sigma), delta)
          case _ => throw ExpectedArrow(tau)
        }
      // SLetS
      case LetStd(x, synthTerm: SynthTerm, s) =>
        val (sigma, delta1) = synth(synthTerm, gamma)
        val delta2 = analyze(s, sigma, gamma + (x -> sigma), delta)
        unionDeltas(delta1, delta2)
      // SLetAS
      case LetStrict(u: StrictVar, aTerm: AnalysisTerm, s) =>
        // not sure about this one
        val deltaExt = analyze(s, tau, gamma, delta)
        val sigma = lookup(deltaExt, u)
        val delta2 = deltaExt - u
        val delta1 = analyze(aTerm, sigma, gamma, delta)
        unionDeltas(delta1, delta2)
      // SLetA
      case LetAnno(x, sigma, aTerm, s) =>
        // not sure about this one
        val delta2 = analyze(s, tau, gamma + (x -> sigma), delta)
        val delta1 = analyze(aTerm, sigma, gamma, delta)
        unionDeltas(delta1, delta2)
    }
    if (verbose) debugOut(s"end analyze `$e` as `$tau`. gamma = $gamma. deltaIn = $delta. deltaOut = $delta")
    res
  end analyze

  private var debugDepth = 0
  private def debugIn(s: => String): Unit =
    println(" " * debugDepth + s)
    debugDepth += 1

  private def debugOut(s: => String): Unit =
    println(" " * debugDepth + s)
    debugDepth -= 1
