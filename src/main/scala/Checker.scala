package slc

object Checker:
  import Syntax._
  type GammaT = Map[StandardVar, Ty]
  type DeltaT = Map[StrictVar, Ty]

  class TyError(syn: Syn, message: String) extends Error(s"error at `$syn`: $message")

  case class InconsistentTypes(syn: Syn, v: StrictVar, t1: Ty, t2: Ty) extends TyError(
    syn, s"var $v is used inconsistently: got types $t1 and $t2"
  )

  case class TypeMismatch(syn: Syn, got: Ty, expected: Ty) extends TyError(
    syn, s"got $got, expected: $expected"
  )

  case class ExpectedArrow(syn: Syn, got: Ty) extends TyError(
    syn, s"got $got, expected: function type"
  )

  case class Unbound(expr: Syn, v: Var) extends TyError(expr, s"unbound var $v")

  def synthesize(e: SynthTerm): Ty = synthesize(e, Map.empty)(using verbose = false)._1
  def synthesizeVerbose(e: SynthTerm): Ty = synthesize(e, Map.empty)(using verbose = true)._1
  def analyze(e: Syn, expected: Ty): Unit = analyze(e, expected, Map.empty, Map.empty)(using verbose = false)
  def analyzeVerbose(e: Syn, expected: Ty): Unit = {
    debugIn("start analyzing")
    analyze(e, expected, Map.empty, Map.empty)(using verbose = true)
    debugIn("end analyzing")
  }

  private def unionDeltas(syn: Syn, delta1: DeltaT, delta2: DeltaT): DeltaT =
    validateDeltasConsistent(syn, delta1, delta2)
    delta1 ++ delta2

  private def validateDeltasConsistent(syn: Syn, delta1: DeltaT, delta2: DeltaT): Unit =
    for (strictVar <- delta1.keySet.intersect(delta2.keySet)) {
      val t1 = lookup(syn, delta1, strictVar)
      val t2 = lookup(syn, delta2, strictVar)
      if (t1 != t2) throw InconsistentTypes(syn, strictVar, t1, t2)
    }

  private def lookup[V <: Var](syn: Syn, m: Map[V, Ty], v: V): Ty =
    m.getOrElse(v, throw Unbound(syn, v))

  private def synthesize(e: SynthTerm, gamma: GammaT)(using verbose: Boolean = false): (Ty, DeltaT) =
    if (verbose) debugIn(s"begin synthesize `$e`. gamma = $gamma")
    val res: (Ty, DeltaT) = e match {
      // SVar
      case stdVar: StandardVar =>
        // if (delta.nonEmpty) throw new Error("nonempty delta")
        (lookup(e, gamma, stdVar), Map.empty)
      // SApp
      case Apply(x, y: StandardVar) =>
        gamma(x) match {
          case ArrowTy(sigma, tau) =>
            val argTy = lookup(e, gamma, y)
            if (argTy != sigma) throw TypeMismatch(e, argTy, sigma)
            (tau, Map.empty)
          case got => throw ExpectedArrow(e, got)
        }
      // SAppS
      case Apply(x, u: StrictVar) =>
        gamma(x) match {
          case ArrowTy(sigma, tau) =>
            (tau, Map(u -> sigma))
          case got => throw ExpectedArrow(e, got)
        }
      // This rule isn't given explicitly in the paper
      case b =>
        (T, Map.empty)
    }
    if (verbose) debugOut(s"end synthesize `$e`. ty = ${res._1}. delta = ${res._2} ")
    res
  end synthesize

  private def analyze(e: Syn, tau: Ty, gamma: GammaT, delta: DeltaT)(using verbose: Boolean = false): DeltaT =
    if (verbose) debugIn(s"begin analyze `$e` against `$tau`. gamma = $gamma. deltaIn = $delta")
    val res = e match {
      // SVarS
      case u: StrictVar =>
        delta + (u -> tau)
      // SSy
      case synthTerm: SynthTerm =>
        val (actualTy, delta1) = synthesize(synthTerm, gamma)
        if (actualTy != tau) throw TypeMismatch(e, actualTy, tau)
        delta1
      // SLambda
      case Lam(x, s) =>
        tau match {
          case ArrowTy(sigma, tau1) =>
            analyze(s, tau1, gamma + (x -> sigma), delta)
          case _ => throw ExpectedArrow(e, tau)
        }
      // SLetS
      case LetStd(x, synthTerm: SynthTerm, s) =>
        val (sigma, delta1) = synthesize(synthTerm, gamma)
        val delta2 = analyze(s, sigma, gamma + (x -> sigma), delta)
        unionDeltas(e, delta1, delta2)
      // SLetAS
      case LetStrict(u: StrictVar, aTerm: AnalysisTerm, s) =>
        // not sure about this one
        val deltaExt = analyze(s, tau, gamma, delta)
        val sigma = lookup(e, deltaExt, u)
        val delta2 = deltaExt - u
        val delta1 = analyze(aTerm, sigma, gamma, delta)
        unionDeltas(e, delta1, delta2)
      // SLetA
      case LetAnno(x, sigma, aTerm, s) =>
        // not sure about this one
        val delta2 = analyze(s, tau, gamma + (x -> sigma), delta)
        val delta1 = analyze(aTerm, sigma, gamma, delta)
        unionDeltas(e, delta1, delta2)
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
