package be.adamv.llgraph.graphs.PatchG

import scala.collection.mutable
import be.adamv.llgraph.*
import be.adamv.llgraph.graphs.DNIELMG.DNIELMG


class RewriteRule[EL, MGL <: DNIELMG[EL], MGR <: DNIELMG[EL]]
                 (val lhs: PatchG[EL, MGL], val rhs: PatchG[EL, MGR])
                 (val rel: Set[(lhs.PT, rhs.PT)]):
  def plot(using output: String => Unit = println): Unit =
    val v = rel.map((l, r) => rel.getLeft(r) cart rel.getRight(l)).toList
    output("subgraph cluster_lhs {\nlabel = \"LHS\";\ncolor = \"grey\";")
    lhs.mg.plot(using output)
    for pt <- lhs.pts do pt.plot(v.getIndexWhere(_.exists(_._1 == pt)).fold("")(_.toString))
    output("}\n")
    output("subgraph cluster_rhs {\nlabel = \"RHS\";\ncolor = \"grey\";")
    rhs.mg.plot(using output)
    for pt <- rhs.pts do pt.plot(v.getIndexWhere(_.exists(_._2 == pt)).fold("")(_.toString))
    output("}")

  def apply[EL2](g: DNIELMG[EL2], matching: (lhs.mg.Node => g.Node, EL => EL2)): Option[DNIELMG[EL2]] =
    import PatchTypes.*
    val (nmap, lmap) = matching
    val m = g.select(lhs.mg, matching)
    def mcontains(n: NodeN[Any]) = m.nodes.toSet.contains(n)
    val (j, c) = g.isolate(m)

    val jes: Set[(j.Node, j.Node)] = j.outgoing.toList.flatMap((js, jts) => jts.map((js, _))).toSet.asInstanceOf
    val pts = rel.map((lpt, rpt) => (lpt.mapNodeN(nmap), rpt))

    val adherence = lhs.pts.map(_.mapNodeN(nmap)).map(lpt => lpt -> jes.collect {
      case (js, jt) if lpt.matches(js, jt) && lpt.ctx(js, jt).forall(!mcontains(_)) => (js, jt)
    }).toMap

    if adherence.values.flatten.toSet != jes || !pts.rl.values.forall(lpts => 
        lpts.map(lpt => adherence(lpt).map((js, jt) => lpt.ctx(js, jt) -> j.labeling(js, jt))).allEqual) then None
    else
      val j_ = DNIELMG[EL2]()

      for (lpt, rpt) <- pts
          (js, jt) <- adherence(lpt)
          label <- j.labeling(js, jt) do (lpt, rpt) match
        case (_, AnyNodeToNode(s, t, _)) => j_.foreignConnect(s, t, label)
        case (AnyContextToNode(_, _), AnyContextToNode(t, _)) => j_.foreignConnect(js, t, label)
        case (AnyNodeToContext(_, _), AnyNodeToContext(s, _)) => j_.foreignConnect(s, jt, label)
        case (AnyNodeToContext(_, _), AnyContextToNode(t, _)) => j_.foreignConnect(jt, t, label)
        case (AnyContextToNode(_, _), AnyNodeToContext(s, _)) => j_.foreignConnect(s, js, label)
        case _ => throw RuntimeException("Unreachable")

      Some(rhs.mg.labelMap(lmap) union j_ union c)

  def applyIt[EL2](g: DNIELMG[EL2]): Iterator[DNIELMG[EL2]] =
    g.occurrencesNaive(lhs.mg).collect(((p: (lhs.mg.Node => g.Node, EL => EL2)) => apply[EL2](g, p)).unlift)

  def applyItLabeled(g: DNIELMG[EL]): Iterator[DNIELMG[EL]] =
    g.labeledOccurrencesNaive(lhs.mg).collect(((p: lhs.mg.Node => g.Node) => apply(g, (p, identity))).unlift)

object RewriteRule:
  def apply[EL, MGL <: DNIELMG[EL], MGR <: DNIELMG[EL]](lhs: PatchG[EL, MGL], rhs: PatchG[EL, MGR], irel: Set[(Int, Int)]): RewriteRule[EL, MGL, MGR] =
    new RewriteRule(lhs, rhs)(irel.deindexify(lhs.pts, rhs.pts))

enum PatchTypes[N <: NodeN[Any]](val id: Int):
  case AnyContextToNode[N <: NodeN[Any]](t: N, override val id: Int) extends PatchTypes[N](id)
  case AnyNodeToContext[N <: NodeN[Any]](s: N, override val id: Int) extends PatchTypes[N](id)
  case AnyNodeToNode[N <: NodeN[Any]](s: N, t: N, override val id: Int) extends PatchTypes[N](id)

  def show(): String = this match
    case AnyContextToNode(t, _) => s"<>-(${t.name})"
    case AnyNodeToContext(s, _) => s"(${s.name})-<>"
    case AnyNodeToNode(s, t, _) => s"(${s.name})-<>-(${t.name})"

  def plot(label: String = "")(using output: String => Unit = println): Unit = this match
    case AnyContextToNode(t, id) =>
      output(s"c$id${t.name} [style=invis]")
      output(s"c$id${t.name} -> ${t.name} [label=\"$label\" style=dotted]")
    case AnyNodeToContext(s, id) =>
      output(s"c$id${s.name} [style=invis]")
      output(s"${s.name} -> c$id${s.name} [label=\"$label\" style=dotted]")
    case AnyNodeToNode(s, t, _) =>
      output(s"${s.name} -> ${t.name} [label=\"$label\" style=dotted]")

  def ctx[N2 <: NodeN[Any]](s: => N2, t: => N2): Option[N2] = this match
    case AnyContextToNode(_, _) => Some(s)
    case AnyNodeToContext(_, _) => Some(t)
    case AnyNodeToNode(_, _, _) => None

  def matches[N2 <: NodeN[Any]](s: => N2, t: => N2): Boolean = this match
    case AnyContextToNode(t_, _) => t == t_
    case AnyNodeToContext(s_, _) => s == s_
    case AnyNodeToNode(s_, t_, _) => s == s_ && t == t_

  def mapNodeN[N2 <: NodeN[Any]](f: N => N2): PatchTypes[N2] = this match
    case AnyContextToNode(t, id) => AnyContextToNode(f(t), id)
    case AnyNodeToContext(s, id) => AnyNodeToContext(f(s), id)
    case AnyNodeToNode(s, t, id) => AnyNodeToNode(f(s), f(t), id)


class PatchG[EL, MG <: DNIELMG[EL]](val mg: MG)(val pts: List[PatchTypes[mg.Node]]):
  type PT = PatchTypes[mg.Node]

  def plot(using output: String => Unit = println): Unit =
    mg.plot(using output)
    for (pt, i) <- pts.zipWithIndex do
      pt.plot(i.toString)


case object C

object PatchG:
  inline def apply[EL, MG <: DNIELMG[EL]](inline mg: MG)(inline ptps: List[(mg.Node | C.type, mg.Node | C.type)]): PatchG[EL, MG] =
    new PatchG(mg)(ptps.zipWithIndex.map{
      case ((C, C), _) => throw RuntimeException("Can not have patch types not connecting the match")
      case ((C, n), i) => PatchTypes.AnyContextToNode(n.asInstanceOf, i)
      case ((n, C), i) => PatchTypes.AnyNodeToContext(n.asInstanceOf, i)
      case ((n, m), i) => PatchTypes.AnyNodeToNode(n.asInstanceOf, m.asInstanceOf, i)
    })
