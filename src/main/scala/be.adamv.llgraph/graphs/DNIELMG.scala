package be.adamv.llgraph.graphs.DNIELMG

import scala.collection.mutable
import scala.util.Try

import be.adamv.llgraph.*


class DNIELMG[EL] private (graphId: Int):
  type Node = NodeN[this.type]
  inline given Conversion[Long, Node] = _.asInstanceOf
  val outgoing: mutable.LongMap[Set[Node]] = mutable.LongMap()
  val incoming: mutable.LongMap[Set[Node]] = mutable.LongMap()
  val labeling: mutable.Map[(NodeN[Any], NodeN[Any]), Set[EL]] = mutable.Map().withDefaultValue(Set())
  private var nextId = 0

  def addNodes[Other](ns: IterableOnce[NodeN[Other]]): IterableOnce[Node] =
    for n <- ns yield
      outgoing.touch(n)
      incoming.touch(n)
      n

  inline def newNode(): Node =
    val node = NodeN[this.type](graphId, nextId)
    nextId += 1
    outgoing(node) = Set()
    incoming(node) = Set()
    node

  def newNodes(n: Int): List[Node] =
    List.tabulate(n)(_ => newNode())

  def connect(src: Node, dst: Node, label: EL): Unit =
    outgoing(src) += dst
    incoming(dst) += src
    labeling(src, dst) += label

  def foreignConnect[Other](src: NodeN[Other], dst: NodeN[Other], label: EL): Unit =
    outgoing.addValue(src, dst)
    outgoing.touch(dst)
    incoming.addValue(dst, src)
    incoming.touch(src)
    labeling(src, dst) += label

  def nodes: Iterable[Node] = outgoing.keys.asInstanceOf
  def sources: Iterable[Node] = incoming.collect{ case (n, ins) if ins.isEmpty || ins == Set(n) => n }.asInstanceOf
  def proper_sources: Iterable[Node] = incoming.collect{ case (n, ins) if ins.isEmpty => n }.asInstanceOf
  def sinks: Iterable[Node] = outgoing.collect{ case (n, outs) if outs.isEmpty || outs == Set(n) => n }.asInstanceOf
  def proper_sinks: Iterable[Node] = outgoing.collect{ case (n, outs) if outs.isEmpty => n }.asInstanceOf

  def plot(using output: String => Unit = println): Unit =
    for n <- nodes do
      output(s"${n.name} [label=\"\" shape=circle width=0.25]")
    for (n, nbs) <- outgoing
        b <- nbs do
      output(s"${n.name} -> ${b.name} [label=\"${labeling(n, b).mkString(",")}\"];")

  def labelMap[EL2](f: EL => EL2): DNIELMG[EL2] =
    val g = DNIELMG[EL2]()
    g.outgoing.addAll(this.outgoing.asInstanceOf)
    g.incoming.addAll(this.incoming.asInstanceOf)
    g.labeling.addAll(this.labeling.map((k, ls) => (k, ls.map(f))))
    g

  infix def union(other: DNIELMG[EL]): DNIELMG[EL] =
    val g = DNIELMG[EL]()
    g.outgoing.merge(this.outgoing.asInstanceOf)(_ | _); g.outgoing.merge(other.outgoing.asInstanceOf)(_ | _)
    g.incoming.merge(this.incoming.asInstanceOf)(_ | _); g.incoming.merge(other.incoming.asInstanceOf)(_ | _)
    g.labeling.merge(this.labeling)(_ | _); g.labeling.merge(other.labeling)(_ | _)
    g

  def isolate(m: DNIELMG[EL]): (DNIELMG[EL], DNIELMG[EL]) =
    val j = DNIELMG[EL]()
    val c = DNIELMG[EL]()
    val mset = m.nodes.toSet

    for (s, outs) <- outgoing; t <- outs; label <- labeling(s, t) do
      if mset.contains(s) != mset.contains(t) then
        j.foreignConnect(s, t, label)
      else if !mset.contains(s) && !mset.contains(t) then
        c.foreignConnect(s, t, label)
      else if !m.labeling(s, t).contains(label) then
        j.foreignConnect(s, t, label)
    c.addNodes(nodes.toSet -- mset)

    (j, c)

  def select[EL2](m: DNIELMG[EL2], mapping: (m.Node => Node, EL2 => EL)): DNIELMG[EL] =
    val subgraph = DNIELMG[EL]()
    val (nmap, lmap) = mapping

    subgraph.addNodes(m.nodes.map(nmap))
    for (mn, mts) <- m.outgoing; mt <- mts; label <- m.labeling(mn, mt) do
      subgraph.foreignConnect(nmap(mn.asInstanceOf[m.Node]), nmap(mt), lmap(label))

    subgraph

  def labeledIsomorphismNaive(m: DNIELMG[EL]): Option[Map[m.Node, Node]] =
    def hasConsistentNeighbours(mapping: Map[m.Node, Node]): Boolean =
      mapping.forall((mn, n) => m.outgoing(mn).map(mapping) == outgoing(n))

    def hasConsistentLabelling(mapping: Map[m.Node, Node]): Boolean =
      mapping.forall((mn1, n1) => mapping.forall((mn2, n2) => m.labeling(mn1, mn2) == labeling(n1, n2)))

    val all_mappings: Iterator[Map[m.Node, Node]] = possible_mappings(m.nodes, nodes.toSet)
    val consistent_mappings: Iterator[Map[m.Node, Node]] = all_mappings.filter(hasConsistentNeighbours)
    val labeled_mapping: Option[Map[m.Node, Node]] = consistent_mappings.find(hasConsistentLabelling)

    labeled_mapping

  def labeledOccurrencesNaive(m: DNIELMG[EL]): Iterator[Map[m.Node, Node]] =
    def hasConsistentNeighbours(mapping: Map[m.Node, Node]): Boolean =
      mapping.forall((mn, n) => m.outgoing(mn).map(mapping) <= outgoing(n))

    def hasConsistentLabelling(mapping: Map[m.Node, Node]): Boolean =
      mapping.forall((mn1, n1) => mapping.forall((mn2, n2) => m.labeling(mn1, mn2) <= labeling(n1, n2)))

    val all_mappings: Iterator[Map[m.Node, Node]] = possible_mappings(m.nodes, nodes.toSet)
    val consistent_mappings: Iterator[Map[m.Node, Node]] = all_mappings.filter(hasConsistentNeighbours)
    val labeled_mappings: Iterator[Map[m.Node, Node]] = consistent_mappings.filter(hasConsistentLabelling)

    labeled_mappings

  def occurrencesNaive[EL2](m: DNIELMG[EL2]): Iterator[(Map[m.Node, Node], Map[EL2, EL])] =
    def hasConsistentNeighbours(mapping: Map[m.Node, Node]): Boolean =
      mapping.forall((mn, n) => m.outgoing(mn).map(mapping) <= outgoing(n))

    def consistentLabellings(mapping: Map[m.Node, Node]): Iterator[(Map[m.Node, Node], Map[EL2, EL])] =
      val (mrlabeling, rlabeling) = mapping.flatMap((mn1, n1) => mapping.map((mn2, n2) => (m.labeling(mn1, mn2), labeling(n1, n2)))).unzip
      val (mlabels, labels) = (mrlabeling.flatten.toSet, rlabeling.flatten.toSet)
      if labels.size < mlabels.size then
        Iterator.empty
      else
        val all_label_mappings = possible_mappings[EL2, EL](mlabels, labels)
        val consistent_label_mappings = all_label_mappings.filter(lmapping =>
          mapping.forall((mn1, n1) => mapping.forall((mn2, n2) => m.labeling(mn1, mn2).map(lmapping) <= labeling(n1, n2))))
        Iterator.continually(mapping) zip consistent_label_mappings

    val all_mappings: Iterator[Map[m.Node, Node]] = possible_mappings(m.nodes, nodes.toSet)
    val consistent_mappings: Iterator[Map[m.Node, Node]] = all_mappings.filter(hasConsistentNeighbours)
    val labeled_mappings: Iterator[(Map[m.Node, Node], Map[EL2, EL])] = consistent_mappings.flatMap(consistentLabellings)

    labeled_mappings

  private def universalMappingPrior[EL2](m: DNIELMG[EL2]): Map[Node, Set[m.Node]] =
    nodes.map(n => n -> m.nodes.toSet[m.Node]).toMap

  private def mappingPrior[EL2](m: DNIELMG[EL2]): Map[Node, Set[m.Node]] =
    nodes.map(n => n -> m.nodes.iterator
      .filter(nm => labeling(n, n).size >= labeling(nm, nm).size)
      .filter(nm => canContain(outgoing(n).toList.map(out => labeling(n, out).size), m.outgoing(nm).toList.map(outm => m.labeling(nm, outm).size)))
      .filter(nm => canContain(incoming(n).toList.map(in => labeling(in, n).size), m.incoming(nm).toList.map(inm => m.labeling(inm, nm).size)))
      .toSet[m.Node]
    ).toMap

  def occurrences[EL2](m: DNIELMG[EL2]): Set[(Map[m.Node, Node], Map[EL2, EL])] =
    if m.nodes.size > nodes.size then return Set()
    val init = universalMappingPrior(m)
    if init.values.flatten.toSet != m.nodes.toSet then return Set()

    val finished = mutable.Set.empty[Walker]

    case class Walker(stack: List[Node], stackm: List[m.Node], labels: Map[EL, EL2]):
      def identities: Map[Node, m.Node] = (stack zip stackm).toMap.asInstanceOf

      def step(lm: mutable.Set[Walker]): Unit  =
        val outs = outgoing(stack.head)
        val outsm = m.outgoing(stackm.head)
        if outsm.isEmpty then finished.add(this)
        else
          for outm <- outsm; out <- outs do
            val selfLoop = stack.contains(out)
            if selfLoop == stackm.contains(outm) then
              val nws = labeling(stack.head, out) -- labels.keySet
              val nwsm = m.labeling(stackm.head, outm) -- labels.values.toSet
              for pm <- possible_partial_mappings(nws, nwsm) do
                val nwLabels = labels.merged(pm)((_, _) => throw RuntimeException("Should be disjoint"))
                if m.labeling(stackm.head, outm) <= labeling(stack.head, out).collect(nwLabels.get.unlift) then
                  if selfLoop then finished.add(Walker(stack, stackm, nwLabels))
                  else lm.add(Walker(out::stack, outm::stackm, nwLabels))

      override def toString: String =
        s"W(${stack.map(_.nodeId).mkString("-")}, ${stackm.map(_.nodeId).mkString("-")}, ${labels.map((el, elm) => s"$el=$elm").mkString(" ")})"

    object Walker:
      def initial(start: Node, startm: m.Node): Walker = Walker(start::Nil, startm::Nil, Map.empty)

    var walkers: mutable.Set[Walker] = mutable.Set.from(init.flatMap((n, nms) => nms.map(nm => Walker.initial(n, nm))))
    var walkers2: mutable.Set[Walker] = mutable.Set[Walker]()

    for _ <- 1 to m.nodes.size do
      for walker <- walkers do
        walker.step(walkers2)
      walkers = walkers2
      walkers2 = mutable.Set[Walker]()

    println()

    println(finished)

    println("nm ws")
    for nm <- m.nodes do
      println(nm.nodeId + 3)
      val ws = finished.filter(_.stackm.last == nm)
      if ws.nonEmpty then println("  " + ws.mkString(";  "))
      else println("  /")

    println()

    def merge(x: (Map[m.Node, Node], Map[EL2, EL]), y: (Map[m.Node, Node], Map[EL2, EL])): Option[(Map[m.Node, Node], Map[EL2, EL])] =
      Try {
        assert((x._1.keySet & y._1.keySet).forall(nm => x._1(nm) == y._1(nm))) // nodes agree on domain
        val nodeMap: Map[m.Node, Node] = x._1.merged(y._1)((n1, n2) => if n1 == n2 then n1 else throw Exception("incompatible"))
        val labelMap: Map[EL2, EL] = x._2.merged(y._2)((l1, l2) => if l1 == l2 then l1 else throw Exception("incompatible"))
        assert(labelMap.values.toSet.size == labelMap.size) // relabeling is injective
        (nodeMap, labelMap)
      }.toOption

    val labeled_mappings = m.nodes.iterator.map(nm => {
      val relevant_options = finished.collect {
        case w if w.stackm.last == nm => (w.identities.map(_.swap).asInstanceOf[Map[m.Node, Node]], w.labels.map(_.swap))
      }
      mergeFix(relevant_options, merge.tupled.unlift)
    }).reduce((rs, nm) => rs.flatMap(r => nm.flatMap(o => merge(r, o))))
      .filter(_._2.size == m.labeling.values.flatten.toSet.size)

    println()
    println("full")

    for case (m1, m2) <- labeled_mappings do
      println(m1.map((nm, n) => s"${nm.nodeId + 3}=${n.nodeId + 1}"))
      println(m2.map((el2, el) => s"$el2=$el"))

    labeled_mappings

  override def equals(obj: Any): Boolean = obj match
    case g: DNIELMG[EL] => g.outgoing == outgoing && g.labeling == labeling
    case _ => false

object DNIELMG:
  private var next_id = 0

  def apply[EL](): DNIELMG[EL] =
    val g = new DNIELMG[EL](next_id)
    next_id += 1
    g

given DNIELMGasGraph[EL, MG <: DNIELMG[EL] & Singleton](using vmg: ValueOf[MG]): NodeIdentified_MultiGraph[MG] with
  type Node = vmg.value.Node

  extension (g: MG)
    def nodeIt = vmg.value.nodes.iterator
    def at(node: Node) =
      given g.type = g
      new NodeIdentified_MultiGraphNodeOps:
        val n = node
        override def neighbourIt: Iterator[Node] = vmg.value.outgoing(n).iterator
        override def Nneighbouring(m: Node): Int = vmg.value.labeling(n, m).size

object DSL:
  class PartialSpec[EL, MG <: DNIELMG[EL]](using val MG: MG)(val src: MG.Node, val els: Seq[EL])

  extension [EL, MG <: DNIELMG[EL]](using MG: MG)(n: MG.Node)
    def apply(els: EL*) : PartialSpec[EL, MG] = PartialSpec(using MG)(n, els)
  extension [EL, MG <: DNIELMG[EL]](ps: PartialSpec[EL, MG])
    def ->(dst: ps.MG.Node) : ps.MG.Node =
      for el <- ps.els do ps.MG.connect(ps.src, dst, el)
      dst
