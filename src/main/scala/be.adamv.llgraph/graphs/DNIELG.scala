package be.adamv.llgraph.graphs.DNIELG

import scala.collection.mutable

import be.adamv.llgraph.*


class DNIELG[EL] private(graph_id: Int):
  type Node = NodeN[this.type]
  inline given Conversion[Long, Node] = _.asInstanceOf
  val outgoing: mutable.LongMap[Set[(EL, Node)]] = mutable.LongMap()
  val incoming: mutable.LongMap[Set[(EL, Node)]] = mutable.LongMap()
  private var next_id = 0

  def addNodes[Other](ns: IterableOnce[NodeN[Other]]): IterableOnce[Node] =
    for n <- ns yield
      outgoing.touch(n)
      incoming.touch(n)
      n.asInstanceOf[Node]

  def newNode(): Node =
    val node = NodeN[this.type](graph_id, next_id)
    next_id += 1
    outgoing(node) = Set()
    incoming(node) = Set()
    node

  def newNodes(n: Int): List[Node] =
    List.tabulate(n)(_ => newNode())
  
  def newNodesFor[K](n: IterableOnce[K]): Map[K, Node] =
    n.iterator.map[(K, Node)](_ -> newNode()).toMap

  def connect(src: Node, dst: Node, label: EL): Unit =
    outgoing(src) += (label, dst)
    incoming(dst) += (label, src)

  def foreignConnect[Other](src: NodeN[Other], dst: NodeN[Other], label: EL): Unit =
    outgoing.addValue(src, (label, dst))
    outgoing.getOrElseUpdate(dst, Set())
    incoming.addValue(dst, (label, src))
    incoming.getOrElseUpdate(src, Set())

  def nodes: Iterable[Node] = outgoing.keys.asInstanceOf
  def label(a: NodeN[Any], b: NodeN[Any]): Option[EL] = outgoing.collectFirst{ case (`a`, ins) if ins.exists(_._2 == b) => ins.find(_._2 == b).get._1 }
  def sources: Iterable[Node] = incoming.collect{ case (n, ins) if ins.isEmpty || ins.map(_._2) == Set(n) => n }.asInstanceOf
  def proper_sources: Iterable[Node] = incoming.collect{ case (n, ins) if ins.isEmpty => n }.asInstanceOf
  def sinks: Iterable[Node] = outgoing.collect{ case (n, outs) if outs.isEmpty || outs.map(_._2) == Set(n) => n }.asInstanceOf
  def proper_sinks: Iterable[Node] = outgoing.collect{ case (n, outs) if outs.isEmpty => n }.asInstanceOf

  def plot(using output: String => Unit = println): Unit =
    for n <- nodes do
      output(s"${n.name} [label=\"\" shape=circle width=0.25]")
    for (n, nbs) <- outgoing
        (l, b) <- nbs do
      output(s"g${n.graphId}n${n.nodeId} -> g${b.graphId}n${b.nodeId} [label=\"$l\"];")

  override def equals(obj: Any): Boolean = obj match
    case g: DNIELG[EL] => g.outgoing == outgoing
    case _ => false


object DNIELG:
  private var next_id = 0

  def apply[EL](): DNIELG[EL] =
    val g = new DNIELG[EL](next_id)
    next_id += 1
    g
