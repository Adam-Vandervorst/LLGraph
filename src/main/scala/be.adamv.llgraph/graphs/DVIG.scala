package be.adamv.llgraph.graphs.DVIG

import scala.collection.mutable

import be.adamv.llgraph.*

class DVIG[V]:
  val outgoing: mutable.Map[V, Set[V]] = mutable.Map()
  val incoming: mutable.Map[V, Set[V]] = mutable.Map()

  def addNodes(ns: IterableOnce[V]): IterableOnce[V] =
    for n <- ns yield
      outgoing.touch(n)
      incoming.touch(n)
      n

  def connect(src: V, dst: V): Unit =
    outgoing(src) += dst
    incoming(dst) += src

  def connected(src: V, dst: V): Boolean =
    outgoing(src).contains(dst)

  def nodes: Iterable[V] = outgoing.keys
  def sources: Iterable[V] = incoming.collect{ case (n, ins) if ins.isEmpty || ins == Set(n) => n }.asInstanceOf
  def proper_sources: Iterable[V] = incoming.collect{ case (n, ins) if ins.isEmpty => n }.asInstanceOf
  def sinks: Iterable[V] = outgoing.collect{ case (n, outs) if outs.isEmpty || outs == Set(n) => n }.asInstanceOf
  def proper_sinks: Iterable[V] = outgoing.collect{ case (n, outs) if outs.isEmpty => n }.asInstanceOf

  def plot(): Unit =
    for (n, i) <- nodes.zipWithIndex do
      println(s"$i [label=\"$n\" shape=circle width=0.25]")
    for (a, i) <- nodes.zipWithIndex
        (b, j) <- nodes.zipWithIndex
        if connected(a, b) do
      println(s"$i -> $j;")

  override def equals(obj: Any): Boolean = obj match
    case g: DVIG[V] => g.outgoing == outgoing
    case _ => false

given DVIGasGraph[V, G <: DVIG[V]]: DirectedGraph[G] with
  type Node = V

  extension (g: G)
    def nodeIt = g.nodes.iterator
    def at(node: Node) =
      given G = g
      new DirectedGraphNodeOps:
        val n = node
        override def sourceIt: Iterator[Node] = g.incoming(n).iterator
        override def targetIt: Iterator[Node] = g.outgoing(n).iterator
