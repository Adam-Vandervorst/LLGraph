package be.adamv.llgraph

/// BASE
trait Graph[G]:
  type Node
  extension (g: G)
    def nodeIt: Iterator[Node]
    def edgeIt: Iterator[(Node, Node)] = nodeIt.flatMap(s => g.at(s).neighbourIt.map(t => (s, t))) // todo dedup
    def at(n: Node): GraphNodeOps

  trait GraphNodeOps(using ev: G):
    val n: Node
    def neighbourIt: Iterator[Node]
    infix def neighbouring(m: Node): Boolean = n.neighbourIt.contains(m)

  implicit def nodeOps(n: Node)(using g: G): GraphNodeOps = g.at(n)

/// DERIVATIVES - 1
trait RootedGraph[G] extends Graph[G]:
  val root_node: Node

  extension (g: G)
    def root: Node = root_node
    def nonRootNodeIt: Iterator[Node] = g.nodeIt.filter(_ != root)
    def at(n: Node): RootedGraphNodeOps

  trait RootedGraphNodeOps(using ev: G) extends GraphNodeOps:
    def isRoot: Boolean = n == root_node

  override implicit def nodeOps(n: Node)(using g: G): RootedGraphNodeOps = g.at(n)

trait MultiGraph[G] extends Graph[G]:
  extension (g: G)
    def edgeNIt: Iterator[(Node, Node, Int)] = g.nodeIt.flatMap(s => g.at(s).neighbourIt.map(t => (s, t, g.at(s).Nneighbouring(t)))) // todo dedup
    def at(n: Node): MultiGraphNodeOps

  trait MultiGraphNodeOps(using ev: G) extends GraphNodeOps:
    infix def Nneighbouring(m: Node): Int
//    override infix def neighbouring(m: Node): Boolean = (n Nneighbouring m) > 0

  override implicit def nodeOps(n: Node)(using g: G): MultiGraphNodeOps = g.at(n)

trait DirectedGraph[G] extends Graph[G]:
  extension (g: G)
    def dedgeIt: Iterator[(Node, Node)] = g.nodeIt.flatMap(s => g.at(s).targetIt.map(t => (s, t)))
    def at(n: Node): DirectedGraphNodeOps

  trait DirectedGraphNodeOps(using ev: G) extends GraphNodeOps:
    def sourceIt: Iterator[Node]
    def targetIt: Iterator[Node]
    infix def connected(m: Node): Boolean = n.targetIt.contains(m)
    def neighbourIt: Iterator[Node] = (sourceIt concat targetIt).distinct
    override infix def neighbouring(m: Node): Boolean = (n connected m) || (m connected n)

  override implicit def nodeOps(n: Node)(using g: G): DirectedGraphNodeOps = g.at(n)

/// DERIVATIVES - 1-1
trait DirectedMultiGraph[G] extends MultiGraph[G], DirectedGraph[G]:
  extension (g: G)
    def dedgeNIt: Iterator[(Node, Node, Int)] = g.dedgeIt.map(e => (e._1, e._2, g.at(e._1).Nconnected(e._2)))
    def at(n: Node): DirectedMultiGraphNodeOps

  trait DirectedMultiGraphNodeOps(using ev: G) extends DirectedGraphNodeOps, MultiGraphNodeOps:
    def sourceIt: Iterator[Node]
    def targetIt: Iterator[Node]
    infix def Nneighbouring(m: Node): Int = (n Nconnected m) + (m Nconnected n)
//    override infix def connected(m: Node): Boolean = (n Nconnected m) > 0
    infix def Nconnected(m: Node): Int

  override implicit def nodeOps(n: Node)(using g: G): DirectedMultiGraphNodeOps = g.at(n)

/// DERIVATIVES - 2
trait NodeIdentified_MultiGraph[G <: Singleton] extends MultiGraph[G]:
  extension (g: G)
    def at(n: Node): NodeIdentified_MultiGraphNodeOps

  trait NodeIdentified_MultiGraphNodeOps(using ev: G) extends MultiGraphNodeOps
  override implicit def nodeOps(n: Node)(using g: G): NodeIdentified_MultiGraphNodeOps = g.at(n)


/// NODE IMPLICITS
inline implicit def appG[G <: Singleton](using g: G, Gr: Graph[G])(a: Gr.Node): Gr.GraphNodeOps = g.at(a)
inline implicit def appDG[G <: Singleton](using g: G, Gr: DirectedGraph[G])(a: Gr.Node): Gr.DirectedGraphNodeOps = g.at(a)
inline implicit def appNIMG[G <: Singleton](using g: G, Gr: NodeIdentified_MultiGraph[G])(a: Gr.Node): Gr.NodeIdentified_MultiGraphNodeOps = g.at(a)
