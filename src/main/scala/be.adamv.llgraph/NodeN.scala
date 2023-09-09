package be.adamv.llgraph


type NodeN[T] = Long & T
object NodeN:
  inline def apply[T](graphId: Int, nodeId: Int): NodeN[T] =
     ((graphId.toLong << 32) | nodeId).asInstanceOf

extension [T](n: NodeN[T])
  inline def name: String = s"g${n >> 32}n${n.toInt}"
  inline def graphId: Int = (n >> 32).toInt
  inline def nodeId: Int = n.toInt
