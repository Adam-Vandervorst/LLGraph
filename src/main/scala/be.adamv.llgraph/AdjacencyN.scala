/*
package be.adamv.llgraph

import collection.mutable

class AdjacencyN[T](lm: mutable.LongMap[mutable.TreeSet[NodeN[T]]]):
  def touch(n: NodeN[T]): Unit =
    lm.updateWith(n){case None => Some(mutable.TreeSet()); case v => v}

  def addEdge(s: NodeN[T], t: NodeN[T]): Unit =
    lm.updateWith(s){case None => Some(mutable.TreeSet(t)); case Some(vs) => {vs += t; Some(vs)}}

  def removeEdge(s: NodeN[T], t: NodeN[T]): Unit =
    lm.updateWith(s){case None => None; case Some(vs) => {vs -= t; Some(vs)}}

  def edges: Iterator[(NodeN[T], NodeN[T])] = lm.iterator.flatMap((s, ts) => ts.map((s.asInstanceOf, _)))
*/
