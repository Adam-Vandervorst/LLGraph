package be.adamv.llgraph

import collection.mutable


extension [K, V](m1: Map[K, V])
  def merged(m2: Map[K, V])(dedup: (V, V) => V): Map[K, V] =
    (m1 -- m2.keySet) ++ m2.map((k, v) => k -> m1.get(k).map(dedup(v, _)).getOrElse(v))

  def inverse: Map[V, K] = m1.map(_.swap)

  def identifications: (PartialFunction[K, Int], PartialFunction[V, Int]) =
    val vals = m1.values.toList
    (((k: K) => vals.getIndex(m1(k))).unlift, (v => vals.getIndex(v)).unlift)

extension [A](as: Iterable[A])
  def allEqual: Boolean = as.lazyZip(as.tail).forall(_ == _)
  def cart[B](bs: Iterable[B]): Iterable[(A, B)] =
    as.flatMap(a => bs.map(b => (a, b)))

extension [A, B](rel: Set[(A, B)])
  def lr: Map[A, Set[B]] = rel.groupBy(_._1).map((k, v) => k -> v.map(_._2))
  def rl: Map[B, Set[A]] = rel.groupBy(_._2).map((k, v) => k -> v.map(_._1))
  
  def getRight(a: A): Set[B] = rel.collect{case (`a`, b) => b}
  def getLeft(b: B): Set[A] = rel.collect{case (a, `b`) => a}

  def indexify: (Set[(Int, Int)], Seq[A], Seq[B]) =
    val (ls, rs) = rel.unzip
    val lss = ls.toSeq
    val rss = rs.toSeq
    (rel.map((a, b) => (lss.indexOf(a), rss.indexOf(b))), lss, rss)

extension (rel: Set[(Int, Int)])
  def deindexify[A, B](as: Seq[A], bs: Seq[B]): Set[(A, B)] =
    rel.map((l, r) => as(l) -> bs(r))

extension [K, V](m1: mutable.Map[K, V])
  def merge(m2: IterableOnce[(K, V)])(dedup: (V, V) => V): Unit =
    for (k, v) <- m2 do m1.get(k) match
      case Some(o) => m1(k) = dedup(o, v)
      case None => m1(k) = v

extension [K, V](m1: mutable.Map[K, Set[V]])
  def touch(k: K): Unit =
    m1.updateWith(k){case None => Some(Set()); case v => v}
  
  def addValue(k: K, v: V): Unit =
    m1.updateWith(k){case None => Some(Set(v)); case Some(vs) => Some(vs + v)}

  def removeValue(k: K, v: V): Unit =
    m1.updateWith(k){case None => None; case Some(vs) => Some(vs - v)}

def indexedToSeq[A](m: Map[Int, A]): Seq[A] = Iterator
  .from(0)
  .takeWhile(m.contains)
  .foldRight(Seq.empty)((i, as) => m(i) +: as)

def indexMap[A, B](m: Map[Int, Int])(bs: Seq[B]): Seq[A] => Seq[B] =
  as => as.indices.map(i => bs(m(i)))

def keyMatch[K, V1, V2](m1: Map[K, V1], m2: Map[K, V2]): Map[V1, V2] =
  (m1.keySet intersect m2.keySet).map(k => m1(k) -> m2(k)).toMap

def canContain(as: List[Int], bs: List[Int]): Boolean = bs.isEmpty || bs.size <= as.size &&
  LazyList.fill(as.length)(bs).transpose.map(as zip _).exists(_.forall(_ >= _))

def possible_mappings[A, B](cs: Iterable[A], ts: Set[B]): Iterator[Map[A, B]] =
  if cs.size <= ts.size then
    for s <- ts.subsets(cs.size)
        p <- s.toList.permutations
    yield cs.zip(p).toMap
  else
    for s <- cs.toSet.subsets(ts.size)
        p <- s.toList.permutations
    yield p.zip(ts).toMap

def possible_partial_mappings[A, B](cs: Set[A], ts: Iterable[B]): Iterator[Map[A, B]] =
  for s <- cs.subsets(ts.size)
      p <- s.toList.permutations
  yield p.zip(ts).toMap

extension[A](xs: Seq[A])
  inline def getIndex[B >: A](inline x: B): Option[Int] = xs.indexOf(x) match
    case -1 => None
    case n => Some(n)

  inline def getIndexWhere(inline p: A => Boolean): Option[Int] = xs.indexWhere(p) match
    case -1 => None
    case n => Some(n)

extension[A](a: Set[A])
  def <=(b: Set[A]): Boolean = a.forall(b.contains)
  def <(b: Set[A]): Boolean = a.size < b.size && a <= b

def mergeFix[A](items: IterableOnce[A], merge: PartialFunction[(A, A), A]): Set[A] =
  val pool = mutable.Set.from(items)
  var i = 10_000
  while pool.exists(x => pool.exists(y => {
    if x == y then false
    else (x, y) match
      case merge(r) =>
        pool -= x
        pool -= y
        pool += r
        true
      case _ => false
  })) && i > 0 do i -= 1
  pool.toSet
