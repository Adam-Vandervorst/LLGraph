package DNIELMGTest

import munit.FunSuite

import be.adamv.llgraph.*
import be.adamv.llgraph.conversions.split
import be.adamv.llgraph.graphs.DNIELMG.DNIELMG

import DNIELGTest.{BasicExample, RewritingExample}


object BasicMGExample:
  val g1 = DNIELMG[String]()
  val g2 = DNIELMG[String]()
  val a: g1.Node = g1.newNode()
  val b: g1.Node = g1.newNode()
  val c: g2.Node = g2.newNode()
  g2.addNodes(List(a, b))
  g1.addNodes(List(c))

  g1.connect(a, a, "aaX")
  g1.connect(a, a, "aaY")
  g1.connect(a, a, "aaZ")
  g2.foreignConnect(a, a, "aaX")
  g1.connect(a, b, "ab1")
  g2.foreignConnect(a, b, "ab2")

  g1.foreignConnect(b, c, "bc1")
  g2.foreignConnect(c, b, "cb2X")
  g2.foreignConnect(c, b, "cb2Y")


object StolenRewritingExample:
  export RewritingExample.{n, m}
  val c1 = RewritingExample.c1.split(Set(_))
  val m1 = RewritingExample.m1.split(Set(_))
  val w1 = RewritingExample.w1.split(Set(_))
  val j1 = RewritingExample.j1.split(Set(_))
  val g1 = RewritingExample.g1.split(Set(_))

object StolenBasicExample:
  export BasicExample.{a, b, c}
  val g1 = BasicExample.g1.split(Set(_))
  val g2 = BasicExample.g2.split(Set(_))

object SimpleFullRewritingExample:
  val c1 = DNIELMG[Char]()
  val m1 = DNIELMG[Char]()
  val w1 = DNIELMG[Char]()
  val j1 = DNIELMG[Char]()
  val g1 = DNIELMG[Char]()

  val n = g1.newNodes(3)

  g1.connect(n(2), n(2), 'e')
  c1.addNodes(Seq(n(0)))
  c1.foreignConnect(n(2), n(2), 'e')

  g1.connect(n(0), n(1), 'b')
  g1.connect(n(0), n(1), 'c')
  g1.connect(n(1), n(2), 'd')
  j1.foreignConnect(n(0), n(1), 'b')
  j1.foreignConnect(n(0), n(1), 'c')
  j1.foreignConnect(n(1), n(2), 'd')

  g1.connect(n(1), n(1), 'a')
  m1.foreignConnect(n(1), n(1), 'a')
  val m = w1.newNodes(1)
  w1.connect(m(0), m(0), 'a')

object FullRewritingExample:
  val c1 = DNIELMG[Char]()
  val m1 = DNIELMG[Char]()
  val w1 = DNIELMG[Char]()
  val j1 = DNIELMG[Char]()
  val g1 = DNIELMG[Char]()

  val n = g1.newNodes(7)

  g1.connect(n(4), n(6), 'c')
  c1.addNodes(Seq(n(2)))
  c1.foreignConnect(n(4), n(6), 'c')

  g1.connect(n(3), n(2), 'b')
  g1.connect(n(4), n(3), 'b')
  g1.connect(n(4), n(5), 'd')
  g1.connect(n(5), n(1), 'b')
  g1.connect(n(6), n(5), 'b')
  j1.foreignConnect(n(3), n(2), 'b')
  j1.foreignConnect(n(4), n(3), 'b')
  j1.foreignConnect(n(4), n(5), 'd')
  j1.foreignConnect(n(5), n(1), 'b')
  j1.foreignConnect(n(6), n(5), 'd')

  g1.connect(n(0), n(1), 'b')
  g1.connect(n(0), n(3), 'a')
  g1.connect(n(5), n(0), 'a')
  m1.foreignConnect(n(0), n(1), 'b')
  m1.foreignConnect(n(0), n(3), 'a')
  m1.foreignConnect(n(5), n(0), 'a')

  val m = w1.newNodes(4)
  w1.connect(m(0), m(1), 'b')
  w1.connect(m(0), m(2), 'a')
  w1.connect(m(3), m(0), 'a')


class DNIELMGTest extends FunSuite {
  test("nodeid union") {
    import StolenBasicExample.*

    assert(g1.labeling(a, a) == Set("aa"))
    assert(g2.labeling(a, a) == Set("aa"))

    assert(g1.labeling(a, b) == Set("ab1"))
    assert(g2.labeling(a, b) == Set("ab2"))
    assert(g1.labeling(b, c) == Set("bc1"))
    assert(g2.labeling(c, b) == Set("cb2"))

    val g3 = g1 union g2
    assert(g3.labeling(a, a) == Set("aa"))
    assert(g3.labeling(a, b) == Set("ab1", "ab2"))
    assert(g3.labeling(b, c) == Set("bc1"))
    assert(g3.labeling(c, b) == Set("cb2"))  }
}

class DNIELMGRewritingTest extends FunSuite {
  test("union adherenceMap") {
    import StolenRewritingExample.*
    assert(g1 == (c1 union j1 union m1))
  }

  test("union patchCompose") {
    import StolenRewritingExample.*
    assert(g1 == (c1 union j1 union m1))
  }
}

class DNIELMGSubgraphIsomorphismTest extends FunSuite {
  test("occurrences") {
    {
      import SimpleFullRewritingExample.*
      val (nmap1, lmap1)::(nmap2, lmap2)::Nil = g1.occurrencesNaive(w1).toList
      assert(Set(nmap1, nmap2) == Set(Map(m(0) -> n(1)), Map(m(0) -> n(2))))
      assert(Set(lmap1, lmap2) == Set(Map('a' -> 'a'), Map('a' -> 'e')))
    }
    {
      import StolenRewritingExample.*
      val (nmap, lmap)::Nil = g1.occurrencesNaive(w1).toList
      assert(nmap == Map(m(2) -> n(4), m(3) -> n(5), m(0) -> n(2), m(1) -> n(3)))
      assert(lmap.forall(_ == _))
    }
  }

  test("labeledOccurrences select labeledIsomorphism") {
    {
      import SimpleFullRewritingExample.*
      val oc::Nil = g1.labeledOccurrencesNaive(w1).map(mapping => g1.select(w1, (mapping, identity))).toList
      assert(oc.labeledIsomorphismNaive(w1).nonEmpty)
    }
    {
      import StolenRewritingExample.*
      val oc::Nil = g1.labeledOccurrencesNaive(w1).map(mapping => g1.select(w1, (mapping, identity))).toList
      assert(oc.labeledIsomorphismNaive(w1).nonEmpty)
    }
  }
}
