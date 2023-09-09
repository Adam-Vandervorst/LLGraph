package DNIELGTest

import munit.FunSuite

import be.adamv.llgraph.graphs.DNIELG.DNIELG


object BasicExample:
  val g1 = DNIELG[String]()
  val g2 = DNIELG[String]()
  val a: g1.Node = g1.newNode()
  val b: g1.Node = g1.newNode()
  val c: g2.Node = g2.newNode()
  g2.addNodes(List(a, b))
  g1.addNodes(List(c))

  g1.connect(a, a, "aa")
  g2.foreignConnect(a, a, "aa")
  g1.connect(a, b, "ab1")
  g2.foreignConnect(a, b, "ab2")

  g1.foreignConnect(b, c, "bc1")
  g2.foreignConnect(c, b, "cb2")

object RewritingExample:
  val c1 = DNIELG[Char]()
  val m1 = DNIELG[Char]()
  val w1 = DNIELG[Char]()
  val j1 = DNIELG[Char]()
  val g1 = DNIELG[Char]()

  val n = g1.newNodes(6)

  g1.connect(n(1), n(0), 'b')
  c1.foreignConnect(n(1), n(0), 'b')

  g1.connect(n(2), n(3), 'b')
  g1.connect(n(3), n(4), 'a')
  g1.connect(n(4), n(5), 'a')
  g1.connect(n(2), n(5), 'c')
  m1.foreignConnect(n(2), n(3), 'b')
  m1.foreignConnect(n(3), n(4), 'a')
  m1.foreignConnect(n(4), n(5), 'a')
  m1.foreignConnect(n(2), n(5), 'c')

  val m = w1.newNodes(4)
  w1.connect(m(0), m(1), 'b')
  w1.connect(m(1), m(2), 'a')
  w1.connect(m(2), m(3), 'a')
  w1.connect(m(0), m(3), 'c')

  g1.connect(n(1), n(2), 'a')
  g1.connect(n(5), n(1), 'b')
  g1.connect(n(3), n(5), 'b')
  g1.connect(n(3), n(4), 'b')
  j1.foreignConnect(n(1), n(2), 'a')
  j1.foreignConnect(n(3), n(5), 'b')
  j1.foreignConnect(n(5), n(1), 'b')
  j1.foreignConnect(n(3), n(4), 'b')

class DNIELGTest extends FunSuite {
  test("nodeid") {
    import BasicExample.*

    assert(g1.label(a, a) == Some("aa"))
    assert(g2.label(a, a) == Some("aa"))
    assert(g1.label(a, b) == Some("ab1"))
    assert(g2.label(a, b) == Some("ab2"))
    assert(g1.label(b, c) == Some("bc1"))
    assert(g2.label(c, b) == Some("cb2"))
  }
}
