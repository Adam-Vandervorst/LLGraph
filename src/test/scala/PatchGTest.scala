package PatchGTest

import munit.FunSuite
import be.adamv.llgraph.graphs.DNIELMG.DNIELMG
import be.adamv.llgraph.graphs.PatchG.*
import be.adamv.llgraph.deindexify

import DNIELMGTest.{FullRewritingExample, SimpleFullRewritingExample}


object ExtendedSimpleFullRewritingExample:
  export SimpleFullRewritingExample.*

  val lhs = DNIELMG[Char]()
  val l = lhs.newNodes(1)
  lhs.connect(l(0), l(0), 'a')

  val lpg = PatchG[Char, lhs.type](lhs)(List(C -> l(0), l(0) -> C))

  val rhs = DNIELMG[Char]()
  val r = rhs.newNodes(2)
  rhs.connect(r(0), r(0), 'b')

  val rpg = PatchG[Char, rhs.type](rhs)(List(C -> r(0), r(1) -> C))

  val rr = RewriteRule(lpg, rpg, Set(0 -> 0, 1 -> 1))

  val result = DNIELMG[Char]()
  val o = result.newNodes(4)
  result.connect(o(0), o(1), 'b')
  result.connect(o(0), o(1), 'c')
  result.connect(o(1), o(1), 'b')
  result.connect(o(2), o(3), 'd')
  result.connect(o(3), o(3), 'e')

object PTMatchSimpleRewritingExample:
  val pos = DNIELMG[Char]()
  val pos_n = pos.newNodes(2)
  pos.connect(pos_n(0), pos_n(1), 'a')
  pos.connect(pos_n(1), pos_n(1), 'a')

  val neg = DNIELMG[Char]()
  val neg_n = neg.newNodes(3)
  neg.connect(neg_n(0), neg_n(1), 'a')
  neg.connect(neg_n(1), neg_n(1), 'a')
  neg.connect(neg_n(1), neg_n(2), 'b')

  val lhs = DNIELMG[Char]()
  val l = lhs.newNodes(2)
  lhs.connect(l(0), l(1), 'a')
  lhs.connect(l(1), l(1), 'a')

  val lpg = PatchG[Char, lhs.type](lhs)(List(C -> l(0)))

  val rhs = DNIELMG[Char]()
  val r = rhs.newNodes(1)
  rhs.connect(r(0), r(0), 'a')

  val rpg = PatchG[Char, rhs.type](rhs)(List(C -> r(0)))

  val rr = RewriteRule(lpg, rpg, Set(0 -> 0))

object PTContextSimpleRewritingExample:
  val g1 = DNIELMG[Char]()
  val n = g1.newNodes(3)
  g1.connect(n(0), n(1), 'a')
  g1.connect(n(1), n(2), 'p')
  g1.connect(n(2), n(1), 'q')
  g1.connect(n(2), n(2), 't')

  val lhs = DNIELMG[Char]()
  val l = lhs.newNodes(2)
  lhs.connect(l(0), l(1), 'p')
  lhs.connect(l(1), l(1), 't')

  val lpg = PatchG[Char, lhs.type](lhs)(List(C -> l(0), l(1) -> l(0)))

  val rhs = DNIELMG[Char]()
  val r = rhs.newNodes(2)
  rhs.connect(r(1), r(1), 't')

  val rpg = PatchG[Char, rhs.type](rhs)(List(C -> r(0), r(1) -> r(1)))

  val rr = RewriteRule(lpg, rpg, Set(0 -> 0, 1 -> 1))

  val result = DNIELMG[Char]()
  val o = result.newNodes(3)
  result.connect(o(0), o(1), 'a')
  result.connect(o(2), o(2), 't')
  result.connect(o(2), o(2), 'q')

object PTSelfSimpleRewritingExample:
  val g1 = DNIELMG[Char]()
  val n = g1.newNodes(2)
  g1.connect(n(0), n(0), 'i')
  g1.connect(n(0), n(1), 'a')
  g1.connect(n(1), n(1), 't')

  val lhs = DNIELMG[Char]()
  val l = lhs.newNodes(2)
  lhs.connect(l(0), l(0), 'i')
  lhs.connect(l(0), l(1), 'a')

  val lpg = PatchG[Char, lhs.type](lhs)(List(C -> l(1), l(1) -> C))

  val rhs = DNIELMG[Char]()
  val r = rhs.newNodes(2)
  rhs.connect(r(1), r(1), 't')

  val rpg = PatchG[Char, rhs.type](rhs)(List(C -> r(0), r(1) -> r(1)))

  val rr = RewriteRule(lpg, rpg, Set(0 -> 0, 1 -> 1))

  val result = DNIELMG[Char]()
  val o = result.newNodes(3)
  result.connect(o(0), o(1), 'a')
  result.connect(o(2), o(2), 't')
  result.connect(o(2), o(2), 'q')

object RewriteRuleExample:
  export FullRewritingExample.*

  val lhs = DNIELMG[Char]()
  val l = lhs.newNodes(4)
  lhs.connect(l(0), l(1), 'a')
  lhs.connect(l(1), l(2), 'b')
  lhs.connect(l(1), l(3), 'a')

  val lpg = PatchG[Char, lhs.type](lhs)(List(C -> l(0), l(0) -> l(2), l(2) -> C, C -> l(3), l(3) -> C))

  val rhs = DNIELMG[Char]()
  val r = rhs.newNodes(3)
  rhs.connect(r(0), r(1), 'c')
  rhs.connect(r(1), r(2), 'a')

  val rpg = PatchG[Char, rhs.type](rhs)(List(r(0) -> C, r(1) -> C, C -> r(2), r(2) -> r(0), r(2) -> r(0)))

  val rr = RewriteRule(lpg, rpg, Set(2 -> 0, 3 -> 1, 3 -> 2, 0 -> 3, 1 -> 4))

  val result = DNIELMG[Char]()
  val o = result.newNodes(6)

  result.connect(o(0), o(2), 'c')
  result.connect(o(2), o(3), 'b')
  result.connect(o(2), o(4), 'a')
  result.connect(o(3), o(4), 'b')
  result.connect(o(3), o(5), 'c')
  result.connect(o(4), o(0), 'd')
  result.connect(o(4), o(0), 'b')

object RewriteRuleRelationExample:
  val pos = DNIELMG[Char]()
  val pos0: pos.Node = pos.newNode()
  val pos1: pos.Node = pos.newNode()
  val posx: pos.Node = pos.newNode()
  pos.connect(pos0, pos1, 'a')

  pos.connect(posx, pos0, 'x')
  pos.connect(pos0, pos.newNode(), 't')

  pos.connect(pos.newNode(), pos1, 'u')
  pos.connect(pos1, posx, 'x')

  val lneg = DNIELMG[Char]()
  val lneg0: lneg.Node = lneg.newNode()
  val lneg1: lneg.Node = lneg.newNode()
  val lnegx: lneg.Node = lneg.newNode()
  lneg.connect(lneg0, lneg1, 'a')

  lneg.connect(lnegx, lneg0, 's')
  lneg.connect(lneg0, lneg.newNode(), 't')

  lneg.connect(lneg.newNode(), lneg1, 'u')
  lneg.connect(lneg1, lnegx, 'v')

  val nneg = DNIELMG[Char]()
  val nneg0: nneg.Node = nneg.newNode()
  val nneg1: nneg.Node = nneg.newNode()
  nneg.connect(nneg0, nneg1, 'a')

  nneg.connect(nneg.newNode(), nneg0, 'x')
  nneg.connect(nneg0, nneg.newNode(), 't')

  nneg.connect(nneg.newNode(), nneg1, 'u')
  nneg.connect(nneg1, nneg.newNode(), 'x')

  val lhs = DNIELMG[Char]()
  val l = lhs.newNodes(2)
  lhs.connect(l(0), l(1), 'a')

  val lpg = PatchG[Char, lhs.type](lhs)(List(C -> l(0), l(0) -> C, C -> l(1), l(1) -> C))

  val rhs = DNIELMG[Char]()
  val r = rhs.newNodes(2)
  rhs.connect(r(0), r(1), 'a')

  val rpg = PatchG[Char, rhs.type](rhs)(List(C -> r(0), r(1) -> C, r(0) -> C))

  val rr = RewriteRule(lpg, rpg, Set(0 -> 0, 1 -> 1, 1 -> 2, 3 -> 0))

  val result = DNIELMG[Char]()
  val o = result.newNodes(5)
  result.connect(o(0), o(1), 'x')
  result.connect(o(1), o(2), 'a')
  result.connect(o(1), o(3), 't')
  result.connect(o(2), o(3), 't')

object IntoSelfRewriteExample:
  val g1 = DNIELMG[Char]()
  val n1 = g1.newNodes(2)
  g1.connect(n1(0), n1(1), 'a')
  g1.connect(n1(1), n1(1), 'a')

  val g2 = DNIELMG[Int]()
  val n2 = g2.newNodes(2)
  g2.connect(n2(0), n2(0), 1)
  g2.connect(n2(0), n2(0), 2)
  g2.connect(n2(1), n2(0), 1)
  g2.connect(n2(1), n2(0), 2)

  val g3 = DNIELMG[String]()
  val n3 = g3.newNodes(5)
  g3.connect(n3(0), n3(2), "osa")
  g3.connect(n3(0), n3(2), "isb")
  g3.connect(n3(1), n3(3), "it")
  g3.connect(n3(2), n3(3), "m")
  g3.connect(n3(3), n3(3), "m")
  g3.connect(n3(3), n3(4), "oa")
  g3.connect(n3(3), n3(4), "ob")
  g3.connect(n3(2), n3(4), "oa")
  g3.connect(n3(2), n3(4), "ob")

  val lhs = DNIELMG[Char]()
  val l = lhs.newNodes(2)
  lhs.connect(l(0), l(1), 'x')
  lhs.connect(l(1), l(1), 'x')

  val lpg = PatchG[Char, lhs.type](lhs)(List(l(0) -> l(1), l(1) -> l(1),
    C -> l(0), C -> l(1),
    l(0) -> C, l(1) -> C))

  val rhs = DNIELMG[Char]()
  val r = rhs.newNodes(1)
  rhs.connect(r(0), r(0), 'x')

  val rpg = PatchG[Char, rhs.type](rhs)(List(r(0) -> r(0), C -> r(0), C -> r(0), r(0) -> C))

  val rr = RewriteRule(lpg, rpg, Set(0 -> 0, 1 -> 0, 2 -> 1, 3 -> 2, 4 -> 3, 5 -> 3))


object SameIncomingRewriteExample:
  val neg1 = DNIELMG[Char]()
  val n1 = neg1.newNodes(3)
  neg1.connect(n1(0), n1(1), 'a')
  neg1.connect(n1(0), n1(2), 'b')

  val lhs = DNIELMG[Char]()
  val l = lhs.newNodes(2)

  val lpg = PatchG[Char, lhs.type](lhs)(List(C -> l(0), C -> l(1), l(0) -> l(0), l(1) -> l(1), l(0) -> C, l(1) -> C))

  val rhs = DNIELMG[Char]()
  val r = rhs.newNodes(1)

  val rpg = PatchG[Char, rhs.type](rhs)(List(C -> r(0), r(0) -> r(0), r(0) -> C, r(0) -> C))

  val rr = RewriteRule(lpg, rpg, Set(0 -> 0, 1 -> 0, 2 -> 1, 3 -> 1, 4 -> 2, 5 -> 3))


class PatchGTest extends FunSuite {
  test("applyIt examples") {
    {
      import IntoSelfRewriteExample.*

      assert(rr.applyIt(g1).nonEmpty)
      assert(rr.applyIt(g2).nonEmpty)
      assert(rr.applyIt(g3).nonEmpty)
    }

    {
      import SameIncomingRewriteExample.*

      assert(rr.applyIt(neg1).isEmpty)
    }
  }

  test("applyItLabeled patchTypeMatch") {
    {
      import PTMatchSimpleRewritingExample.*
      assert(rr.applyItLabeled(pos).nonEmpty)
      assert(rr.applyItLabeled(neg).isEmpty)
    }
  }

  test("applyItLabeled patchTypeRelation") {
    import RewriteRuleRelationExample.*

    assert(rr.applyItLabeled(pos).next().labeledIsomorphismNaive(result).nonEmpty)
    assert(rr.applyItLabeled(lneg).isEmpty)
    assert(rr.applyItLabeled(nneg).isEmpty)
  }

  test("applyItLabeled patchContextMatch") {
    {
      import PTContextSimpleRewritingExample.*
      assert(rr.applyItLabeled(g1).next().labeledIsomorphismNaive(result).nonEmpty)
    }
  }

  test("applyItLabeled patchSelfMatch") {
    {
      import PTSelfSimpleRewritingExample.*
      assert(rr.applyItLabeled(g1).isEmpty)
    }
  }

  test("applyItLabeled labeledIsomorphism") {
    {
      import ExtendedSimpleFullRewritingExample.*
      assert(rr.applyItLabeled(g1).next().labeledIsomorphismNaive(result).nonEmpty)
    }
    {
      import RewriteRuleExample.*
      assert(rr.applyItLabeled(g1).next().labeledIsomorphismNaive(result).nonEmpty)
    }
  }
}
