import munit.FunSuite

import be.adamv.llgraph.graphs.DNIELG.{DNIELG, given}
import be.adamv.llgraph.graphs.DNIELMG.{DNIELMG, given}
import be.adamv.llgraph.graphs.DVIG.{DVIG, given, *}
import be.adamv.llgraph.{*, given}

import DNIELGTest.BasicExample
import DNIELMGTest.BasicMGExample
import DVIGTest.SmallIntDVIG


class propertiesTest extends FunSuite {
  test("Graph") {
    import SmallIntDVIG.*

    assert(g.at(1) neighbouring 2)

    {
      given DVIG[Int] = g
      assert(1 neighbouring 2)
      for (s, t) <- g.dedgeIt do assert(s connected t)
    }

    assert(g.nodeIt.toSet == g.nodes.toSet)
  }

  test("NIMG") {
    import BasicMGExample.*

    assert((g2.at(c) Nneighbouring b.asInstanceOf) == 2)

    {
      given g1.type = g1
      b Nneighbouring c.asInstanceOf
    }

    assert(g1.nodeIt.toSet == g1.nodes.toSet)
  }
}
