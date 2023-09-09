package DVIGTest

import munit.FunSuite

import be.adamv.llgraph.graphs.DVIG.DVIG

object SmallIntDVIG {
  val g = DVIG[Int]()
  g.addNodes(1 to 5)

  g.connect(1, 2)
  g.connect(1, 3)

  g.connect(2, 4)
  g.connect(3, 4)

  g.connect(4, 5)
  g.connect(5, 1)
}

class DNIELGTest extends FunSuite {
  test("valueId") {
    {
      import SmallIntDVIG.*

      assert(g.nodes.exists(_ == 5))
      assert(g.nodes.forall(_ != 0))
    }
  }
}
