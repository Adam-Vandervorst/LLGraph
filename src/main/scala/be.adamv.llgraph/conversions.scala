package be.adamv.llgraph.conversions

import be.adamv.llgraph.graphs.DNIELG.DNIELG
import be.adamv.llgraph.graphs.DNIELMG.DNIELMG


extension [EL](o: DNIELG[EL])
  def split[EL2](f: EL => Set[EL2]): DNIELMG[EL2] =
    val g = DNIELMG[EL2]()
    for (n, nbs) <- o.outgoing
        (l, b) <- nbs
        l2 <- f(l) do
      g.foreignConnect(n, b, l2)
    g


extension [EL](o: DNIELMG[EL])
  def unify[EL2](f: Set[EL] => EL2): DNIELG[EL2] =
    val g = DNIELG[EL2]()
    for (s, outs) <- o.outgoing; t <- outs do
      g.foreignConnect(s, t, f(o.labeling(s, t)))
    g