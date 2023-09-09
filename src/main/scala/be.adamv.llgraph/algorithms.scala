package be.adamv.llgraph.algorithms


object MetaRewriting:
  object Eager:
    def winStay[A](initial: A, rrs: Seq[A => Iterator[A]]): A =
      var curr = initial
      var i = 0
      var failed = 0
      while failed < rrs.length do
        val it = rrs(i)(curr)
        if it.hasNext then
          curr = it.next()
          failed = 0
        else
          i = (i + 1) % rrs.length
          failed += 1

      curr

    def frequentist[A](initial: A, rrs: Seq[A => Iterator[A]]): A =
      var curr = initial
      var i = 0
      val success_freq = Array.fill(rrs.length)(0)
      val all_possible = collection.immutable.BitSet(rrs.indices*)
      var possible_mask = all_possible
      while true do
        val it = rrs(i)(curr)
        if it.hasNext then
          curr = it.next()
          success_freq(i) += 1
          possible_mask = all_possible
        else
          possible_mask -= i
          if possible_mask.isEmpty then return curr
          else i = possible_mask.maxBy(success_freq)
      throw java.lang.IllegalStateException()

    def ordered[A](initial: A, rrs: Seq[A => Iterator[A]]): LazyList[A] =
      LazyList.unfold(initial)((c: A) => rrs.iterator.flatMap(_(c)).nextOption().map(x => (x, x)))
end MetaRewriting


@main def bench =
  val is = List.tabulate(100)(_ => util.Random.nextInt(100_000))

  val rrs = Array[Int => Iterator[Int]](
    k => if k < 10 then Iterator.empty else Iterator.single(k - 1),
    k => if k > 0 && k % 2 == 0 then Iterator.single(k/2) else Iterator.empty,
    k => Iterator.range(k - 1, 2, -1).filter(_ % k == 0)
  )

  for i <- is do
    MetaRewriting.Eager.frequentist(i, rrs)

  val t = System.nanoTime()
  for _ <- 1 to 20 do
    for i <- is do
      MetaRewriting.Eager.frequentist(i, rrs)
  println(System.nanoTime() - t)

  // 2054157411