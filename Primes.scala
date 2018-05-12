object Main extends App {
  def primes(upper: Int): List[Int] = {
    val ns = Array.fill(upper)(true)
    for ( i <- 2 until ns.length )
      for ( l <- 2 until ns.length; if i*l < ns.length )
        ns(i*l) = false
    (0 to ns.length).zip(ns).filter(_._2).map(_._1).drop(2).toList
  }
}

