object Main extends App {
    val scan = new java.util.Scanner(System.in)
    val n = scan.next().toInt
    
    val graph: Kruscal.Graph = Array.fill(n)(List())

    for (_ <- 1 to n) {
      val u, k = scan.next().toInt
      graph(u) = (1 to k).toList
                .map(_ => Kruscal.Edge(scan.next().toInt, scan.next().toInt))
    }

    Kruscal.minimum_spanning_tree(graph).zip(0 until n)
      .zip(0 until graph.length)
      .foreach(println(_))
}

object Kruscal {
    case class Edge(to: Int, cost: Long)
    type Edges = List[Edge]
    type Graph = Array[Edges]
    
    // 0-indexed
    def minimum_spanning_tree(graph: Graph): Graph = {
        // cost, from, to
        var pq = List[(Long, Int, Int)]()
        for ((from, edges) <- (0 until graph.length).zip(graph); Edge(to, cost) <- edges)
          pq = (cost, from, to) :: pq

        val unionFind = new Array[Int](graph.length)
        for (i <- 0 until graph.length) unionFind(i) = i
        val mst = new Array[Edges](graph.length)
        for ((cost, from, to) <- pq.sorted; if find(from) != find(to)) {
            mst(from) = Edge(to,   cost) :: mst(from)
            mst(to)   = Edge(from, cost) :: mst(to)
            union(from, to)
        }
        def find(i: Int): Int = {
            if (unionFind(i) == i) i
              else {
                val r = find(unionFind(i))
                unionFind(i) = r
              r
            }
        }

        def union(i: Int, l: Int): Unit = {
            val i_root = find(i)
            val l_root = find(l)
            unionFind(i_root) = math.min(i_root, l_root)
            unionFind(l_root) = math.min(i_root, l_root)
        }

        mst
    }
}
