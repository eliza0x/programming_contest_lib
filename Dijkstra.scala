object Main extends App {
    val scan = new java.util.Scanner(System.in)
    val n = scan.next().toInt
    
    val graph: Dijkstra.Graph = Array.fill(n)(List())

    for (_ <- 1 to n) {
      val u, k = scan.next().toInt
      graph(u) = (1 to k).toList
                .map(_ => Dijkstra.Edge(scan.next().toInt, scan.next().toInt))
    }

    Dijkstra.shortest_path(graph, 0,n-1).zip(0 until n)
      .map(t => t._2.toString ++ " " ++ t._1.get.toString)
      .foreach(println(_))
}

object Dijkstra {
    case class Edge(to: Int, cost: Long)
    type Edges = List[Edge]
    type Graph = Array[Edges]
    
    def shortest_path(graph: Graph, start: Int, goal: Int): Array[Option[Long]] = {
        val pq = new collection.mutable.PriorityQueue[(Long, Int)]
        val cost: Array[Option[Long]] = Array.fill(graph.length)(None)
        cost(start) = Some(0L)
        pq += ((cost(start).get, start))
        while (pq.nonEmpty) {
            val current_edge = pq.dequeue()
            // if (current_edge._2 < current_edge._1) continue
            if (!cost(current_edge._2).map(_ < current_edge._1).getOrElse(false)){
                for (tmp <- graph(current_edge._2)) {
                    val sumCost = current_edge._1 + tmp.cost
                    if (cost(tmp.to).map(_ > sumCost).getOrElse(true)) {
                        cost(tmp.to) = Some(sumCost)
                        pq += ((cost(tmp.to).getOrElse(Long.MaxValue), tmp.to))
                    }
                }
            }
        }
        cost
    }
}
