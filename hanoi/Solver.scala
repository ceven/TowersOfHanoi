package hanoi

trait Solver extends GameDef {

  def done(g: Game): Boolean = { g.l == firstTower }

  lazy val pathsFromStart: Stream[(Game, List[Move])] = from(Stream((startGame, List())), Set())

  lazy val solution: List[Move] = {
    if (pathsToGoal.isEmpty) List()
    else pathsToGoal.head._2.reverse
  }

  lazy val pathsToGoal: Stream[(Game, List[Move])] = {
    pathsFromStart.filter(x => done(x._1))
  }

  def from(initial: Stream[(Game, List[Move])], explored: Set[Game]): Stream[(Game, List[Move])] = {
    def addExplored(e: Set[Game], n: List[(Game, Move)]): Set[Game] = if (n.isEmpty) e else addExplored(e + n.head._1, n.tail)
    initial match {
      case Stream.Empty => Stream.Empty
      case head #:: tail => {
        val neighs = head._1.neighbors
        val neighsWithHistory = newNeighborsOnly(neighborsWithHistory(head._1, head._2), explored)
        head #:: from(tail #::: neighsWithHistory, addExplored(explored, neighs))
      }
    }
  }

  def neighborsWithHistory(b: Game, history: List[Move]): Stream[(Game, List[Move])] = {
    def streamNeighbors(n: List[(Game, Move)]): Stream[(Game, List[Move])] = {
      if (n.isEmpty) Stream.Empty
      else (n.head._1, n.head._2 :: history) #:: streamNeighbors(n.tail)
    }
    streamNeighbors(b.neighbors)
  }

  def newNeighborsOnly(n: Stream[(Game, List[Move])], explored: Set[Game]): Stream[(Game, List[Move])] = {
    n.filter(k => !explored.contains(k._1))
  }
}