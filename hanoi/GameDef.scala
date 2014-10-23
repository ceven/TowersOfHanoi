package hanoi

trait GameDef {
  /** A Ring is a number, the higher the number the bigger the ring*/
  type Ring = Integer
  type Tower = List[Ring]

  val firstTower: Tower
  val interm: Tower
  val last: Tower
  def startGame: Game = new Game(firstTower, interm, last)

  sealed abstract class Move
  case object FirstToInterm extends Move
  case object FirstToLast extends Move
  case object IntermToFirst extends Move
  case object IntermToLast extends Move
  case object LastToFirst extends Move
  case object LastToInterm extends Move

  case class Game(f: Tower, i: Tower, l: Tower) {

    def moveFromTowerToTower(t: Tuple2[Tower, Tower]): Tuple2[Tower, Tower] = if (!t._1.isEmpty && (t._2.isEmpty || t._1.head < t._2.head)) t.copy(_1 = t._1.tail, _2 = t._1.head :: t._2) else t

    def f12 = {
      val t = moveFromTowerToTower(f, i)
      Game(t._1, t._2, l)
    }
    def f13 = {
      val t = moveFromTowerToTower(f, l)
      Game(t._1, i, t._2)
    }
    def f21 = {
      val t = moveFromTowerToTower(i, f)
      Game(t._2, t._1, l)
    }
    def f23 = {
      val t = moveFromTowerToTower(i, l)
      Game(f, t._1, t._2)
    }
    def f31 = {
      val t = moveFromTowerToTower(l, i)
      Game(t._2, i, t._1)
    }
    def f32 = {
      val t = moveFromTowerToTower(l, i)
      Game(f, t._2, t._1)
    }

    def neighbors: List[(Game, Move)] = List((f12, FirstToInterm), (f13, FirstToLast), (f21, IntermToFirst), (f23, IntermToLast), (f31, LastToFirst), (f32, LastToInterm))

  }
}