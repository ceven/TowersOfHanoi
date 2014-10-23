package hanoi

object Hanoi extends App {

  object level extends Solver {
    val r1: Ring = 1
    val r2: Ring = 2
    val r3: Ring = 3
    val r4: Ring = 4
    val r5: Ring = 5

    val interm = List()
    val last = List()
    val firstTower = List(r1, r2, r3, r4, r5)
  }

  println(level.solution)

}