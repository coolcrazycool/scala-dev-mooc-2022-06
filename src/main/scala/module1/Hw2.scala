package module1

import scala.util.Random

object Hw2 extends App {
  class Experiment(val blackCount: Int, val whiteCount: Int) {
    val bucket: Seq[Int] = (1 to blackCount).map(_ => 1) ++ (1 to whiteCount).map(_ => 0)

    def randomBall: Boolean = {
      val randomized = Random.shuffle(bucket)
      randomized match {
        case Seq(0, 1, _, _, _, _) => true
        case _ => false
      }
    }
  }

  val experimentCount = 10000
  val whiteCount = 3
  val blackCount = 3

  val experimentList = for {
    _ <- 1 to experimentCount
    element = new Experiment(blackCount, whiteCount)
  } yield element.randomBall

  // Вероятность P(AB) == 3/10 = 0.3
  println(experimentList.partition(_ == true)._1.length / experimentCount.toDouble)
  println(experimentList.count(_ == true) / experimentCount.toDouble)
  println(experimentList.filter(_ == true).length / experimentCount.toDouble)
  println(experimentList.foldLeft(0)((acc, el) => if (el) acc + 1 else acc) / experimentCount.toDouble)
  // 0.3012
  // 0.3012
  // 0.3012
  // 0.3012

  // Вероятность P(B|A) == 3/5 = 0.6
  println((experimentList.count(_ == true) / experimentCount.toDouble) / (whiteCount.toDouble / (whiteCount + blackCount)))
  // 0.6024
}
