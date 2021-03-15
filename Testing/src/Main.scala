
import game._
import Monitor._

object Main extends App {
  val diff = (256 / Monitor.game.rows)
  val ctrl = new Controller
  Monitor

  while (true) {
    player2() = if (player2.x - diff < 0) 255 else player2.x - diff
    Thread.sleep(100)
    print(game)
  }

}


