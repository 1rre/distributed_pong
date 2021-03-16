
import game._
import Monitor._

object Main extends App {
  var diff = (256 / Monitor.game.rows)
  val ctrl = new Controller
  Monitor

  while (true) {
    player2() = {
      if (0 to 255 contains player2.x - diff) player2.x - diff
      else {
        diff *= -1
        player2.x
      } 
    }
    Thread.sleep(100)
    print(game)
  }

}


