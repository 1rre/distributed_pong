
import sys.process._
import language.postfixOps
import collection.mutable.ListBuffer


object Main extends App {
  Runtime.getRuntime.addShutdownHook(new Thread {override def run = print("\u001b[?25h")})
  val game = new Game


  class Game {
    print("\u001b[?25l")
    case class Player(num: Int) {
      var x = if (num == 1) 0 else rows-1
      val y = if (num == 1) 0 else cols-1
      def update(newX: Int) = {
        if (0 until rows contains x-1) buffer(x-1)(y) = ' '
        if (0 until rows contains x) buffer(x)(y) = ' '
        if (0 until rows contains x+1) buffer(x+1)(y) = ' '
        x = newX
        if (0 until rows contains x-1) buffer(x-1)(y) = '#'
        if (0 until rows contains x) buffer(x)(y) = '#'
        if (0 until rows contains x+1) buffer(x+1)(y) = '#'
      }
    }

    val players = ListBuffer[Player]()
    private var x = 0;
    lazy val cols = "bash -c 'tput cols'".!!.trim.toInt
    lazy val rows = "bash -c 'tput lines'".!!.trim.toInt
    private val buffer = Array.fill(rows, cols)(' ')
    override def toString = s"\u001b[3J${buffer.map(_.mkString).mkString("\n")}\u001b[0;0H"
  }

  val player1 = new game.Player(1)
  val player2 = new game.Player(2)
  game.players ++= Seq(player1,player2)

  while (true) {
    player1() = (player1.x + 1) % game.rows
    player2() = if (player2.x == 0) game.rows - 1 else player2.x - 1
    Thread.sleep(100)
    print(game)
  }
  

}


