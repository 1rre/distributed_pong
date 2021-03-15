package game

import sys.process._
import language.postfixOps
import collection.mutable.ListBuffer

object Monitor {
  Runtime.getRuntime.addShutdownHook(new Thread {override def run = print("\u001b[?25h")})
  val game = new Game


  class Game {
    print("\u001b[?25l")
    case class Player(num: Int) {
      var x: Int = 127
      val y: Int = if (num == 1) 0 else (cols-1)
      def chars = ((x-4) * rows / 255d).toInt to ((x+4) * rows / 255d).toInt
      def update(newX: Int) = {
        chars foreach {
          case n if 0 until rows contains n => buffer(n)(y) = ' '
          case _ =>
        }
        x = if (newX > 255 || newX < 0) x else newX
        chars foreach {
          case n if 0 until rows contains n => buffer(n)(y) = '#'
          case _ =>
        }
      }
    }

    val players = ListBuffer[Player]()
    lazy val cols = "bash -c 'tput cols'".!!.trim.toInt
    lazy val rows = "bash -c 'tput lines'".!!.trim.toInt
    private val buffer = Array.fill(rows, cols)(' ')
    override def toString = s"\u001b[3J${buffer.map(_.mkString).mkString("\n")}\u001b[0;0H"
  }

  val player1 = new game.Player(1)
  val player2 = new game.Player(2)
  game.players ++= Seq(player1,player2)

}