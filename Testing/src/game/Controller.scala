package game

import java.awt.{event,Frame}
import event.{KeyEvent,KeyListener}
import connection.OtpHelpers._
import connection.Node._


class Controller(game: Game) extends Frame with KeyListener {

  private def player1 = game.players(0)
  private def diff = 16 + util.Random.nextInt(3)-2
  def keyTyped(e: KeyEvent) = {}

  def keyPressed(e: KeyEvent) = e.getKeyCode match {
    case KeyEvent.VK_DOWN => {
      val move = math.min(player1.y+diff,255)
      player1(move)
      connection.send("pong_server",(a"change_pos",pid,move).toOTP)
    }
    case KeyEvent.VK_UP => {
      val move = math.max(player1.y-diff,0)
      player1(move)
      connection.send("pong_server",(a"change_pos",pid,move).toOTP)
    }
    case _ =>
  }

  def keyReleased(e: KeyEvent) = {}

  addKeyListener(this)
  show
}