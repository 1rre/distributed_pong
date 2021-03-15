package game

import java.awt.{event,Frame}
import event.{KeyEvent,KeyListener}
import Monitor._



class Controller extends Frame with KeyListener {
  val diff = (256 / Monitor.game.rows)
  def keyTyped(e: KeyEvent) = {}

  def keyPressed(e: KeyEvent) = e.getKeyCode match {
    case KeyEvent.VK_DOWN => {
      player1() = player1.x + diff
    }
    case KeyEvent.VK_UP => {
      player1() = player1.x - diff
    }
    case _ =>
  }

  def keyReleased(e: KeyEvent) = {}

  addKeyListener(this)
  show
  
}