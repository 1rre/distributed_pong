package game

import java.awt.{event,Frame}
import event.{KeyEvent,KeyListener}
import connection.OtpHelpers._
import connection.Node._

// For testing only, there is a JWT frame which monitors keypresses

class Controller(game: Game) extends Frame with KeyListener {

  // Take the 1st (left) player as player 1 for convenience
  private def player1 = game.players(0)
  // How much to move with each keypress (with fuzzing)
  private def diff = 16 + util.Random.nextInt(3)-2
  // Do nothing when a key is typed
  def keyTyped(e: KeyEvent) = {}

  // When a key is pressed, check if it is down or up
  def keyPressed(e: KeyEvent) = e.getKeyCode match {
    case KeyEvent.VK_DOWN => {
      // Calculate the movement
      val move = math.min(player1.y+diff,255)
      // Move the player locally
      player1(move)
      // Send the server a message saying to change the position of the player
      connection.send("pong_server",(a"change_pos",pid,move).toOTP)
    }
    case KeyEvent.VK_UP => {
      // Calculate the movement
      val move = math.max(player1.y-diff,0)
      // Move the player locally
      player1(move)
      // Send the server a message saying to change the position of the player
      connection.send("pong_server",(a"change_pos",pid,move).toOTP)
    }
    case _ =>
  }

  // Do nothing when a key is released
  def keyReleased(e: KeyEvent) = {}

  // Enable the key listener & show the frame
  addKeyListener(this)
  show
}