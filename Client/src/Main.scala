
import game.Game
import connection.{OtpHelpers,Node}
import OtpHelpers._
import nios.Nios2Interface
import com.ericsson.otp.erlang._
import language.postfixOps
import sys.process._

object Main extends App {
  // Typedefs because those types are long haha
  type ErlTuple = OtpErlangTuple
  type ErlFloat = OtpErlangDouble
  type ErlInt = OtpErlangLong
  type Atom = OtpErlangAtom

  // Make a new game & connection to the nios2 board
  val game = new Game
  val nios = new Nios2Interface
  val node = new Node

  // Align the input so that we're reading the data & not the newlines
  // This should be do-able as a while loop but when I tried it didn't work
  def alignInput: Unit =
    if (!nios.isAlive) sys error "Nios2 didn't start"
    else if (!nios.ready) alignInput
    else if (nios.read != 10) alignInput

  alignInput
  // While we are still connected to both the board & the server 
  while (node.isAlive && nios.isAlive) {
    if (node.hasMsg) {
      val msg = node.getTupleMsg
      // Check the type of message & resolve it accordingly
      msg match {
        // 'ok' as the 1st element could only be a frame to print 
        case List(ok: Atom, newState: ErlTuple) if ok.atomValue == "ok" =>
          newState toList match {
            case List(ball: ErlTuple, players: ErlTuple) => {
              ball toList match {
                case List(x: ErlFloat, y: ErlFloat) =>
                  // Update the ball positions to those included in the message
                  game.Ball(x.doubleValue,y.doubleValue)
                // This is to avoid crashes: "do nothing if it doesn't match"
                case _ =>
              }
              // Update each of the players to the positions sent in the message
              players.toList.zip(game.players).map(x => x._2(x._1))
            }
            case _ =>
          }
        // 'new_score' as the 1st element means we have a new score to display
        case List(new_score: Atom, erlScore: ErlInt) if new_score.atomValue == "new_score" =>
          val score = Array[Byte]('\u001b','g',erlScore.byteValue)
          nios write score
        case List(speed: Atom, newSpeed: ErlFloat) if speed.atomValue == "speed" =>
          val speed = Array[Byte]('\u001b','s',newSpeed.doubleValue.toByte)
          println(speed.toList)
          nios write speed
        case other => println(other)
      }
      // Print the current game state using to overloaded 'toString'
      print(game)
    } else if (nios ready) {
      // Read 1 byte & ignore another
      val newPos = nios.read; nios.skip
      // Send a message saying 'change_pos', our process id and the new position to 'pong_server'
      node.send(a"change_pos", node pid, newPos)
    }
  }

}


