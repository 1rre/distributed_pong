
import game.Game
import connection.OtpHelpers._
import connection.Node._
import nios.Interface
import com.ericsson.otp.erlang._
import language.postfixOps
import scala.collection.immutable

object Main extends App {
  // Typedefs because those types are long haha
  type ErlTuple = OtpErlangTuple
  type ErlFloat = OtpErlangDouble
  type ErlInt = OtpErlangLong
  type Atom = OtpErlangAtom

  // Make a new game
  val game = new Game

  val nios = new Interface

  // Align the input so that we're reading the data & not the newlines
  def alignInput: Unit =
    if (nios.readData.available == 0) alignInput
    else if (nios.readData.read != 10) alignInput

  alignInput
  
  while (connection.isAlive && nios.nios2.isAlive) {
    if (connection.msgCount > 0) {
      val msg = connection.waitFor[ErlTuple]
      msg elementAt 0 match {
        case ok: Atom if ok.atomValue == "ok" =>
          (msg elementAt 1).asInstanceOf[ErlTuple].elements toList match {
            case List(ball: ErlTuple, players: ErlTuple) => {
              ball.elements.toList match {
                case List(x: ErlFloat, y: ErlFloat) =>
                  // Update the ball positions to those included in the message
                  game.Ball(x.doubleValue,y.doubleValue)
                case other =>
              }
              // Update each of the players to the positions sent in the message
              players.elements.zip(game.players).map(x => x._2(x._1))
            }
            case other =>
          }
        case new_score: Atom if new_score.atomValue == "new_score" =>
          val toNios = (msg elementAt 1).asInstanceOf[ErlInt].byteValue
          nios.writeData write toNios
      }
      print(game)
    }
    else if (nios.readData.available >= 2) {
      val read = nios.readData.read;nios.readData.read
      //println(read)
      connection.send("pong_server", (a"change_pos", pid, read) toOTP)
      game.players(0)(read)
      //print(game)
    }
  }

  if (!connection.isAlive) println("Connection Dropped")
  if (!nios.nios2.isAlive) println("Nios2 Dropped")


}


