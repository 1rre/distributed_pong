
import game.{Controller,Game}
import connection.OtpHelpers._
import connection.Node._
import com.ericsson.otp.erlang._
import language.postfixOps
import scala.collection.immutable

object Main extends App {
  // Typedefs because those types are long haha
  type ErlTuple = OtpErlangTuple
  type ErlFloat = OtpErlangDouble
  type Atom = OtpErlangAtom

  // Make a new game
  val game = new Game

  // (For testing only) make a new controller to control the kame with the keyboard
  val controller = new Controller(game)

  while (true) {
    // Receive a message & decode it
    connection.waitFor[ErlTuple].elementAt(1).asInstanceOf[ErlTuple].elements.toList match {
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
    // Print the buffer to reflect the new game state
    print(game)
  }


}


