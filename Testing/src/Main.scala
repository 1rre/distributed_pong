
import game.{Controller,Game}
import connection.OtpHelpers._
import connection.Node._
import com.ericsson.otp.erlang._
import language.postfixOps
import scala.collection.immutable

object Main extends App {
  type ErlTuple = OtpErlangTuple
  type ErlFloat = OtpErlangDouble
  type Atom = OtpErlangAtom

  Runtime.getRuntime.addShutdownHook(new Thread {override def run = print("\u001b[?25h")})
  val game = new Game

  val controller = new Controller(game)

  while (true) {
    connection.waitFor[ErlTuple].elementAt(1).asInstanceOf[ErlTuple].elements.toList match {
      case List(ball: ErlTuple, players: ErlTuple) => {
        ball.elements.toList match {
          case List(x: ErlFloat, y: ErlFloat) =>
            game.Ball(x.doubleValue,y.doubleValue)
          case other =>
        }
        players.elements.zip(game.players).map(x => x._2(x._1))
      }
      case other =>
    }
    print(game)
  }


}


