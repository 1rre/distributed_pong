package es.tmoor

import OtpHelpers._
import com.ericsson.otp.erlang._
import sys.process._
import language.postfixOps

object Main extends App {
   // Ensure the port mapper daemon is running
  val windows = sys.props.get("os.name") != Some("Linux") // Sorry Mac users haha
  val epmd = if (windows) """C:\'Program Files'\erl-23.0\erts-11.0\bin\epmd.exe""" else "epmd"
  val exitcode = s"$epmd -names"!
  
  if (exitcode != 0)
    s"$epmd -relaxed_command_check" run
  
  val ip = "35.242.152.130"
  
  val rand = util.Random.nextInt
  val self = new OtpSelf(s"$rand@$ip","infoproc")
  self.publishPort
  val server = new OtpPeer(s"remnd@$ip")

  val connection = self.connect(server)
  
  val registration = (self.pid,(a"register",self.node.eAtm)).eAuto
  connection.send("infoproc_cloud", registration)
  println(connection.waitMsg[OtpErlangBinary].mkString)

  while (connection.isAlive) {
    connection.receive match {
      case str: OtpErlangBinary => println(str.mkString)
      case msg => println(msg)
    }
  }

  self.unPublishPort

}
