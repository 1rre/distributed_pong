package es.tmoor

import OtpHelpers._
import com.ericsson.otp.erlang._
import sys.process._
import language.postfixOps

object Main extends App {
  // Ensure the port mapper daemon is running
  val windows = sys.props.get("os.name") != Some("Linux") // Sorry Mac users haha
  val epmd = if (windows) """:\'Program Files'\erl-23.0\erts-11.0\bin\epmd.exe""" else "epmd"
  val exitcode = s"$epmd -names"!
  
  if (exitcode != 0)
    s"$epmd -relaxed_command_check" run

  // Set IP
  // Currently we have to change this if our server IP changes
  // I had an automatic system using oauth but that's more effort to maintain
  val ip = "35.197.249.244"

  // Create a node (the random int is to avoid remote nodes)
  val rand = util.Random.nextInt
  val javaNode = (s"$rand@$ip", "infoproc").node

  // Make sure our node can connect to the server
  if (javaNode.ping(s"remnd@$ip", 10000))
    println("Connection established")
  else
    sys.error("Connection failed")
  
  // Create a mailbox to send & receive messages to/from the server
  val mailbox = javaNode.createMbox(s"infoproc_client")

  // While we have a connection to the server
  while (javaNode.ping(s"remnd@$ip", 10000)) {

    // Get message to send to the board
    val toBoard = io.StdIn.readLine("Enter a message to send to the board, or `exit` to quit ~> ")
    if (toBoard == "exit") {
      println("Exiting")
      mailbox.close
      javaNode.close
      sys.exit
    }

    println(s"Sending $toBoard to the board")

    // Construct & run a command to send the message to the board & receive the result
    val echo = if (windows) s"cmd.exe /c" else ""
    val nios2 = if (windows) "nios2-terminal.exe" else "nios2-terminal"
    val fromBoard = s"$echo echo $toBoard" #|(s"$nios2 -q") !!
   
    // Send the message recieved from the board to the server along with this node's identification
    val toServer = (mailbox.self, fromBoard.init).eAuto
    mailbox.send("infoproc_cloud", s"remnd@$ip", toServer)

    // Receive a binary response from the server & print the result
    val fromServer = mailbox.get[OtpErlangBinary].binaryValue.map(_.toChar).mkString
    println(s"Message from server: $fromServer")

  }

  println("Connection to the remote node dropped, exiting")
}
