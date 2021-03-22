package connection

import com.ericsson.otp.erlang._
import OtpHelpers._
import util.Random.alphanumeric
import language.postfixOps

object Node {
  // The ip of the server
  private val ip = "35.242.152.130"
  // Take 16 random alphanumeric characters for ournode name
  private val uid = alphanumeric take 16 mkString
  // Create our local node & set the cookie to "let's play pong"
  private val local = new OtpSelf(s"$uid@$ip","let's play pong")
  // Create a representation of the remote node so we can connect to it etc.
  private val remote = new OtpPeer(s"pong@$ip")

  // Connect to the remote node from the local node
  val connection = local connect remote
  
  Runtime.getRuntime.addShutdownHook(new Thread {override def run = {connection.close}})

  // Store the PID so we don't have to get it every time
  val pid = local pid

  // Send a message to the registered name "pong server" through the connection
  // to register as a player with our process ID and local node
  connection send ("pong_server", (a"register", pid, local node) toOTP)
}