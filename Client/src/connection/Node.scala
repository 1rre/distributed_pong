package connection

import com.ericsson.otp.erlang._
import OtpHelpers._
import util.Random.alphanumeric
import language.postfixOps

class Node {
  // The ip & registered name of the server
  private val ip = "35.242.152.130"
  private val serverName = "pong_server"
  // Take 16 random alphanumeric characters for ournode name
  private val uid = alphanumeric take 16 mkString
  // Create our local node & set the cookie to "let's play pong"
  private val local = new OtpSelf(s"$uid@$ip","let's play pong")
  // Create a representation of the remote node so we can connect to it etc.
  private val remote = new OtpPeer(s"pong@$ip")


  // Connect to the remote node from the local node
  private val connection = local connect remote
  
  //Runtime.getRuntime addShutdownHook new Thread {override def run = {connection.close}}

  // Store the PID so we don't have to get it every time
  val pid = local pid

  // Exported functions
  def hasMsg = connection.msgCount > 0
  def isAlive = connection.isAlive
  def getTupleMsg = connection.waitFor[OtpErlangTuple].toList
  // This isn't a pointer, it's analagous to `...` in C/C++
  def send(msg: Any*) = connection.send(serverName, msg toOTP)
  def exit = connection.close


  Runtime.getRuntime.addShutdownHook(new Thread {override def run = exit})
  // Send a message to the registered name "pong server" through the connection
  // to register as a player with our process ID and local node
  send(a"register", pid, local node)

}