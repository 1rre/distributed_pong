package connection

import com.ericsson.otp.erlang._
import OtpHelpers._
import util.Random.alphanumeric
import language.postfixOps

object Node {
  def test = {
    val ip = "tm"
    val uid = alphanumeric take 16 mkString
    val local = new OtpSelf(s"$uid@$ip","let's play pong")
    val remote = new OtpPeer(s"pong@$ip")
    val connection = local connect remote

    println(connection isAlive)

    val msg = (a"register", local pid, local node) toOTP

    connection send ("pong_server", msg)

    for (_ <- 1 to 10) println(connection receive)
    
  }
}