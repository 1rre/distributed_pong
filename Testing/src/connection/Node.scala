package connection

import com.ericsson.otp.erlang._
import OtpHelpers._
import util.Random.alphanumeric
import language.postfixOps

object Node {
  private val ip = "35.242.152.130"
  private val uid = alphanumeric take 16 mkString
  private val local = new OtpSelf(s"$uid@$ip","let's play pong")
  private val remote = new OtpPeer(s"pong@$ip")

  val connection = local connect remote
  val pid = local pid

  connection send("pong_server", (a"register", local pid, local node) toOTP)
}