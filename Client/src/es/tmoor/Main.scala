package es.tmoor

import OtpHelpers._
import com.ericsson.otp.erlang._
import sys.process._
import language.postfixOps
import java.io.{OutputStream,InputStream}
import scala.collection.mutable.ListBuffer

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
  println(connection.waitFor[OtpErlangBinary].mkString)

  var writeData = OutputStream.nullOutputStream
  var readData = InputStream.nullInputStream
  def nios2Input = (in: OutputStream) => writeData = in
  def nios2Output: InputStream=>Unit = (out: InputStream) => readData = out
  val nios2Io = new ProcessIO(nios2Input,nios2Output,(err: InputStream) => err.close)

  val nios2cmd = if (windows) "nios2-terminal.exe" else "nios2-terminal"
  val nios2 = s"$nios2cmd -q --persistent --no-quit-on-ctrl-d".run(nios2Io)
  
  while (connection.isAlive && nios2.isAlive) {
    if (readData.available > 0) {
      val data = readData.read
      if (data == 'a') connection.send("infoproc_cloud",(self.node.eAtm,(a"led",a"on")).eAuto)
      else if (data == 'b') connection.send("infoproc_cloud",(self.node.eAtm,(a"led",a"off")).eAuto)
    } else if (connection.msgCount > 0) {
      connection.receive match {
        case msg: OtpErlangBinary => println(msg.mkString)
        case tpl: OtpErlangTuple => tpl.elementAt(1) match {
          case msg: OtpErlangAtom if (msg.atomValue == "on") => writeData.write('a')
          case msg: OtpErlangAtom if (msg.atomValue == "off") => writeData.write('b')
          case _ =>
        }
        case _ =>
      }
      writeData.flush
    }
  }
  println("Connection to board or server dropped")
  println(nios2.isAlive)
  println(connection.isAlive)
  nios2.destroy
  connection.close
  self.unPublishPort

}
