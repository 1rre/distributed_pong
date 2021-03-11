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

  // See if the IP has been given as a command line argument, if not then use the default
  val ip = args.headOption.getOrElse("35.242.152.130")
  
  // Generate a random string and use it as our ID to connect to the server
  val uid = util.Random.alphanumeric.take(16).mkString
  val self = new OtpSelf(s"$uid@$ip","infoproc")
  val server = new OtpPeer(s"remnd@$ip")
  val connection = self.connect(server)

  // Register ourselves with the server & wait for our confirmation
  val registration = (self.pid,(a"register",self.node.eAtm)).eAuto
  connection.send("infoproc_cloud", registration)
  println(connection.waitFor[OtpErlangBinary].mkString)

  // Set up the input & output streams
  var writeData = null.asInstanceOf[OutputStream]
  var readData = null.asInstanceOf[InputStream]
  def nios2Input = (in: OutputStream) => writeData = in
  def nios2Output: InputStream=>Unit = (out: InputStream) => readData = out
  val nios2Io = new ProcessIO(nios2Input,nios2Output,(err: InputStream) => err.close)
  val nios2cmd = if (windows) "nios2-terminal.exe" else "nios2-terminal"
  val nios2 = s"$nios2cmd -q --persistent --no-quit-on-ctrl-d".run(nios2Io)

  // Add a safe way to shut down following a interrupt etc.
  Runtime.getRuntime.addShutdownHook(new Thread {
    override def run = {
      if (!nios2.isAlive) println("Connection to Nios2 Terminal dropped")
      if (!connection.isAlive) print("Connection to Server dropped")
      connection.close
      writeData.close
      readData.close
      nios2.destroy
    }
  })
  
  // While we are still connected to both the server & board
  while (connection.isAlive && nios2.isAlive) {
    // If we have output waiting from the board, read it & handle it accordingly
    if (readData.available > 0) {
      val data = readData.read
      if (data == 'a') connection.send("infoproc_cloud",(self.node.eAtm,(a"led",a"on")).eAuto)
      else if (data == 'b') connection.send("infoproc_cloud",(self.node.eAtm,(a"led",a"off")).eAuto)
    }
    // If we have messages waiting from the server then recieve them & handle them accordingly
    else if (connection.msgCount > 0) {
      connection.receive match {
        // If it's raw binary data then convert it to a string & print it
        case msg: OtpErlangBinary => println(msg.mkString)
        // If it's a tuple then check if it's an instruction to turn the LEDs on or off
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

}
