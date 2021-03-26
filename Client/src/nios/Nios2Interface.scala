package nios

import java.io.{OutputStream,InputStream}
import language.postfixOps
import sys.process._

object Nios2Interface {
  // If os.name is Linux, we're on Linux. Assume Windows otherwise
  // This seems to work for all Linux distros I've checked, while MacOS and Windows have inconsistant names depending on version.
  val windows = (sys.props get "os.name") != Some("Linux")
  val wsl = (("uname -r"!!) contains "Microsoft")
}

class Nios2Interface {
  import Nios2Interface._
  private var writeData = null.asInstanceOf[OutputStream]
  private var readData = null.asInstanceOf[InputStream]
  // Set up input, output and error streams for nios2 terminal
  private val nios2Io = new ProcessIO (
    (in: OutputStream) => writeData = in,
    (out: InputStream) => readData = out,
    (err: InputStream) => err close
  )
  private val nios2cmd =
    if (windows) "cmd.exe /c nios2-terminal.exe"
    else if (wsl) "/mnt/c/intelFPGA_lite/18.0/quartus/bin64/nios2-terminal.exe"
    else "nios2-terminal"
  // Run the command using the process IO we set up earlier
  private val nios2 = s"$nios2cmd -q --persistent --no-quit-on-ctrl-d" run nios2Io

  // Will there be carriage returns in the input
  private val byteNum = if (windows || wsl) 3 else 2

  // Exported functions
  def isAlive = nios2 isAlive
  def ready = readData.available >= byteNum
  def write(data: Array[Byte]) = {
    writeData write data
    writeData flush
  }
  def read: Int = {
    val data = readData readNBytes byteNum map (0xff & _)
    data filterNot (Seq(10,13) contains _) headOption match {
      case Some(x) => x
      case _ if data.count(_ == 10) >= 2 => 10
      case _ => 13
    }
  }
  def exit = {writeData.close; readData.close; nios2.destroy}

  Runtime.getRuntime addShutdownHook new Thread {override def run = exit}
}