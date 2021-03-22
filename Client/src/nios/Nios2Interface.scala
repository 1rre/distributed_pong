package nios

import java.io.{OutputStream,InputStream}
import language.postfixOps
import sys.process._

class Nios2Interface {
  // If os.name is Linux, we're on Linux. Assume Windows otherwise
  // This seems to work for all Linux distros I've checked, while MacOS and Windows have inconsistant names depending on version.
  private val windows = sys.props.get("os.name") != Some("Linux")
  private var writeData = null.asInstanceOf[OutputStream]
  private var readData = null.asInstanceOf[InputStream]
  // Set up input, output and error streams for nios2 terminal
  private val nios2Io = new ProcessIO (
    (in: OutputStream) => writeData = in,
    (out: InputStream) => readData = out,
    (err: InputStream) => err close
  )
  private val nios2cmd = if (windows) "nios2-terminal.exe" else "nios2-terminal"
  // Run the command using the process IO we set up earlier
  private val nios2 = s"$nios2cmd -q --persistent --no-quit-on-ctrl-d" run nios2Io

  // Exported functions
  def isAlive = nios2.isAlive
  def ready = readData.available > 0
  def write(data: Int) = writeData write data
  def read = readData.read
  def skip: Unit = readData.read

  // Close the io streams & SIGKILL the process as part of the JVM shutdown
  Runtime.getRuntime addShutdownHook new Thread {override def run = {writeData close; readData close; nios2 destroy}}
}