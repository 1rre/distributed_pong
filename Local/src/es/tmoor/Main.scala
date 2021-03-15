package es.tmoor

import sys.process._
import language.postfixOps
import java.io.{OutputStream,InputStream}

object Main extends App {
   // Ensure the port mapper daemon is running
  val windows = sys.props.get("os.name") != Some("Linux") // Sorry Mac users haha

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
      writeData.close
      readData.close
      nios2.destroy
    }
  })
  
  // While we are still connected to both the server & board
  while (nios2.isAlive) {
    // If we have output waiting from the board, read it & handle it accordingly
    if (readData.available > 0) {
      val data = readData.read
      println(data)
      // whatever else you wanna do
    }

  }

}
