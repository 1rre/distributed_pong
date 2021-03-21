package nios

import java.io.{OutputStream,InputStream}
import language.postfixOps
import sys.process._

class Interface {
  val windows = sys.props.get("os.name") != Some("Linux") 
  var writeData = null.asInstanceOf[OutputStream]
  var readData = null.asInstanceOf[InputStream]
  def nios2Input = (in: OutputStream) => writeData = in
  def nios2Output: InputStream=>Unit = (out: InputStream) => readData = out
  val nios2Io = new ProcessIO(nios2Input,nios2Output,(err: InputStream) => err close)
  val nios2cmd = if (windows) "nios2-terminal.exe" else "nios2-terminal"
  val nios2 = s"$nios2cmd -q --persistent --no-quit-on-ctrl-d" run nios2Io
  
  Runtime.getRuntime.addShutdownHook(new Thread {override def run = {writeData.close; readData.close; nios2.destroy}})
}