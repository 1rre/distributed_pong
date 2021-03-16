package connection

import com.ericsson.otp.erlang._

object OtpHelpers {
  implicit class `OtpErlangBinary->String`(bin: OtpErlangBinary) {
    def mkString = bin.binaryValue.map(_.toChar).mkString
  }
  implicit class `Tuple2->OtpNode`(args: Tuple2[String, String]) {
    def node = new OtpNode(args._1, args._2)
  }
  implicit class `?OtpConnection`(conn: OtpConnection) {
    def waitFor[T <: OtpErlangObject : Manifest]: T = {
      val notAccept = collection.mutable.ListBuffer[OtpMsg]()
      def loop: T = {
        val msg = conn.receiveMsg
        msg.getMsg match {
          case obj: T => {
            notAccept.foreach(conn.deliver(_))
            obj
          }
          case _ => {
            notAccept += msg
            loop
          }
        }
      }
      loop
    }
  }
  implicit class `Int->OtpErlangInt`(obj: Int) {
    def eInt = new OtpErlangInt(obj)
  }
  implicit class `Double->OtpErlangDouble`(obj: Double) {
    def eDbl = new OtpErlangDouble(obj)
  }
  implicit class `String->OtpErlang(Binary|List|String|Atom|Node)`(obj: String) {
    def eBin = new OtpErlangBinary(obj)
    def eLst = new OtpErlangList(obj)
    def eStr = new OtpErlangString(obj)
    def eAtm = new OtpErlangAtom(obj)
    def node = new OtpNode(obj)
  }
  implicit class `String->OtpErlangAtom`(obj: StringContext) {
    def a(args: Any*) = new OtpErlangAtom(obj.parts.head)
    def b(args: Any*) = new OtpErlangBinary(obj.parts.head.getBytes)
  }
  implicit class `Array->OtpErlang(List|Tuple)`(obj: Array[_]) {
    def eLst = new OtpErlangList(obj.map(_.toOTP))
    def eTpl = new OtpErlangTuple(obj.map(_.toOTP))
  }
  implicit class `Product->OtpErlang(Tuple|List)`(obj: Product) {
    def eTpl = new OtpErlangTuple(obj.productIterator.map(_.toOTP).toArray)
    def eLst = new OtpErlangList(obj.productIterator.map(_.toOTP).toArray)
  }
  implicit class `Any->OtpErlangObject`(obj: Any) {
    def toOTP: OtpErlangObject = obj match {
      case i: Int => i.eInt
      case d: Double => d.eDbl
      case s: String => s.eAtm
      case a: Array[_] => a.eLst
      case t: Product => t.eTpl
      case o: OtpErlangObject => o
      case _ => sys.error("Type not found")
    }
  }
}