package es.tmoor

import com.ericsson.otp.erlang._

object OtpHelpers {
  implicit class `OtpErlangBinary->String`(bin: OtpErlangBinary) {
    def mkString = bin.binaryValue.map(_.toChar).mkString
  }
  implicit class `Tuple2->OtpNode`(args: Tuple2[String, String]) {
    def node = new OtpNode(args._1, args._2)
  }
  implicit class `?OtpConnection`(conn: OtpConnection) {
    def waitMsg[T <: OtpErlangObject] = conn.receive.asInstanceOf[T]
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
  }
  implicit class `Array->OtpErlang(List|Tuple)`(obj: Array[_]) {
    def eLst = new OtpErlangList(obj.map(_.eAuto))
    def eTpl = new OtpErlangTuple(obj.map(_.eAuto))
  }
  implicit class `Product->OtpErlang(Tuple|List)`(obj: Product) {
    def eTpl = new OtpErlangTuple(obj.productIterator.map(_.eAuto).toArray)
    def eLst = new OtpErlangList(obj.productIterator.map(_.eAuto).toArray)
  }
  implicit class `Any->OtpErlangObject`(obj: Any) {
    def eAuto: OtpErlangObject = obj match {
      case i: Int => i.eInt
      case d: Double => d.eDbl
      case s: String => s.eStr
      case a: Array[_] => a.eLst
      case t: Product => t.eTpl
      case o: OtpErlangObject => o
      case _ => sys.error("Type not found")
    }
  }
}