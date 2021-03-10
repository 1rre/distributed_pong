package es.tmoor

import com.ericsson.otp.erlang._

object OtpHelpers {
  case class Atom(s: String)
  implicit class `?OtpNode`(args: Tuple2[String, String]) {
    def node = new OtpNode(args._1, args._2)
  }
  implicit class `?OtpMbox`(mbox: OtpMbox) {
    def get[T <: OtpErlangObject] = mbox.receive.asInstanceOf[T]
  }
  implicit class `?OtpErlangInt`(obj: Int) {
    def eInt = new OtpErlangInt(obj)
  }
  implicit class `?OtpErlangDouble`(obj: Double) {
    def eDbl = new OtpErlangDouble(obj)
  }
  implicit class `?OtpErlangString`(obj: String) {
    def eBin = new OtpErlangBinary(obj)
    def eLst = new OtpErlangList(obj)
    def eStr = new OtpErlangString(obj)
    def eAtm = new OtpErlangAtom(obj)
    def node = new OtpNode(obj)
  }
  implicit class `?OtpErlangAtom`(obj: Atom) {
    def eAtm = new OtpErlangAtom(obj.s)
  }
  implicit class `?OtpErlang(List|Tuple)`(obj: Array[_]) {
    def eLst = new OtpErlangList(obj.map(_.eAuto))
    def eTpl = new OtpErlangTuple(obj.map(_.eAuto))
  }
  implicit class `?OtpErlang(Tuple|List)`(obj: Product) {
    def eTpl = new OtpErlangTuple(obj.productIterator.map(_.eAuto).toArray)
    def eLst = new OtpErlangList(obj.productIterator.map(_.eAuto).toArray)
  }
  implicit class `?OtpErlangObject`(obj: Any) {
    def eAuto: OtpErlangObject = obj match {
      case i: Int => i.eInt
      case d: Double => d.eDbl
      case s: String => s.eStr
      case s: Atom => s.eAtm
      case a: Array[_] => a.eLst
      case t: Product => t.eTpl
      case o: OtpErlangObject => o
      case _ => sys.error("Type not found")
    }
  }
}