package connection

import com.ericsson.otp.erlang._
import reflect.ClassTag

// This is a bunch of stuff to make sending (and receiving) messages either
// Implicit classes essentially add members to a given class, stated in brackets after the name

object OtpHelpers {
  // Convert an Erlang binary to a Java string (it just works)
  implicit class `OtpErlangBinary->String`(bin: OtpErlangBinary) {
    def mkString = bin.binaryValue.map(_.toChar).mkString
  }
  // This isn't used here but idk I use this file in multiple projects so I kinda wanna keep it around
  implicit class `Tuple2->OtpNode`(args: Tuple2[String, String]) {
    def node = new OtpNode(args._1, args._2)
  }
  // Extend the connection class by adding a function to wait for a certain type of message & return any other messages to the queue
  implicit class `?OtpConnection`(conn: OtpConnection) {
    // Just look at that polymorphism <3
    // Basically T is anything which inherits from an Erlang object
    // we want T to be the same T each time it is used so we keep a tag at runtime
    def waitFor[T <: OtpErlangObject : ClassTag]: T = {
      val notAccept = collection.mutable.ListBuffer[OtpMsg]()
      def loop: T = {
        // Receive the next message in the queue
        val msg = conn.receiveMsg
        // Check the type of message 
        msg.getMsg match {
          // If it is the type we want
          case obj: T => {
            // Add each message which was the wrong type to the queue
            notAccept foreach (conn deliver _)
            // Return the object
            obj
          }
          // If it is any other type
          case _ => {
            // Add it to the list of messages of the wrong type
            notAccept += msg
            // loop back to the start
            loop
          }
        }
      }
      // Return the value of loop, ie the message we want
      loop
    }
  }
  // Java into to Erlang int
  implicit class `Int->OtpErlangInt`(obj: Int) {
    def eInt = new OtpErlangInt(obj)
  }
  // Java double to Erlang double
  implicit class `Double->OtpErlangDouble`(obj: Double) {
    def eDbl = new OtpErlangDouble(obj)
  }
  // Java string to any of the types which we may want to convert a string to
  implicit class `String->OtpErlang(Binary|List|String|Atom|Node)`(obj: String) {
    // Bitfield
    def eBin = new OtpErlangBinary(obj)
    // List
    def eLst = new OtpErlangList(obj)
    // String (idk what this does as there are no strings but I may as well include it)
    def eStr = new OtpErlangString(obj)
    // Atom (it's a bit like a global enum & you use them SO MUCH)
    def eAtm = new OtpErlangAtom(obj)
    // Node
    def node = new OtpNode(obj)
  }
  // Java string to Erlang atom or bitfield using string interpolation
  // ie a"..." would make an atom with value '...'
  implicit class `StringContext->OtpErlang(Atom|Binary)`(obj: StringContext) {
    def a(args: Any*) = new OtpErlangAtom(obj.parts.head)
    def b(args: Any*) = new OtpErlangBinary(obj.parts.head.getBytes)
  }
  // Java array to Erlang list or tuple
  implicit class `Array->OtpErlang(List|Tuple)`(obj: Array[_]) {
    def eLst = new OtpErlangList(obj.map(_.toOTP))
    def eTpl = new OtpErlangTuple(obj.map(_.toOTP))
  }
  // Scala tuple to Erlang list or tuple
  implicit class `Product->OtpErlang(Tuple|List)`(obj: Product) {
    def eTpl = new OtpErlangTuple(obj.productIterator.map(_.toOTP).toArray)
    def eLst = new OtpErlangList(obj.productIterator.map(_.toOTP).toArray)
  }
  // Automatic Scala/Java to Erlang, callable on anything
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