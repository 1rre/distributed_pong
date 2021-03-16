package game

import sys.process._
import language.postfixOps
import collection.mutable.ListBuffer
import com.ericsson.otp.erlang._

class Game {
  val paddleSize = 25
  print("\u001b[?25l")
  case class Player(num: Int) {
    var x = num match {
      case 1 => 0
      case 2 => 255
      case 3 => 127
      case 4 => 127
    }
    
    var y = num match {
      case 1 => 127
      case 2 => 127
      case 3 => 0
      case 4 => 255
    }

    def chars = num match {
      case 1 => ((y-paddleSize/2) * rows / 255d).ceil.toInt to ((y+paddleSize/2) * rows / 255d).toInt
      case 2 => ((255-paddleSize/2-y) * rows / 255d).ceil.toInt to ((255+paddleSize/2-y) * rows / 255d).toInt
      case 3 => ((x-paddleSize/2) * cols / 255d).ceil.toInt to ((x+paddleSize/2) * cols / 255d).toInt
      case 4 => ((255-paddleSize/2-x) * cols / 255d).ceil.toInt to ((255-paddleSize/2-x) * cols / 255d).toInt
    }

    def apply(n: Int): Unit = {
      x = num match {
        case 1 => 0
        case 2 => 255
        case 3 => n
        case 4 => (255 - n)
      }
      y = num match {
        case 1 => n
        case 2 => (255 - n)
        case 3 => 0
        case 4 => 255
      }
      chars.foreach {
        case n if (1 until rows-1 contains n) && num <= 2 => buffer(n)((x*cols)/256) = '#'
        case n if (1 until cols-1 contains n) => buffer((y*rows)/256)(n) = '#'
        case _ =>
      }
    }

    def apply(obj: OtpErlangObject): Unit = {
      obj match {
        case _: OtpErlangAtom => num match {
          case 1 => buffer.foreach(_(0) = '#')
          case 2 => buffer.foreach(_(cols-1) = '#')
          case 3 => buffer(0).mapInPlace(_ => '#')
          case 4 => buffer(rows-1).mapInPlace(_ => '#')
        }
        case f: OtpErlangLong => {
          /*
          This is less resource intensive than updating in `Ball` but meant any glitches stayed on the screen & it really annoyed me
          num match {
            case 1 => buffer.foreach(_(0) = ' ')
            case 2 => buffer.foreach(_(cols-1) = ' ')
            case 3 => buffer(0).mapInPlace(_ => ' ')
            case 4 => buffer(rows-1).mapInPlace(_ => ' ')
          }
          */
          this(f.intValue)
        }
      }
    }
  }

  object Ball {
    var x = 127.5
    var y = 127.5
    def apply(newX: Double, newY: Double) = {
      buffer.mapInPlace(_.mapInPlace(_ => ' '))
      x = newX
      y = newY
      buffer((x * rows / 255).toInt)((y * cols / 255).toInt) = 'O'
    }
  }

  val players = Array.tabulate(4)(n => Player(n+1))
  val cols = "bash -c 'tput cols'".!!.trim.toInt
  val rows = "bash -c 'tput lines'".!!.trim.toInt
  private val buffer = Array.fill(rows, cols)(' ')
  override def toString = s"\u001b[3J${buffer.map(_.mkString).mkString("\n")}\u001b[0;0H"
}
