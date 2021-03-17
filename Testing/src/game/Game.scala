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
      case _ => 127
    }
    
    var y = num match {
      case 3 => 0
      case 4 => 255
      case _ => 127
    }

    def chars = num match {
      case 1 => ((y-paddleSize/2) * rows / 255d).ceil.toInt to ((y+paddleSize/2) * rows / 255)
      case 2 => ((255-paddleSize/2-y) * rows / 255d).ceil.toInt to ((255+paddleSize/2-y) * rows / 255)
      case 3 => ((x-paddleSize/2) * cols / 255d).ceil.toInt to ((x+paddleSize/2) * cols / 255)
      case 4 => ((255-paddleSize/2-x) * cols / 255d).ceil.toInt to ((255+paddleSize/2-x) * cols / 255)
    }

    def apply(n: Int): Unit = {
      chars foreach {
        case n if num <= 2 && (1 until rows-1 contains n) => buffer(n)((x*cols)/256) = ' '
        case n if num >= 3 && (1 until cols-1 contains n) => buffer((y*rows)/256)(n) = ' '
        case _ =>
      }
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
      chars foreach {
        case n if num <= 2 && (1 until rows-1 contains n) => buffer(n)((x*cols)/256) = '#'
        case n if num >= 3 && (1 until cols-1 contains n) => buffer((y*rows)/256)(n) = '#'
        case _ =>
      }
    }

    def apply(obj: OtpErlangObject): Unit = {
      obj match {
        case _: OtpErlangAtom => num match {
          case 1 => buffer foreach (_(0) = '#')
          case 2 => buffer foreach (_(cols-1) = '#')
          case 3 => buffer(0) mapInPlace (_ => '#')
          case 4 => buffer(rows-1) mapInPlace (_ => '#')
        }
        case f: OtpErlangLong => {
          this(f intValue)
        }
      }
    }
  }

  object Ball {
    var x = 127.5
    var y = 127.5
    def apply(newX: Double, newY: Double) = {
      //buffer mapInPlace (_ mapInPlace(_ => ' '))
      buffer((x * rows / 256).round toInt)((y * cols / 256).round toInt) = ' '
      x = if (((newX * rows / 256).round toInt) >= rows) rows - 1 else newX
      y = if (((newY * cols / 256).round toInt) >= cols) cols - 1 else newY
      buffer((x * rows / 256).round toInt)((y * cols / 256).round toInt) = 'O'
    }
  }

  val players = Array.tabulate(4)(n => Player(n+1))
  val cols = "bash -c 'tput cols'".!!.trim toInt
  val rows = "bash -c 'tput lines'".!!.trim toInt
  private val buffer = Array.fill(rows, cols)(' ')
  override def toString = s"\u001b[3J${buffer map (_ mkString) mkString "\n"}\u001b[0;0H"
}
