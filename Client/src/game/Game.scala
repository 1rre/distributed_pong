package game

import sys.process._
import language.postfixOps
import collection.mutable.ListBuffer
import com.ericsson.otp.erlang._

class Game {
  // Re-enable the cursor when the game exits
  // Constant paddle size (can be tweaked)
  val paddleSize = 25
  // Hide the cursor
  print("\u001b[?25l")

  // Player class
  case class Player(num: Int) {
    // Set the x position based on the player number
    private var x = num match {
      case 1 => 0
      case 2 => 255
      case _ => 127
    }
    
    // Set the y position based on the player number
    private var y = num match {
      case 3 => 0
      case 4 => 255
      case _ => 127
    }

    // Function to get the current screen range taken up by the player based on the current position & paddle size
    private def chars = num match {
      case 1 => ((y-paddleSize/2) * rows / 255d).ceil.toInt to ((y+paddleSize/2) * rows / 255)
      case 2 => ((255-paddleSize/2-y) * rows / 255d).ceil.toInt to ((255+paddleSize/2-y) * rows / 255)
      case 3 => ((x-paddleSize/2) * cols / 255d).ceil.toInt to ((x+paddleSize/2) * cols / 255)
      case 4 => ((255-paddleSize/2-x) * cols / 255d).ceil.toInt to ((255+paddleSize/2-x) * cols / 255)
    }

    // Override () operator to change the current position when called on an int
    def apply(n: Int): Unit = {
      // Change the previous position of the paddle to spaces (ie clear it) in the buffer
      // This probably needs changing as it leads to graphical glitches
      chars foreach {
        case n if num <= 2 && (1 until rows-1 contains n) => buffer(n)((x*cols)/255) = ' '
        case n if num >= 3 && (1 until cols-1 contains n) => buffer((y*rows)/255)(n) = ' '
        case _ =>
      }
      // Update the x position depending on the player number
      x = num match {
        case 1 => 0
        case 2 => 255
        case 3 => n
        case 4 => (255 - n)
      }
      // Update the y position depending on the player number
      y = num match {
        case 1 => n
        case 2 => (255 - n)
        case 3 => 0
        case 4 => 255
      }
      // Set the new position of the paddle to hashes in the buffer
      chars foreach {
        case n if num <= 2 && (1 until rows-1 contains n) => buffer(n)((x*cols)/256) = '#'
        case n if num >= 3 && (1 until cols-1 contains n) => buffer((y*rows)/256)(n) = '#'
        case _ =>
      }
    }

    // Override () operator to either change the current position or 'construct a wall' when called on an Erlang Object
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

  // The ball is an object as there's only 1 of them
  object Ball {
    private var x = 127.5
    private var y = 127.5
    // Override () operator to change the position of the ball when called with 2 doubles
    def apply(newX: Double, newY: Double) = {
      // Change the previous position of the ball to spaces (ie clear it) in the buffer
      buffer((x * rows / 256).round toInt)((y * cols / 256).round toInt) = ' '
      // Change the position of the ball, making sure it isn't out of range of the array
      // This looks weird but we need to round to the nearest multiple of rows-1/cols-1 to 255 to avoid segfaults
      x = if (((newX * rows / 256).round toInt) >= rows) (255 / (rows-1)) * (rows-1) else newX
      y = if (((newY * cols / 256).round toInt) >= cols) (255 / (cols-1)) * (cols-1) else newY
      // Set the current position of the ball to an 'O'
      buffer((x * rows / 256).round toInt)((y * cols / 256).round toInt) = 'O'
    }
  }

  // Make an array of size 4 for the players
  val players = Array.tabulate(4)(n => Player(n+1))
  // Run a bash command to get the number of columns & rows
  private val cols = "bash -c 'tput cols'".!!.trim toInt
  private val rows = "bash -c 'tput lines'".!!.trim toInt
  // Create the buffer by filling a space equivalent to the screen size with spaces
  private val buffer = Array.fill(rows, cols)(' ')
  // Change the string representation of the game to a command to clear the screen
  // followed by the buffer & finally a command to move the cursor to the top left of the screen
  override def toString = s"\u001b[3J${buffer map (_ mkString) mkString "\n"}\u001b[0;0H"

  Runtime.getRuntime.addShutdownHook(new Thread {override def run = print("\u001b[?25h")})
}
