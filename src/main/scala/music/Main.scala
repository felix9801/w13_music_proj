package music

object Main:
  val (helloMsg, exitMsg) = ("*** Welcome to music!", "Goodbye music!")
  val console = new jline.console.ConsoleReader
  console.setExpandEvents(false)
  def readLine(): String = console.readLine("music> ")

  def main(args: Array[String]): Unit =
    println(helloMsg)
    Command.loopUntilExit(readLine _)
