package music

abstract class Command(val str: String, val help: String):
  def apply(args: Seq[String]): String

class Defined extends Command(s"${Def.name}", s"Execute the function ${Def.name}"){
      //New function every new instance
      val command = Def.func.tail
        def apply(args: Seq[String]): String =
          Play(command) 
}

object Command:
  var all: Seq[Command] = Seq(Help, Quit, Play, Def)
  def allHelpTexts: String  =
    Command.all.map(c => c.str.padTo(10,' ') + c.help).mkString("\n")

  def find(command: String): Option[Command] = all.find(_.str == command)

  def apply(cmd: String, args: Seq[String]): String =

    all.find(_.str == cmd) match
      case Some(c) => c(args)
      case None => s"Unkown command: $cmd\nType ? for help."

  def loopUntilExit(nextLine: () => String): Unit =
    val line = nextLine()
    if line != null then
      val result = line.split(' ').toSeq match
        case Seq() => ""
        case cmd +: args => Command(cmd, args)
      if result != "" then println(result)
      if result != Main.exitMsg then loopUntilExit(nextLine)
    else 
      println("\n" + Main.exitMsg)

object Delete extends Command("del", "delete command"):
  def apply(args: Seq[String]): String = args match{
    case Seq(cmd) => 
            //Map through all and if matching lookup sequence remove element
    case _ => s"Choose a command to delete..."
  }

object Help extends Command("?", "print help"):
  def apply(args: Seq[String]): String = args match
    case Seq() => Command.allHelpTexts 
    case Seq(cmd) => Command.find(cmd).map(_.help).getOrElse(s"Unknown: $cmd")
    case _ => s"Usage: $str [cmd]"

object Quit extends Command(":q", "quit this app"):
  def apply(args: Seq[String]): String = args match
    case Seq() => Main.exitMsg
    case _ => s"Error: $args after :q not allowed"

object Def extends Command("def", "Define function"):
  //Accessible by Defined
  var name = ""
  var func = Seq("")
  def apply(args: Seq[String]): String = args match
    case Seq(_, "!", _*) => 
      name = args.head //Ex. Em
      func = args.tail //Ex. ! p 64 67 71

      Command.all = Command.all :+ new Defined
      Command.all.map(c => println(c))

      s"defined $name: ${func.mkString(" ")}"
    case _ => ""

object Play extends Command("!", "play chord"):
  def apply(args: Seq[String]): String = args match
    case Seq("p", tail @ _*) => Synth.changeInstrument(Synth.GMInstruments.AcousticGrandPiano, 0)
        val nbr = args.tail.map(_.toInt)
        val pno = new Piano(nbr.toSet)
        val chord = pno.toChordOpt.get

        ChordPlayer.play(chord) 
        s"Play $pno $chord"
    case Seq("g", _, _, _, _, _, _) => 
      if(args.tail.length == 6){Synth.changeInstrument(Synth.GMInstruments.AcousticGuitarSteel, 0) 
        val nbr = args.tail.map(_.toInt)
        val nbr_tuple = (nbr(0), nbr(1), nbr(2), nbr(3), nbr(4), nbr(5))

        val gtr = new Guitar(nbr_tuple)
        val chord = gtr.toChordOpt.get

        ChordPlayer.play(chord) 
        s"Play $gtr $chord"
      }
      else
        "On a Guitar each string must have a position!"
    case Seq("u", _, _, _, _) => Synth.changeInstrument(Synth.GMInstruments.AcousticGuitarNylon, 0)
        val nbr = args.tail.map(_.toInt)
        val nbr_tuple = (nbr(0), nbr(1), nbr(2), nbr(3))

        val uke = new Ukulele(nbr_tuple)
        val chord = uke.toChordOpt.get
        
        ChordPlayer.play(chord) 
        s"Play $uke $chord"
    case _ => if(args.tail.length != 6 && args.head.toLowerCase.equals("g")) then 
               "On a Guitar each string must have a position!"
               else if(args.tail.length != 4 && args.head.toLowerCase.equals("u")) then
                "On an Ukelele each string must have a position!"
               else
                 "Try again \n"
