package music

abstract class Command(val str: String, val help: String):
  def apply(args: Seq[String]): String

class Defined extends Command(s"${Def.getName}", s"Execute the function ${Def.getName}"){
      //New function every new instance
      val command = Def.getFunc.tail
        def apply(args: Seq[String]): String =
          Play(command)
}

//This class is to avoid the other Commands to access the object Command sequence. 
class CommandSet:
  private var commandSet: Seq[Command] = Seq(Help, Quit, Play, Def, Delete)
  
  def add(command: Command): Unit =
    commandSet = commandSet :+ command

  def remove(command: Command): Unit =
    commandSet = commandSet.filter(_ != command)

  def get: Seq[Command] =
    commandSet
     


object Command:
  val cmdSet = new CommandSet
  def all: Seq[Command] = cmdSet.get
  def allHelpTexts: String  =
    cmdSet.get.map(c => c.str.padTo(10,' ') + c.help).mkString("\n")

  def find(command: String): Option[Command] = cmdSet.get.find(_.str == command)

  def apply(cmd: String, args: Seq[String]): String =

    cmdSet.get.find(_.str == cmd) match
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

object Delete extends Command("del", "Delete command"):
  def apply(args: Seq[String]): String = args match{
    case Seq(cmd) => var str = ""; println(cmd)
        Command.cmdSet.get.foreach(c => 
          if(cmd.equals(c.str) && !cmd.equals("!") 
                               && !cmd.equals("?") 
                               && !cmd.equals("del") 
                               && !cmd.equals(":q") 
                               && !cmd.equals("!"))
                               && !cmd.equals("def") 
                                then
                                  println(true) 
                                  Command.cmdSet.remove(c)
                                  str = s"Deleted: ${c.str}"
                                  else
                                   str = s"$cmd is an essential command which cannot be deleted."
        )
                                            
                      str
            //Map through all and if matching lookup sequence remove element
    case _ => s"Choose a command to delete... $args"
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

      Command.cmdSet.add(new Defined)

      s"defined $name: ${func.mkString(" ")}"
    case _ => ""
  def getName: String = name
  def getFunc: Seq[String] = func

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
