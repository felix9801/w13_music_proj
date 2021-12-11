package music

case class Chord(ps: Vector[Pitch]):
  assert(ps.nonEmpty, "Chord pitch sequence is empty")

  val pitchClasses: Vector[Int]   = ps.map(_.pitchClass).toVector

  val pitches: Vector[Int] = ps.map(_.nbr).toVector

  def apply(i: Int): Pitch = ps(i)

  def intervals(root: Pitch = ps(0)): Vector[Int] = ps.map(_.nbr - root.nbr)

  def relativePitchClasses(root: Pitch = ps(0)): Vector[Int] =
    intervals(root).map(i => (i%12 + 12) % 12).distinct.sorted

  def name(root: Pitch = ps(0)): String = relativePitchClasses(root) match
    case Vector(0, 4, 7)     => root.pitchClassName
    case Vector(0, 3, 7)     => root.pitchClassName + "m"
    case Vector(0, 4, 7, 10) => root.pitchClassName + "7"
    case Vector(0, 3, 7, 10) => root.pitchClassName + "m7"
    case Vector(0, 4, 7, 11) => root.pitchClassName + "maj7"
    case Vector(0, 3, 6)     => root.pitchClassName + "dim"
    case Vector(0, 4, 8)     => root.pitchClassName + "aug"
    case Vector(0, 6, 10, 15)=> root.pitchClassName + " Tristan"
    case _ => root.pitchClassName + intervals(root).mkString("[",",","]")

  override def toString = ps.map(_.name).mkString("Chord(",",",")")

object Chord:
  def apply(xs: String*): Chord = Chord(xs.map(Pitch.apply).toVector)

  def random(pitchNumbers: Seq[Int] = (60 to 72), n: Int = 3): Chord =
    val shuffled = scala.util.Random.shuffle(pitchNumbers).toVector
    Chord(shuffled.take(n).map(Pitch.apply))
