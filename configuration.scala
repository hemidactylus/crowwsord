/*
  configuration.scala

  class for holding immutable configurations
*/

package net.salamandrina.crowwsord

abstract class ConfShape[+T]
// cells are filled with T, items to insert are U
abstract class Configuration[T,U] (val shape: ConfShape[Configuration[T,U]], val contents: Seq[T], val pieces: Set[U]) {
  type ExtendStep
  def stepProposals: Seq[ExtendStep]
  def isCompleted: Boolean
  def canExtendWith(c: ExtendStep): Boolean
  val objName: String = "GeneralConfig"
  val displayFiller: T
  def extendWith(stp: ExtendStep): Configuration[T,U]
  def findSolutions: Seq[Configuration[T,U]] = {
    if (isCompleted)
      Seq(this)
    else {
      {
        for ( 
          c: ExtendStep <- stepProposals;
          if canExtendWith(c)
        ) yield extendWith(c)
      }.toSeq.flatMap (_.findSolutions)
    }
  }
  override def toString = s"${objName}< . . . >"
}

class CharLine(shape: CharLineShape, contents: Seq[Char], pieces: Set[Char])
  extends Configuration[Char,Char](shape,contents,pieces) {
  case class CharLineStep(val piece: Char)
  type ExtendStep = CharLineStep
  def stepProposals: Seq[ExtendStep] = {
    ( for (c: Char <- pieces) yield CharLineStep(c)).toSeq
  }
  def isCompleted = contents.length==shape.length
  val displayFiller: Char = '.'
  override val objName="CharLine"
  def extendWith(stp: ExtendStep): CharLine = new CharLine(shape,contents :+ stp.piece,pieces)
  def canExtendWith(stp: ExtendStep): Boolean = {
    val newContents: Seq[Char] = contents :+ stp.piece
    if(newContents.length < 2)
      true
    else
      newContents.sliding(2) forall ( pair => pair(0) < pair(1) )
  }
  override def toString = (contents ++ Seq.fill(shape.length-contents.length)(displayFiller)).mkString(s"${objName}<","",">")
}
class CharLineShape(val length: Int) extends ConfShape[CharLine]

class IntLine(shape: IntLineShape, contents: Seq[Int], pieces: Set[Int]) 
  extends Configuration[Int,Int](shape,contents,pieces) {
  case class IntLineStep(val piece: Int)
  type ExtendStep = IntLineStep
  def stepProposals: Seq[ExtendStep] = {
    ( for (c: Int <- pieces) yield IntLineStep(c)).toSeq
  }
  def isCompleted = contents.length==shape.length
  val displayFiller: Int = -1
  override val objName="IntLine"
  def extendWith(stp: IntLineStep): IntLine = new IntLine(shape,contents :+ stp.piece,pieces)
  def canExtendWith(stp: IntLineStep): Boolean = {
    val newContents: Seq[Int] = contents :+ stp.piece
    ( newContents forall ( _ > 2 ) ) &&
    (
      newContents.length < shape.length ||
      ( ( newContents :\ 0 ) (_ + _) ) % 2 == 0
    )
  }
  override def toString = (contents ++ Seq.fill(shape.length-contents.length)(displayFiller)).mkString(s"${objName}<","",">")
}
class IntLineShape(val length: Int) extends ConfShape[IntLine]

class SteppableIntLine(shape: SteppableIntLineShape, contents: Seq[Int]) 
  extends Configuration[Int,Nothing](shape,contents,Set.empty) {
  /*
    contents: list of indices reached in their order
  */
  case class SteppableIntLineStep(val pos: Int)
  type ExtendStep = SteppableIntLineStep
  def stepProposals: Seq[ExtendStep] = {
    val lastIndex = if (contents.isEmpty) (shape.length+10) else contents.last
    val okIndices = (0 until shape.length)
      .filter( x => ! ( contents contains x ) )
      .filter(x => Math.abs(lastIndex - x)>1)
    (for (c <- okIndices) yield new SteppableIntLineStep(c)).toSeq
  }
  def isCompleted = contents.size==shape.length
  val displayFiller: Int = -1
  override val objName="SteppableIntLine"
  def extendWith(stp: SteppableIntLineStep): SteppableIntLine = new SteppableIntLine(shape,contents :+ stp.pos)
  def canExtendWith(stp: SteppableIntLineStep): Boolean = {
    true
  }
  override def toString = {
    val indMap = (contents zipWithIndex).toMap
    (0 until shape.length)
      .map( x => indMap getOrElse(x,displayFiller) )
      .mkString(s"${objName}<","",">")
  }
}
class SteppableIntLineShape(val length: Int) extends ConfShape[SteppableIntLine]
