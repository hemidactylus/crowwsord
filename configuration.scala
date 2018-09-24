/*
  configuration.scala

  class for holding immutable configurations
*/

package net.salamandrina.crowwsord

abstract class ConfShape[+T]
// cells are filled with T, items to insert are U
abstract class Configuration[T,U] (val shape: ConfShape[Configuration[T,U]], val contents: Seq[T], val pieces: Set[U]) {
  def isCompleted: Boolean
  def isLegal: Boolean
  val objName: String = "GeneralConfig"
  val filler: T
  def extend(c: U): Configuration[T,U]
  def findSolutions: Seq[Configuration[T,U]] = {
    if (isCompleted)
      Seq(this)
    else {
      {
        for ( 
          c: U <- pieces;
          newC: Configuration[T,U] = extend(c)
          if newC.isLegal
        ) yield newC
      }.toSeq.flatMap (_.findSolutions)
    }
  }
  override def toString = s"${objName}<...>"
}

class CharLine(shape: CharLineShape, contents: Seq[Char], pieces: Set[Char]) 
  extends Configuration[Char,Char](shape,contents,pieces) {
  def isCompleted = contents.length==shape.length
  val filler: Char = '.'
  override val objName="CharLine"
  def extend(c: Char): CharLine = new CharLine(shape,contents :+ c,pieces)
  def isLegal = {
    if(contents.length < 2)
      true
    else
      contents.sliding(2) forall ( pair => pair(0) < pair(1) )
  }
  override def toString = (contents ++ Seq.fill(shape.length-contents.length)(filler)).mkString(s"${objName}<","",">")
}
class CharLineShape(val length: Int) extends ConfShape[CharLine]

class IntLine(shape: IntLineShape, contents: Seq[Int], pieces: Set[Int]) 
  extends Configuration[Int,Int](shape,contents,pieces) {
  def isCompleted = contents.length==shape.length
  val filler: Int = -1
  override val objName="IntLine"
  def extend(c: Int): IntLine = new IntLine(shape,contents :+ c,pieces)
  def isLegal = {
    ( contents forall ( _ > 2 ) ) &&
    (
      !isCompleted ||
      ( ( contents :\ 0 ) (_ + _) ) % 2 == 0
    )
  }
  override def toString = (contents ++ Seq.fill(shape.length-contents.length)(filler)).mkString(s"${objName}<","",">")
}
class IntLineShape(val length: Int) extends ConfShape[IntLine]
