/*
  configuration.scala

  class for holding immutable configurations
*/

package net.salamandrina.crowwsord

abstract class Configuration[T] (val size: Int, val contents: Seq[T], val pieces: Set[T]) {
  def isCompleted: Boolean
  def isLegal: Boolean
  val objName: String = "GeneralConfig"
  val filler: T
  def extend(c: T): Configuration[T]
  def findSolutions: Seq[Configuration[T]] = {
    if (isCompleted)
      Seq(this)
    else {
      {
        for ( 
          c: T <- pieces;
          newC: Configuration[T] = extend(c)
          if newC.isLegal
        ) yield newC
      }.toSeq.flatMap (_.findSolutions)
    }
  }
  override def toString = (contents ++ Seq.fill(size-contents.length)(filler)).mkString(s"${objName}<","",">")
}

class CharLine(size: Int, contents: Seq[Char], pieces: Set[Char]) 
  extends Configuration[Char](size,contents,pieces) {
  def isCompleted = contents.length==size
  val filler: Char = '.'
  override val objName="CharLine"
  def extend(c: Char): CharLine = new CharLine(size,contents :+ c,pieces)
  def isLegal = {
    if(contents.length < 2)
      true
    else
      contents.sliding(2) forall ( pair => pair(0) < pair(1) )
  }
}
