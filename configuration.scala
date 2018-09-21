/*
  configuration.scala

  class for holding immutable configurations
*/

package net.salamandrina.crowwsord

class Configuration(val length: Int, val contents: Seq[Char], val pieces: Set[Char]) {
  def isCompleted: Boolean = contents.length==length
  def isLegal: Boolean = {
    if(contents.length < 2)
      true
    else
      contents.sliding(2) forall ( pair => pair(0) < pair(1) )
  }
  def findSolutions: Seq[Configuration] = {
    if (isCompleted)
      Seq(this)
    else {
      {
        for ( 
          c: Char <- pieces;
          newC: Configuration = new Configuration(length, contents :+ c, pieces)
          if newC.isLegal
        ) yield newC
      }.toSeq.flatMap (_.findSolutions)
    }
  }
  override def toString = (contents ++ Seq.fill(length-contents.length)('.')).mkString("Cfg< ",""," >")
}
