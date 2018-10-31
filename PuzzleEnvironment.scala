/*
  PuzzleEnvironment.scala
*/


package net.salamandrina.crowwsord

import scala.reflect.ClassTag

abstract class PuzzleEnvironment {
  val puzzleName: String
  type Configuration <: AbstractConfiguration
  type PuzzleShape <: AbstractPuzzleShape
  type ExtensionStep <: AbstractExtensionStep
  def makeConfig(shape: PuzzleShape): Configuration
  def makeConfig(other: Configuration, extension: ExtensionStep): Configuration
  def makeNewConfig(shape: PuzzleShape): Configuration = makeConfig(shape)
  //
  abstract class AbstractConfiguration {
    override def toString: String = s"${puzzleName}<...>"
    def stepProposals: Seq[ExtensionStep]
    def isCompleted: Boolean
    def isValidCompletion: Boolean = true
    def extendWith(exp: ExtensionStep): Configuration
    def canExtendWith(exp: ExtensionStep): Boolean
    def lastTouch: Configuration
    // this ClassTag avoids an "unchecked for type erasure reasons" compiler warning
    def findSolutions(implicit ct: ClassTag[ExtensionStep]): Seq[Configuration] = {
      if (isCompleted)
        if (isValidCompletion)
          Seq(lastTouch)
        else
          Seq.empty
      else {
        {
          for ( 
            ext: ExtensionStep <- stepProposals;
            if canExtendWith(ext)
          ) yield extendWith(ext)
        }.toSeq.flatMap (_.findSolutions)
      }
    }
  }
  abstract class AbstractPuzzleShape
  abstract class AbstractExtensionStep
}

// object CharLineEnvironment extends PuzzleEnvironment {
//   type Configuration = CharLineConfiguration
//   type PuzzleShape = CharLinePuzzleShape
//   def makeConfig(shape: PuzzleShape): CharLineConfiguration = new CharLineConfiguration(shape)
//   //
//   class CharLineConfiguration(shape: CharLinePuzzleShape) extends AbstractConfiguration {
//     val clShape: CharLinePuzzleShape = shape
//   }
//   class CharLinePuzzleshape(len: Int, universe: Set[Char]) extends AbstractPuzzleShape {
//     val clLen: Int = len
//     val clUniverse: Set[Char] = universe
//   }
// }

///

/*
abstract class ConfShape[+T]
abstract class ConfContents[+T]
// cells are filled with T, items to insert are U
abstract class Configuration[T,U] (val shape: ConfShape[Configuration[T,U]],
    // val contents: Seq[T], val pieces: Set[U]) {
    val contents: ConfContents[Configuration[T,U]], val pieces: Set[U]) {
  type ExtendStep
  def stepProposals: Seq[ExtendStep]
  def isCompleted: Boolean
  def isValidCompletion: Boolean = true
  def canExtendWith(c: ExtendStep): Boolean
  val objName: String = "GeneralConfig"
  val displayFiller: T
  def extendWith(stp: ExtendStep): Configuration[T,U]
  def findSolutions: Seq[Configuration[T,U]] = {
    if (isCompleted)
      if (isValidCompletion)
        Seq(lastTouch)
      else
        Seq.empty
    else {
      {
        for ( 
          c: ExtendStep <- stepProposals;
          if canExtendWith(c)
        ) yield extendWith(c)
      }.toSeq.flatMap (_.findSolutions)
    }
  }
  def lastTouch: Configuration[T,U] = this
  override def toString = s"${objName}< . . . >"
}

class CharLine(shape: CharLineShape, contents: CharLineContents, pieces: Set[Char])
  extends Configuration[Char,Char](shape,contents,pieces) {
  case class CharLineStep(val piece: Char)
  type ExtendStep = CharLineStep
  def stepProposals: Seq[ExtendStep] = {
    ( for (c: Char <- pieces) yield CharLineStep(c)).toSeq
  }
  def isCompleted = contents.chars.length==shape.length
  val displayFiller: Char = '.'
  override val objName="CharLine"
  def extendWith(stp: ExtendStep): CharLine = new CharLine(
    shape,
    new CharLineContents(contents.chars :+ stp.piece),
    pieces
  )
  def canExtendWith(stp: ExtendStep): Boolean = {
    val newContentsBody: Seq[Char] = contents.chars :+ stp.piece
    if(newContentsBody.length < 2)
      true
    else
      newContentsBody.sliding(2) forall ( pair => pair(0) < pair(1) )
  }
  override def toString = (
      contents.chars ++
      Seq.fill(shape.length-contents.chars.length)(displayFiller)
    )
    .mkString(s"${objName}<","",">")
}
class CharLineShape(val length: Int) extends ConfShape[CharLine]
class CharLineContents(val chars: Seq[Char]) extends ConfContents[CharLine]
*/
