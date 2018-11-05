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
    def stepProposals: Stream[ExtensionStep]
    def isCompleted: Boolean
    def isValidCompletion: Boolean = true
    def extendWith(exp: ExtensionStep): Configuration
    def canExtendWith(exp: ExtensionStep): Boolean
    def lastTouch: Configuration
    // this ClassTag avoids an "unchecked for type erasure reasons" compiler warning
    def findSolutions(implicit ct: ClassTag[ExtensionStep]): Stream[Configuration] = {
      if (isCompleted)
        if (isValidCompletion)
          Stream(lastTouch)
        else
          Stream.empty
      else {
        {
          for ( 
            ext: ExtensionStep <- stepProposals;
            if canExtendWith(ext)
          ) yield extendWith(ext)
        }.toStream.flatMap (_.findSolutions)
      }
    }
  }
  abstract class AbstractPuzzleShape
  abstract class AbstractExtensionStep
}
