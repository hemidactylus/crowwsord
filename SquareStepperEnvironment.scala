/*
  SquareStepperEnvironment.scala
*/

import net.salamandrina.crowwsord._

object SquareStepperEnvironment extends PuzzleEnvironment {
  //
  case class Position(x: Int, y: Int) {
    override def toString: String = s"(${x},${y})"
  }
  object DirectionEnum extends Enumeration {
    val Forward, Backward = Value
  }
  type Direction = DirectionEnum.Value
  //
  val puzzleName = "SquareStepper"
  type Configuration = SquareStepperConfiguration
  type PuzzleShape = SquareStepperPuzzleShape
  type ExtensionStep = SquareStepperExtensionStep
  def makeConfig(shape: PuzzleShape): Configuration = new Configuration(shape,Seq.empty,Seq.empty)
  def makeConfig(other: Configuration, extension: ExtensionStep): Configuration = {
    //
    // if (other.sForward.length > 4) println(other)
    //
    extension.direction match {
      case DirectionEnum.Forward => new Configuration(
        other.sShape,
        other.sForward :+ extension.newPosition,
        other.sBackward
      )
      case DirectionEnum.Backward => new Configuration(
        other.sShape,
        other.sForward,
        other.sBackward :+ extension.newPosition
      )
    }
  }
  //
  class SquareStepperPuzzleShape(val side: Int) extends AbstractPuzzleShape
  //
  class SquareStepperConfiguration(
    shape: SquareStepperPuzzleShape,
    forward: Seq[Position],
    backward: Seq[Position]
  ) extends AbstractConfiguration {
    //
    val sShape: SquareStepperPuzzleShape = shape
    val sForward: Seq[Position]=forward
    val sBackward: Seq[Position]=backward
    //
    def extendWith(extensionStep: ExtensionStep): Configuration = makeConfig(this,extensionStep)
    def canExtendWith(extensionStep: ExtensionStep): Boolean = {
      !(
        sForward contains extensionStep.newPosition
      ) && !(
        sBackward contains extensionStep.newPosition
      )
    }
    val deltas: Seq[Position] = Seq(
      Position( 0,+3),
      Position( 0,-3),
      Position(+3, 0),
      Position(-3, 0),
      Position(-2,-2),
      Position(-2,+2),
      Position(+2,-2),
      Position(+2,+2)
    )
    def expandFromPoint(sPoint: Position): Seq[Position] = {
      for (
        delta <- deltas;
        nPoint = Position(sPoint.x+delta.x,sPoint.y+delta.y);
        if nPoint.x >= 0 && nPoint.y >= 0 && nPoint.x < sShape.side && nPoint.y < sShape.side
      ) yield nPoint
    }
    def stepProposals: Seq[ExtensionStep] = {
      val newDir: Direction = if(sForward.length > sBackward.length) DirectionEnum.Backward else DirectionEnum.Forward
      val posList: Seq[Position] = if (newDir == DirectionEnum.Forward) sForward else sBackward
      if (posList.isEmpty) {
        if ( newDir == DirectionEnum.Forward)
          for( y <- 0 until sShape.side ) yield new ExtensionStep(Position(sShape.side-2,y),newDir)
          // for( x <- 0 until sShape.side ; y <- 0 until sShape.side ) yield new ExtensionStep(Position(x,y),newDir)
        else
          Seq[ExtensionStep](new ExtensionStep(Position(sForward.last.y,sForward.last.x),newDir))
      } else {
        for (newPos <- expandFromPoint(posList.last) ) yield new ExtensionStep(newPos,newDir)
      }
    }
    def lastTouch: Configuration = this
    def isCompleted: Boolean = 
      !sForward.isEmpty &&
      !sBackward.isEmpty &&
      // assuming neither is empty
      (sForward.length + sBackward.length == sShape.side*sShape.side) && {
        val lastF: Position = sForward.last
        val lastB: Position = sBackward.last
        ( expandFromPoint(lastF) ).exists( _ == lastB )
      }
    override def toString: String = {
      val nChars: Int = s"${sShape.side*sShape.side+1}".length+3
      val places = Array.fill(sShape.side)(Array.fill(sShape.side)(""))
      for ( (pos,ind) <- ( sForward ++ sBackward.reverse ).zipWithIndex) {
        val indDesc: String = s"${ind+1}"
        places(pos.x)(pos.y)=" "*(nChars-indDesc.length) + indDesc
      }
      s"${puzzleName} <\n${ places.map( _.mkString ).mkString("\n") }\n>"

      // val strDescF: String = s"F ${sForward.map( _.toString ).mkString("#")}"
      // val strDescB: String = s"B ${sBackward.reverse.map( _.toString ).mkString("#")}"
      // s"${puzzleName}<${strDescF}|${strDescB}>"
    }
  }
  class SquareStepperExtensionStep(val newPosition: Position, val direction: Direction) extends AbstractExtensionStep
}
