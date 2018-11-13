/*
  PatriciaTreeSet.scala
*/

package net.salamandrina.patriciatrees

import collection._
import scala.collection.mutable.{Builder, SetBuilder}
import scala.collection.generic.CanBuildFrom

class PatriciaTreeSet
  extends mutable.Set[String]
  with mutable.SetLike[String,PatriciaTreeSet] {

  var suffixes: immutable.Map[Char, PatriciaTreeSet] = Map.empty
  var value: Boolean = false

  def contains(s: String): Boolean = {
    if ( s.isEmpty ) value
    else suffixes get ( s(0) ) match {
      case None => false
      case Some(sm) => sm.contains ( s substring 1 )
    }
  }

  def withPrefix(s: String): PatriciaTreeSet =
    if ( s.isEmpty ) this
    else {
      val leading = s(0)
      suffixes get leading match {
        case None =>
          suffixes = suffixes + (leading -> empty)
        case _ =>
      }
      suffixes (leading) withPrefix ( s substring 1 )
    }

  override def update(s: String, included: Boolean) =
    withPrefix(s).value = included

  def iterator: Iterator[ String ] =
    (for ( (k,v) <- kvIterator; if (v) ) yield k)

  def maskedIterator(mask: List[Set[Char]]): Iterator[ String ] = {
    /*
      Returns an iterator given a char-per-char mask.
        Items longer than the provided mask are not returned.
        Items not matching the mask are not returned.
        Items shorter than, but matching, the mask are returned.
      (this is tailored to the application of this method in the crossword puzzle)
    */
    (
      mask match {
        case Nil => Iterator.empty
        case chars :: rest => for (
            c <- chars.iterator;
            if suffixes contains c;
            submap = suffixes(c);
            subvalue <- submap.maskedIterator(rest)
          ) yield (c +: subvalue)
      }
    ) ++ (if (value) List("").iterator else Iterator.empty)
  }

  def kvIterator: Iterator[ (String, Boolean) ] =
    ( for (rt <- List("").iterator) yield (rt,value) ) ++
    (for (
        (k,submap) <- suffixes.iterator;
        (rst,subval) <- submap.kvIterator;
        if subval
      ) yield (k +: rst, subval)
    )

  def += (s: (String)): this.type = { update(s,true); this }
  def -= (s: String): this.type = {update(s,false); this }
  override def empty = new PatriciaTreeSet

  override def toString =
    "PatriciaTreeSet(" +
    ( iterator map ( k => s"${k}" ) mkString ", " ) +
    ")"

}

object PatriciaTreeSet {
  
  def empty = new PatriciaTreeSet

  def apply ( ss: String* ): PatriciaTreeSet = {
    apply( ss )
  }
  def apply ( sq: TraversableOnce[String] ): PatriciaTreeSet = {
    val m: PatriciaTreeSet = empty
    for (s <- sq ) m += s
    m
  }

  def newBuilder: Builder[String, PatriciaTreeSet] =
    new SetBuilder[String, PatriciaTreeSet](empty)

    implicit def canBuildFrom: CanBuildFrom[PatriciaTreeSet, String, PatriciaTreeSet] =
      new CanBuildFrom[PatriciaTreeSet, String, PatriciaTreeSet] {
        def apply(from: PatriciaTreeSet) = newBuilder
        def apply() = newBuilder
      }

}
