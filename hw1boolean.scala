package booleanlogic

sealed abstract class Boolean {
  
  def ifTrue[T](doWhenTrue: => T): Option[T]
  
  def &&(other: => Boolean): Boolean
  
  def &(other: Boolean): Boolean
  
  def ||(other: => Boolean): Boolean
  
  def |(other: Boolean): Boolean
  
  def ^(other: Boolean): Boolean
  
  def unary_! : Boolean
}

case object true1 extends Boolean {

  override def ifTrue[T](doWhenTrue: => T): Option[T] = Some(doWhenTrue)
  
  override def &&(other: => Boolean): Boolean = other

  override def &(other: Boolean): Boolean = other

  override def ||(other: => Boolean): Boolean = this
  
  override def |(other: Boolean): Boolean = this
  
  override def ^(other: Boolean): Boolean = !other

  override def unary_! : Boolean = false1
}
  
case object false1 extends Boolean {

  override def ifTrue[T](doWhenTrue: => T): Option[T] = None
  
  override def &&(other: => Boolean): Boolean = this
  
  override def &(other: Boolean): Boolean = this

  override def ||(other: => Boolean): Boolean = other

  override def |(other: Boolean): Boolean = other
  
  override def ^(other: Boolean): Boolean = other

  override def unary_! : Boolean = true1
}

object Conditional {
  
  def if1[T](boolean: Boolean)(doWhenTrue: => T): IfResult[T] = {
    val resultOfIfTrue = boolean.ifTrue(doWhenTrue)
    return new IfResult(boolean, resultOfIfTrue)
  }
  
  class IfResult[T] private[Conditional] (boolean: Boolean, resultOfIfTrue: Option[T]) {
    
    def elseif(boolean: Boolean)(doWhenTrue: => T): IfResult[T] = {
      val ifResultIfAlreadyTrue = resultOfIfTrue.map(_=> this)
      return ifResultIfAlreadyTrue.getOrElse(new IfResult(boolean, boolean.ifTrue(doWhenTrue)))
    }

    def else1(doWhenFalse: => T): T = {
      return resultOfIfTrue.getOrElse(doWhenFalse)
    }
  }
}
