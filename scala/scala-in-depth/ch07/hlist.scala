object holder {

  sealed trait HList {
    type ViewAt[Idx <: Nat] <: IndexedView
  }

  final case class HCons[H, T <: HList](head: H, tail: T) extends HList {
    type ViewAt[N <: Nat] = N#Expand[
      ({ type Z[P <: Nat] = HListViewN[H, T#ViewAt[P]] })#Z,
      HListView0[H,T],
      IndexedView
    ]

    type FullType = HCons[H,T]
    def viewAt[Idx <: Nat](implicit in: FullType => FullType#ViewAt[Idx]) =
      in(this.asInstanceOf[FullType])

    def ::[A](v: A) = HCons(v, this)
    override def toString = head + " :: " + tail
  }

  final class HNil extends HList {
    def ::[A](v: A) = HCons(v, this)
    override def toString = "Nil"
  }

  object HList {
    type ::[H, T <: HList] = HCons[H, T]

    val ::   = HCons
    val HNil = new HNil
  }
  import HList._


  sealed trait IndexedView {
    type Before <: HList
    type At
    type After  <: HList

    def fold[R](f: (Before, At, After) => R): R
    def get = fold((_, value, _) => value)
  }

  class HListView0[H, T <: HList](val list : H :: T) extends IndexedView {
    type Before = HNil
    type At     = H
    type After  = T

    def fold[R](f: (Before, At, After) => R): R = f(HNil, list.head, list.tail)
  }

  final class HListViewN[H, NextIdxView <: IndexedView](
      h: H, next: NextIdxView) extends IndexedView {
    type Before = H :: NextIdxView#Before
    type At     = NextIdxView#At
    type After  = NextIdxView#After

    def fold[R](f: (Before, At, After) => R): R =
      next.fold((before, at, after) => f(HCons(h, before), at, after))
  }

  object IndexedView {
    implicit def index0[H, T <: HList](list : H :: T) : HListView0[H,T] =
      new HListView0[H,T](list)
    implicit def indexN[H, T <: HList, Prev <: IndexedView](list : H :: T)(
        implicit indexTail: T => Prev): HListViewN[H,Prev] =
        new HListViewN[H, Prev](list.head, indexTail(list.tail))
  }


  sealed trait Nat {
    type Expand[NonZero[N <: Nat] <: Up, IfZero <: Up, Up] <: Up
  }
  object Nat {
    sealed trait _0 extends Nat {
      type Expand[NonZero[N <: Nat] <: Up, IfZero <: Up, Up] = IfZero
    }

    sealed trait Succ[Prev <: Nat] extends Nat {
      type Expand[NonZero[N <: Nat] <: Up, IfZero <: Up, Up] = NonZero[Prev]
    }

    type _1 = Succ[_0]
    type _2 = Succ[_1]
    type _3 = Succ[_2]
    type _4 = Succ[_3]
    type _5 = Succ[_4]
  }

}

import holder._
import HList._

val x : (String :: Int :: Boolean :: HNil) = "Hello" :: 5 :: false :: HNil
val one :: two :: three :: HNil = x
val first :: second :: rest     = x
