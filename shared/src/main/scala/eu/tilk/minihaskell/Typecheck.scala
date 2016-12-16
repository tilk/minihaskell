package eu.tilk.minihaskell

import cats._
import cats.data._
import cats.implicits._

sealed class TypeErrorMsg
case class DefNotFoundMsg(val v : String) extends TypeErrorMsg
case class NotEnoughArgumentsMsg(val v : String) extends TypeErrorMsg
case class OccursCheckMsg(val v : String, val tp : Type) extends TypeErrorMsg
case class UnificationErrorMsg(val t1 : Type, val t2 : Type) extends TypeErrorMsg
case object ImplementationErrorMsg extends TypeErrorMsg

final case class TypeError(val pos : Pos, val msg : TypeErrorMsg)

object TypeError {
  implicit object TypeErrorMonoid extends Monoid[TypeError] {
    def empty = TypeError(null, ImplementationErrorMsg)
    def combine(t1 : TypeError, t2 : TypeError) = t1
  }
}

class Typecheck(tenv : Map[String, TypeSchema]) {
  private var counter = 0
  
  private def freshVar = {
    counter += 1
    VarT("_" ++ counter.toString)
  }
  
  private var subst = Map.empty[String, Type]
  
  private def instSchema(s : TypeSchema) = s.t.subst(Map(s.vars.toList.map(v => (v, freshVar)):_*))
  
  private def typeError[T](pos : Pos, msg : TypeErrorMsg) : Either[TypeError, T] = Left(TypeError(pos, msg))
  
  private def extendSubst(x : String, t : Type) = {
    val st = t.subst(subst)
    subst = subst.mapValues(_.subst(Map((x, st)))) + ((x, st))
  }
  
  private def applySubst(t : Type) = t.subst(subst)
  
  private def unify(pos : Pos, t1 : Type, t2 : Type) : Either[TypeError, Unit] = (t1, t2) match {
    case (ConsT(s1), ConsT(s2)) if s1 == s2 => Right(())
    case (VarT(v1), VarT(v2)) if v1 == v2 => Right(())
    case (VarT(v), _) => subst.get(v) match {
      case None => 
        if (t2.subst(subst).freevars(v)) 
          typeError(pos, OccursCheckMsg(v, t2.subst(subst))) 
        else { extendSubst(v, t2); Right(()) }
      case Some(t) => unify(pos, t, t2)
    }
    case (_, VarT(v)) => subst.get(v) match {
      case None => 
        if (t1.subst(subst).freevars(v)) 
          typeError(pos, OccursCheckMsg(v, t1.subst(subst))) 
        else { extendSubst(v, t1); Right(()) }
      case Some(t) => unify(pos, t, t1)
    }
    case (AppT(t1a, t1b), AppT(t2a, t2b)) => for {
      _ <- unify(pos, t1a, t2a)
      _ <- unify(pos, t1b, t2b)
    } yield ()
    case (_, _) => typeError(pos, UnificationErrorMsg(t1, t2))
  }
  
  private def typecheckPat(p : Pat) : Either[TypeError, (Type, Map[String, Type])] = p match {
    case WildcardP => Right((freshVar, Map.empty[String, Type]))
    case VarP(v) => val t = freshVar; Right((t, Map((v, t))))
    case LitP(lit) => Right((lit.tp, Map.empty[String, Type]))
    case AsP(v, pat) => typecheckPat(pat).map { case (t, m) => (t, m + (v -> t)) }
    case ConsP(n, pats) => for {
      l <- pats.traverse[({type T[A] = Either[TypeError, A]})#T, (Type, Map[String, Type])](typecheckPat)
      t <- tenv.get(n).toRight(TypeError(p.pos, DefNotFoundMsg(n))).map(instSchema)
      tps = l.map(_._1)
      rt = freshVar
      _ <- unify(p.pos, t, Type.Fun(tps, rt))
      m = l.map(_._2).fold(Map.empty)(_ ++ _)
      _ <- if (!applySubst(rt).isArrow) Right(()) else Left(TypeError(p.pos, NotEnoughArgumentsMsg(n)))
    } yield (rt, m)
    case AnnotP(pat, t) => for {
      x <- typecheckPat(pat)
      _ <- unify(p.pos, t, x._1)
    } yield x
  }
  
  private def typecheck(e : Expr, m : Map[String, TypeSchema]) : Either[TypeError, Type] = e match {
    case VarE(v) => Right(instSchema(m(v)))
    case LamE(p, e) => for {
      p <- typecheckPat(p); (pt, pmap) = p
      rt <- typecheck(e, m ++ pmap.mapValues(TypeSchema(_)))
    } yield (pt -> rt)
    case AppE(e1, e2) => for {
      t1 <- typecheck(e1, m)
      t2 <- typecheck(e2, m)
      rt = freshVar
      _ <- unify(e.pos, t1, t2 -> rt)
    } yield rt
    case LitE(lit) => Right(lit.tp)
    case IfE(cond, e1, e2) => for {
      tcond <- typecheck(cond, m)
      t1 <- typecheck(e1, m)
      t2 <- typecheck(e2, m)
      _ <- unify(e.pos, t1, t2)
    } yield t1
    case MatchE(me, pats) => for {
      mt <- typecheck(me, m)
      rts <- pats.traverse[({type T[A] = Either[TypeError, A]})#T, Type] { case (p, pe) => for {
        p <- typecheckPat(p); (pt, pmap) = p
        _ <- unify(e.pos, pt, mt)
        t <- typecheck(pe, m ++ pmap.mapValues(TypeSchema(_))) // TODO
      } yield t}
      _ <- (rts zip rts.tail).traverse[({type T[A] = Either[TypeError, A]})#T, Unit] { case (t1, t2) => 
        unify(e.pos, t1, t2) }
    } yield rts.head
    case LetrecE(defs, e) => 
      for {
        pts <- defs.traverse[({type T[A] = Either[TypeError, A]})#T, (Type, Map[String, Type])] { case (pat, _) => typecheckPat(pat) }
        ts = pts.flatMap(_._2)
        tm = m ++ ts.map(_.map(TypeSchema(_)))
        ts1 <- defs.traverse[({type T[A] = Either[TypeError, A]})#T, Type] { case (x, de) => typecheck(de, tm) }
        _ <- (pts.map(_._1) zip ts1).traverse[({type T[A] = Either[TypeError, A]})#T, Unit]{ case (t1, t2) => unify(e.pos, t1, t2) }
        mfv = m.values.map(_.freevars).fold(Set.empty)(_ union _)
        m1 = m ++ ts.map(_.map{dt => val dt1 = applySubst(dt); TypeSchema(dt1, dt1.freevars -- mfv)})
        t <- typecheck(e, m1)
      } yield t
    case AnnotE(e, tp) => for {
      t <- typecheck(e, m)
      _ <- unify(e.pos, t, tp)
    } yield t
  }
  
  def typecheck(e : Expr) : Either[TypeError, Type] = typecheck(e, tenv).map(applySubst(_))
  def typecheckAs[T : ValueType](e : Expr) : Either[TypeError, TExpr[T]] = 
    typecheck(e).flatMap { t => unify(e.pos, t, ValueType.typeOf[T]).map(_ => TExpr[T](e)) }
}