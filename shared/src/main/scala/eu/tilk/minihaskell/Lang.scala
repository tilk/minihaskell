package eu.tilk.minihaskell

import cats._
import cats.data._
import cats.implicits._

final class Mem[T](f : => T) {
  private var m : Option[T] = None
  def get : T = m match {
    case None =>
      val v = f
      m = Some(v)
      v
    case Some(v) => v
  }
}

final class Cell[T](var value : T)

object Mem {
  def apply[T](f : => T) : Mem[T] = new Mem(f)
}

final case class Pos(val row : Int, val col : Int)

trait WithPos {
  var pos : Pos = null
}

sealed abstract class Lit {
  val value : Any
  val tp : ConsT
}
case class IntL(val value : Int) extends Lit { val tp = ConsT("Int") }
case class StrL(val value : String) extends Lit { val tp = ConsT("String") }
case class BoolL(val value : Boolean) extends Lit { val tp = ConsT("Bool") }

trait ValueType[T] {
  val tp : Type
}

object ValueType {
  def typeOf[T : ValueType] = implicitly[ValueType[T]].tp
  implicit object BoolValueType extends ValueType[Boolean] { val tp = Type.Bool }
  implicit object StringValueType extends ValueType[String] { val tp = Type.String }
  implicit object IntValueType extends ValueType[Int] { val tp = Type.Int }
  implicit object UnitValueType extends ValueType[Unit] { val tp = Type.Unit }
  implicit def Tuple2ValueType[T : ValueType, U : ValueType] = new ValueType[(T,U)] { val tp = Type.Tuple2(typeOf[T], typeOf[U]) }
  implicit def Tuple3ValueType[T : ValueType, U : ValueType, V : ValueType] = 
    new ValueType[(T,U,V)] { val tp = Type.Tuple3(typeOf[T], typeOf[U], typeOf[V]) }
  implicit def Tuple4ValueType[T : ValueType, U : ValueType, V : ValueType, W : ValueType] = 
    new ValueType[(T,U,V,W)] { val tp = Type.Tuple4(typeOf[T], typeOf[U], typeOf[V], typeOf[W]) }
  implicit def FunValueType[T : ValueType, U : ValueType] = new ValueType[T => U] { val tp = typeOf[T] -> typeOf[U] }
  implicit def MaybeValueType[T : ValueType] = new ValueType[Option[T]] { val tp = Type.Maybe(typeOf[T]) }
  implicit def ListValueType[T : ValueType] = new ValueType[List[T]] { val tp = Type.List(typeOf[T]) }
  implicit def EitherValueType[T : ValueType, U : ValueType] = new ValueType[Either[T,U]] { val tp = Type.Either(typeOf[T], typeOf[U]) }
}

class TExpr[T : ValueType] private (val expr : Expr) {
  def eval(env : Env) : T = expr.eval(env).asInstanceOf[T]
}

object TExpr {
  private[minihaskell] def apply[T : ValueType](expr : Expr) : TExpr[T] = new TExpr(expr)
}

case class TVal(val value : Any, val tp : Type) {
  def get[T : ValueType] : Option[T] = if (tp == ValueType.typeOf[T]) Some(value.asInstanceOf[T]) else None
  def apply[T : ValueType] : T =  get[T].get
}

object TVal {
  def apply[T : ValueType](v : T) : TVal = TVal(v, ValueType.typeOf[T])
}

sealed abstract class Pat extends WithPos {
  def test(env : Env, v : Any) : Option[Map[String, Any]]
  def vars : Set[String]
}
case object WildcardP extends Pat {
  def test(env : Env, v : Any) = Some(Map.empty)
  def vars = Set.empty
}
case class VarP(val x : String) extends Pat { 
  def test(env : Env, v : Any) = Some(Map(x -> v))
  def vars = Set(x)
} 
case class LitP(val lit : Lit) extends Pat { 
  def test(env : Env, v : Any) = if (v == lit.value) Some(Map.empty) else None
  def vars = Set.empty
}
case class ConsP(val name : String, val pats : List[Pat] = Nil) extends Pat {
  def test(env : Env, v : Any) = env.p(name).lift(v).map { vs =>
    if (pats.length != vs.length) throw new IllegalStateException()
    (pats, vs).zipped.map((p, v) => p.test(env, v)).flatten.fold(Map.empty)(_ ++ _)
  }
  def vars = pats.map(_.vars).fold(Set.empty)(_ ++ _)
}
case class AsP(val x : String, val pat : Pat) extends Pat {
  def test(env : Env, v : Any) = pat.test(env, v).map(_ + (x -> v))
  def vars = pat.vars + x
}
case class AnnotP(val pat : Pat, val tp : Type) extends Pat {
  def test(env : Env, v : Any) = pat.test(env, v)
  def vars = pat.vars
}

case class Env(val v : Map[String, Any], val p : Map[String, PartialFunction[Any, List[Any]]]) {
  def ++(s : Iterable[(String, Any)]) = Env(v ++ s, p)
}

sealed abstract class Expr extends WithPos {
  def eval(env : => Env) : Any = this match {
    case AppE(e1, e2) =>
      val v = e2.eval(env)
      e1.eval(env).asInstanceOf[Any => Any].apply(v)
    case lam : LamE => (v : Any) => lam.e.eval(env ++ lam.pat.test(env, v).get)
    case VarE(s) => env.v(s)
    case LitE(lit) => lit.value
    case IfE(cond, et, ef) =>
      val b = cond.eval(env).asInstanceOf[Boolean]
      if (b) et.eval(env) else ef.eval(env)
    case MatchE(e, pats) => 
      val v = e.eval(env)
      val z = pats.toStream.map { case (p, pe) => p.test(env, v).map(penv => pe.eval(env ++ penv)) }
      z.collect { case Some(v) => v }.head
    case LetrecE(defs, e) =>
      var denv : Env = env
      denv = env ++ defs.map {
        case (p, pe) => p.test(env, pe.eval(denv)).get
      }.flatten
      e.eval(denv)
    case AnnotE(e, _) => e.eval(env)
  }
  def apply(e : Expr) : Expr = AppE(this, e)
}
case class AppE(val e1 : Expr, val e2 : Expr) extends Expr
case class VarE(val v : String) extends Expr
case class LamE(val pat : Pat, val e : Expr) extends Expr
case class IfE(val cond : Expr, val et : Expr, val ef : Expr) extends Expr
case class LetrecE(val defs : List[(Pat, Expr)], val e : Expr) extends Expr
case class MatchE(val e : Expr, val pats : List[(Pat, Expr)]) extends Expr
case class LitE(val lit : Lit) extends Expr
case class AnnotE(val e : Expr, val tp : Type) extends Expr

sealed abstract class Type {
  def subst(s : Map[String, Type]) : Type = this match {
    case VarT(v) => s.get(v).getOrElse(this)
    case AppT(t1, t2) => AppT(t1.subst(s), t2.subst(s))
    case ConsT(_) => this
  }
  def freevars : Set[String] = this match {
    case VarT(v) => Set(v)
    case ConsT(_) => Set.empty
    case AppT(t1, t2) => t1.freevars union t2.freevars
  }
  def ->(t : Type) = ConsT("->")(this)(t)
  def apply(t : Type) = AppT(this, t)
  def isArrow = this match {
    case AppT(AppT(ConsT("->"), _), _) => true
    case _ => false
  }
}
case class ConsT(val s : String) extends Type
case class VarT(val v : String) extends Type
case class AppT(val t1 : Type, val t2 : Type) extends Type

object Type {
  val Unit = ConsT("Unit")
  val String = ConsT("String")
  val Int = ConsT("Int")
  val Bool = ConsT("Bool")
  def List(t : Type) = ConsT("List")(t)
  def Maybe(t : Type) = ConsT("Maybe")(t)
  def Either(t1 : Type, t2 : Type) = ConsT("Either")(t1)(t2)
  def Tuple2(t1 : Type, t2 : Type) = ConsT("Tuple2")(t1)(t2)
  def Tuple3(t1 : Type, t2 : Type, t3 : Type) = ConsT("Tuple2")(t1)(t2)(t3)
  def Tuple4(t1 : Type, t2 : Type, t3 : Type, t4 : Type) = ConsT("Tuple2")(t1)(t2)(t3)(t4)
  def Tuple(ts : Type*) = ts.foldLeft(ConsT("Tuple" ++ ts.length.toString) : Type)((tt, t) => tt(t))
  def Fun(ts : Seq[Type], rt : Type) : Type = ts.foldRight(rt)((l, r) => l -> r)
  def Fun(ts : Type*) : Type = Fun(ts.init, ts.last)
}

final case class TypeSchema(val t : Type, val vars : Set[String] = Set.empty) {
  def freevars = t.freevars -- vars
}

sealed abstract class Kind
case object Star extends Kind
case class ArrowK(val t1 : Kind, val t2 : Kind) 

object Lang {
  private val a = VarT("a")
  private val b = VarT("b")
  private val c = VarT("c")
  private val d = VarT("d")
  
  import eu.tilk.minihaskell.{Type => T}
  val varEnv : Map[String, TVal] = Map(
      "+" -> TVal((a : Int) => (b : Int) => a + b),
      "-" -> TVal((a : Int) => (b : Int) => a - b),
      "*" -> TVal((a : Int) => (b : Int) => a * b),
      "/" -> TVal((a : Int) => (b : Int) => a / b),
      "&&" -> TVal((a : Boolean) => (b : Boolean) => a && b),
      "||" -> TVal((a : Boolean) => (b : Boolean) => a || b),
      "==" -> TVal((v1 : Any) => (v2 : Any) => v1 == v2, a -> (a -> Type.Bool)),
      "/=" -> TVal((v1 : Any) => (v2 : Any) => v1 != v2, a -> (a -> Type.Bool)),
      "<" -> TVal((a : Int) => (b : Int) => a < b),
      "<=" -> TVal((a : Int) => (b : Int) => a <= b),
      ">" -> TVal((a : Int) => (b : Int) => a > b),
      ">=" -> TVal((a : Int) => (b : Int) => a >= b),
      "++" -> TVal((a : List[Any]) => (b : List[Any]) => a ++ b, T.List(a) -> (T.List(a) -> T.List(a))),
      "id" -> TVal((v : Any) => v, a -> a),
      "const" -> TVal((a : Any) => (b : Any) => a, a -> b -> a),
      "." -> TVal((f : Any => Any) => (g : Any => Any) => f compose g, (b -> c) -> ((a -> b) -> (a -> c))),
      "$" -> TVal((f : Any => Any) => f, (a -> b) -> (a -> b)),
      "curry" -> TVal((f : ((Any, Any)) => Any) => (a : Any) => (b : Any) => f((a, b)), (Type.Tuple2(a, b) -> c) -> (a -> (b -> c))),
      "uncurry" -> TVal((f : Any => Any => Any) => (p : (Any, Any)) => f(p._1)(p._2), (a -> (b -> c)) -> (Type.Tuple2(a, b) -> c)),
      "flip" -> TVal((f : Any => Any => Any) => (a : Any) => (b : Any) => f(b)(a), (a -> (b -> c)) -> (b -> (a -> c))),
      "fst" -> TVal((p : (Any, Any)) => p._1, Type.Tuple2(a, b) -> a),
      "snd" -> TVal((p : (Any, Any)) => p._2, Type.Tuple2(a, b) -> b),
      "head" -> TVal((l : List[Any]) => l.head, Type.List(a) -> a),
      "tail" -> TVal((l : List[Any]) => l.tail, Type.List(a) -> Type.List(a)),
      "null" -> TVal((l : List[Any]) => l.isEmpty, Type.List(a) -> Type.Bool),
      "map" -> TVal((f : Any => Any) => (l : List[Any]) => l.map(f), (a -> b) -> (Type.List(a) -> Type.List(b))),
      "filter" -> TVal((f : Any => Boolean, l : List[Any]) => l.filter(f), (a -> Type.Bool) -> (Type.List(a) -> Type.List(a))),
      "foldl" -> TVal((f : Any => Any => Any) => (x : Any) => (l : List[Any]) => l.foldLeft(x)((a, b) => f(a)(b)), 
          (b -> (a -> b)) -> (b -> (Type.List(a) -> b))),
      "foldr" -> TVal((f : Any => Any => Any) => (x : Any) => (l : List[Any]) => l.foldRight(x)((a, b) => f(a)(b)), 
          (a -> (b -> b)) -> (b -> (Type.List(a) -> b))),
      "concat" -> TVal((l : List[List[Any]]) => l.flatten, T.List(T.List(a)) -> a),
      "concatMap" -> TVal((f : Any => List[Any]) => (l : List[Any]) => l.flatMap(f), (a -> T.List(b)) -> (T.List(a) -> T.List(b))),
      "all" -> TVal((f : Any => Boolean) => (l : List[Any]) => l.forall(f), (a -> T.Bool) -> (T.List(a)) -> T.Bool),
      "any" -> TVal((f : Any => Boolean) => (l : List[Any]) => l.exists(f), (a -> T.Bool) -> (T.List(a)) -> T.Bool),
      "and" -> TVal((l : List[Boolean]) => l.forall(identity)),
      "or" -> TVal((l : List[Boolean]) => l.exists(identity)),
      "drop" -> TVal((n : Int) => (l : List[Any]) => l.drop(n), T.Int -> (T.List(a) -> T.List(a))),
      "take" -> TVal((n : Int) => (l : List[Any]) => l.take(n), T.Int -> (T.List(a) -> T.List(a))),
      "dropWhile" -> TVal((f : Any => Boolean) => (l : List[Any]) => l.dropWhile(f), (a -> T.Bool) -> (T.List(a) -> T.List(a))),
      "takeWhile" -> TVal((f : Any => Boolean) => (l : List[Any]) => l.takeWhile(f), (a -> T.Bool) -> (T.List(a) -> T.List(a))),
      "either" -> TVal((f : Any => Any) => (g : Any => Any) => (e : Either[Any, Any]) => e.fold(f, g), 
          (a -> c) -> ((b -> c) -> (T.Either(a, b) -> c))),
      "even" -> TVal((x : Int) => (x & 1) == 0),
      "odd" -> TVal((x : Int) => (x & 1) == 1),
      "init" -> TVal((l : List[Any]) => l.init, T.List(a) -> T.List(a)),
      "last" -> TVal((l : List[Any]) => l.last, T.List(a) -> T.List(a)),
      "maybe" -> TVal((x : Any) => (f : Any => Any) => (m : Option[Any]) => m.fold(x)(f), a -> ((b -> a) -> (T.Maybe(b) -> a))),
      "not" -> TVal((x : Boolean) => !x),
      "elem" -> TVal((x : Any) => (l : List[Any]) => l.contains(x), a -> (T.List(a) -> T.Bool)),
      "notElem" -> TVal((x : Any) => (l : List[Any]) => !l.contains(x), a -> (T.List(a) -> T.Bool)),
      "otherwise" -> TVal(true),
      "replicate" -> TVal((n : Int) => (x : Any) => List.fill(n)(x), T.Int -> (a -> T.List(a))),
      "reverse" -> TVal((l : List[Any]) => l.reverse, T.List(a) -> T.List(a)),
      "zip" -> TVal((l1 : List[Any]) => (l2 : List[Any]) => l1 zip l2, T.List(a) -> (T.List(b) -> T.List(T.Tuple2(a, b)))),
      "zipWith" -> TVal((f : Any => Any => Any) => (l1 : List[Any]) => (l2 : List[Any]) => (l1, l2).zipped.map((x, y) => f(x)(y)), 
          (a -> (b -> c)) -> (T.List(a) -> (T.List(b) -> T.List(c)))),
      "unzip" -> TVal((l : List[(Any, Any)]) => l.unzip, T.List(T.Tuple2(a, b)) -> T.Tuple2(T.List(a), T.List(b))),
      "length" -> TVal((l : List[Any]) => l.length, T.List(a) -> T.Int),
      "error" -> TVal((s : String) => throw new Exception(s), T.String -> a),
      "True" -> TVal(true),
      "False" -> TVal(false),
      "Nil" -> TVal(List[Any](), Type.List(a)),
      "Cons" -> TVal((h : Any) => (t : List[Any]) => h :: t, a -> (Type.List(a) -> Type.List(a))),
      ":" -> TVal((h : Any, t : List[Any]) => h :: t, a -> (Type.List(a) -> Type.List(a))),
      "Nothing" -> TVal(None : Option[Any], Type.Maybe(a)),
      "Just" -> TVal((v : Any) => Option(v), a -> Type.Maybe(a)),
      "Left" -> TVal((v : Any) => Left(v) : Either[Any, Any], a -> Type.Either(a, b)),
      "Right" -> TVal((v : Any) => Right(v) : Either[Any, Any], b -> Type.Either(a, b)),
      "Tuple2" -> TVal((v1 : Any) => (v2 : Any) => (v1, v2), a -> (b -> T.Tuple2(a, b))),
      "Tuple3" -> TVal((v1 : Any) => (v2 : Any) => (v3 : Any) => (v1, v2, v3), T.Fun(a, b, c, T.Tuple(a, b, c))),
      "Tuple4" -> TVal((v1 : Any) => (v2 : Any) => (v3 : Any) => (v4 : Any) => (v1, v2, v3, v4), T.Fun(a, b, c, d, T.Tuple(a, b, c, d))),
      "Unit" -> TVal(())
  )
  val patEnv : Map[String, PartialFunction[Any, List[Any]]] = Map(
      "Nil" -> { case Nil => Nil },
      "Cons" -> { case (v : Any) :: vs => List(v, vs) },
      "Nothing" -> { case None => Nil },
      "Just" -> { case Some(v : Any) => List(v)},
      "Left" -> { case Left(v : Any) => List(v)},
      "Right" -> { case Right(v : Any) => List(v)},
      "Tuple2" -> { case (v1 : Any, v2 : Any) => List(v1, v2)},
      "Tuple3" -> { case (v1 : Any, v2 : Any, v3 : Any) => List(v1, v2, v3)},
      "Tuple4" -> { case (v1 : Any, v2 : Any, v3 : Any, v4 : Any) => List(v1, v2, v3, v4)},
      "Unit" -> { case () => Nil },
      "True" -> { case true => Nil },
      "False" -> { case false => Nil }
  )
  val initEnv = Env(varEnv.mapValues(_.value), patEnv)
  
  val initTypeEnv = varEnv.mapValues(tv => TypeSchema(tv.tp, tv.tp.freevars))
}
