package eu.tilk.minihaskell

import cats._
import cats.data._
import cats.implicits._

import fastparse.WhitespaceApi

object Lexer {
  import fastparse.all._

  val kws = Set("if", "then", "else", "let", "in", "fun", "case", "of", "true", "false")
  
  val number : P[Int] = P(digit.rep(1).!.map(_.toInt)).opaque("number")
  val identifier : P[String] = P((lower ~ idchar.rep).!).filter { s => !kws(s) }.opaque("identifier")
  val constructor : P[String] = P((upper ~ idchar.rep).!).opaque("constructor")
  val string : P[String] = P("\"" ~/ stringchar.rep ~ "\"").map(_.mkString).opaque("string literal")
  
  def kw(s : String) = s ~ !(idchar)
  def op(s : String) = s ~ !(opchar)
  def binop[T](s : String, f : (T, T) => T) = op(s).map(_ => f)
  def binopVar(s : String, n : String) = binop[Expr](s, (l, r) => AppE(AppE(VarE(n), l), r))
  def binopSimple(s : String) = binopVar(s, s)
  def ws = Pass.map(_ => AppE)
  def tws = Pass.map(_ => AppT)
  
  val digit = CharPred(_.isDigit)
  val hex = digit | CharIn("abcdefABCDEF")
  val lower = CharPred(_.isLower)
  val upper = CharPred(_.isUpper)
  val letterOrDigit = CharPred(_.isLetterOrDigit)
  val idchar = letterOrDigit | "_"
  val opchar = CharIn("~!@#$%^&*+|-=<>?:")
  val escaped : P[String] = "\\" ~/ (
      P("n").map(_ => "\n") | 
      P("\"").map(_ => "\"") | 
      P("\\").map(_ => "\\") |
      P("x" ~ hex.rep(exactly=2).!).map(v => Integer.parseInt(v, 16).toChar.toString) |
      P("u" ~ hex.rep(exactly=4).!).map(v => Integer.parseInt(v, 16).toChar.toString))
  val stringchar = CharPred(c => !c.isControl && c != '"' && c != '\\').! | escaped
}

object Parser {
  val White = WhitespaceApi.Wrapper{
    import fastparse.all._
    NoTrace(" ".rep)
  }
  import fastparse.noApi._
  import Lexer._
  import White._
  
  def chainl[T](p : P[T], op : P[(T, T) => T]) : P[T] = P(p ~ (op ~ p).rep).map {
    case (lhs, chunks) => 
      chunks.foldLeft(lhs) { case (lhs, (op, rhs)) => op(lhs, rhs) }
  }
  
  def chainr[T](p : P[T], op : P[(T, T) => T]) : P[T] = 
    P(p ~ ((op ~/ chainr(p, op)).map { case(op, rhs) => (lhs : T) => op(lhs, rhs) } | Pass.map(_ => identity[T](_))))
    .map(p => p._2(p._1))
  
  def withPos[T <: WithPos](p : P[T]) : P[T] = P(Index ~ p).map { case (i, e) => e.pos = Pos(0, i); e } // TODO row
    
  val addOp = binopSimple("+")
  val subOp = binopSimple("-")
  val sumOp = addOp | subOp

  val timOp = binopSimple("*")
  val divOp = binopSimple("/")
  val mulOp = timOp | divOp

  val eqOp = binopSimple("==")
  val neOp = binopSimple("/=")
  val ltOp = binopSimple("<")
  val leOp = binopSimple("<=")
  val gtOp = binopSimple(">")
  val geOp = binopSimple(">=")
  val compareOp = neOp | eqOp | ltOp | leOp | gtOp | geOp
  
  val andOp = binop[Expr]("&&", (l, r) => IfE(l, r, LitE(BoolL(false))))
  val orOp = binop[Expr]("||", (l, r) => IfE(l, LitE(BoolL(true)), r))
  
  val appendOp = binopSimple("++") | binopSimple(":")
  
  val appOp = binopSimple("$")
  val composeOp = binopSimple(".")
  
  val literal : P[Lit] = number.map(IntL) | string.map(StrL) | P(kw("true")).map(_ => BoolL(true)) | P(kw("false")).map(_ => BoolL(false))
  val parens : P[Expr] = P("(" ~/ expr.rep(0, ",".~/) ~ ")").map { s =>
    val l = s.length
    l match {
      case 0 => VarE("Unit")
      case 1 => s.head
      case _ => s.foldLeft(VarE(s"Tuple$l") : Expr)((t, h) => t(h))
    }
  }
  
  val lcelem = P(pat ~ "<-" ~/ expr).map { case (pat, pe) => (e : Expr) => VarE("concatMap")(LamE(pat, e))(pe)} |
    P(expr).map { be => (e : Expr) => IfE(be, e, VarE("Nil")) }
  val listliteral : P[Expr] = P("[" ~/ expr.rep(0, ",".~/) ~ ("|" ~/ lcelem.rep(1, ",".~/)).? ~ "]").map { case (l, lc) => 
    lc.toSeq.flatten.foldRight(l.foldRight(VarE("Nil") : Expr)((h, t) => VarE("Cons")(h)(t))) { case (f, e) => f(e) }
  }
  val atom : P[Expr] = withPos(literal.map(LitE) | identifier.map(VarE) | constructor.map(VarE)) | parens | listliteral
  
  def mkCaseTree(k : Int, clauses : List[(Seq[Pat], Expr)]) : Expr =
    if (clauses.isEmpty) VarE("error")(LitE(StrL("Pattern match failed")))
    else if (clauses.head._1.isEmpty) {
      clauses.head._2
    } else {
      val cases = clauses.groupBy(_._1.head).mapValues(clauses1 => mkCaseTree(k+1, clauses1.map(_.bimap(_.tail, identity))))
      val ocases = clauses.map(_._1.head).distinct.map(x => (x, cases(x)))
      MatchE(VarE(s"%$k"), ocases)
    }
  
  def fundefguard(opname : String) = P(((opname ~/ expr) | ("|" ~/ expr ~ opname ~/ expr).rep(1).map( l => 
      l.foldRight(VarE("error")(LitE(StrL("no matching guard")))){ case ((c, e), re) => IfE(c, e, re) } )))
  
  val fundef = P(identifier ~ pat.rep(1) ~/ fundefguard("=")).flatMap { case (x, ps, e) =>
    (";" ~ identifier.filter(_ == x).map(_ => ()) ~/ pat.rep(ps.length) ~ fundefguard("=")).rep(0).map { clauses => 
      if (clauses.isEmpty) (VarP(x), ps.foldRight(e)(LamE(_, _))) 
      else (VarP(x), (0 to (clauses.length-1)).map(i => VarP(s"%$i")).foldRight(mkCaseTree(0, (ps, e) :: clauses.toList))(LamE(_, _)))
    }
  }
  
  val letclause = P(pat ~ "=" ~/ expr) | fundef
  
  val caseclause = 
    P(pat ~ fundefguard("->"))
  
  val expr10 = chainl(atom, ws)
  val expr9 = chainl(expr10, composeOp)
  val expr8 = expr9
  val expr7 = chainl(expr8, mulOp)
  val expr6 = chainl(expr7, sumOp)
  val expr5 = chainr(expr6, appendOp)
  val expr4 = chainl(expr5, compareOp)
  val expr3 = chainl(expr4, andOp)
  val expr2 = chainl(expr3, orOp)
  val expr1 = expr2
  val expr0 = chainr(expr1, appOp)
  val expra = P(expr0 ~ ("::" ~/ tp).?).map { case (e, ot) => ot.map(tp => AnnotE(e, tp)).getOrElse(e) }
  val expr : P[Expr] = expra | withPos(
    P(kw("if") ~/ expr ~ kw("then") ~/ expr ~ kw("else") ~/ expr).map { case (e1, e2, e3) => IfE(e1, e2, e3) } |
    P(kw("let") ~/ letclause.rep(1, ";".~/) ~ kw("in") ~/ expr).map { case (ds, e) => LetrecE(ds.toList, e) } |
    P(kw("fun") ~/ atompat.rep(1) ~ "->" ~ expr).map { case (p, e) => p.foldRight(e)(LamE(_,_)) } |
    P(kw("case") ~/ expr ~ kw("of") ~ caseclause.rep(1, ";".~/)).map { case(e, ps) => MatchE(e, ps.toList) }
  )
  
  val parenstp : P[Type] = P("(" ~/ tp.rep(0, ",".~/) ~ ")").map { s =>
    val l = s.length
    l match {
      case 0 => ConsT("Unit")
      case 1 => s.head
      case _ => s.foldLeft(ConsT(s"Tuple$l") : Type)((t, h) => t(h))
    }
  }
  val listtp : P[Type] = P("[" ~/ tp ~ "]").map(Type.List)
  
  val atomtp = identifier.map(VarT(_)) | constructor.map(ConsT(_)) | parenstp | listtp
  val tp0 = chainl(atomtp, tws)
  val tp : P[Type] = chainr(tp0, binop[Type]("->", (t1, t2) => t1 -> t2))
  
  val parenspat : P[Pat] = P("(" ~/ pat.rep(0, ",".~/) ~ ")").map { s =>
    val l = s.length
    l match {
      case 0 => ConsP("Unit")
      case 1 => s.head
      case _ => ConsP(s"Tuple$l", s.toList)
    }
  }
  
  val listpat : P[Pat] = P("[" ~/ pat.rep(0, ",".~/) ~ "]").map { s =>
    s.foldRight(ConsP("Nil"))((h, t) => ConsP("Cons", List(h, t)))
  }

  val atompat : P[Pat] = parenspat | listpat | withPos (
    P(identifier).map(VarP) |
    P(literal).map(LitP) |
    P("_").map(_ => WildcardP)
  )
  val pat0 : P[Pat] = atompat | 
    P(identifier ~ "@" ~/ pat0).map { case (i, p) => AsP(i, p) } |
    P(constructor ~/ atompat.rep).map { case (c, ps) => ConsP(c, ps.toList) }
  val pat : P[Pat] = P(pat0 ~ ("::" ~/ tp).?).map { case (p, otp) => otp.map(tp => AnnotP(p, tp)).getOrElse(p) }
    
  def parse(s : String) : Either[Parsed.Failure, Expr] = (expr ~ End).parse(s) match {
    case Parsed.Success(res, _) => Right(res)
    case f : Parsed.Failure => Left(f)
  }
}