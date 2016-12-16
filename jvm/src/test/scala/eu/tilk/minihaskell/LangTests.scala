package eu.tilk.minihaskell

import org.scalatest._

class LangTests extends WordSpec {
  def test[T : ValueType](name : String, prog : String, ret : T) = {
    name should {
      var parsed : Expr = null
      "parse correctly" in {
        parsed = Parser.parse(prog).right.get
      }
      var typed : TExpr[T] = null
      "typecheck correctly" in {
        val Right(te) = new Typecheck(Lang.initTypeEnv).typecheckAs[T](parsed)
        typed = te
      }
      "evaluate correctly" in {
        assert(typed.eval(Lang.initEnv) == ret)
      }
    }
  }
  test("2+2", "2+2", 4)
  test("id application", "(fun x -> x) 4", 4)
  test("Just 1", "Just 1", Some(1) : Option[Int])
  test("(1, True)", "Tuple2 1 True", (1, true))
  test("polymorphic id", "let id x = x in Tuple2 (id 1) (id True)", (1, true))
  test("if expression", "if 1 > 0 then 5 else 0", 5)
  test("factorial", "let fact x = if x > 0 then x * fact (x-1) else 1 in fact 5", 120)
  test("factorial by pattern match", "let fact 0 = 1; fact x = x * fact (x-1) in fact 5", 120)
  test("factorial by guards", "let fact x | x > 0 = x * fact (x-1) | otherwise = 1 in fact 5", 120)
  test("mutual recursion", "let myodd x = x == 1 || myeven(x-1); myeven x = x == 0 || myodd(x-1) in myeven(8)", true)
  test("destructuring let", "let (x,y) = (1,2) in x+y", 3)
  test("length by recursion", "let len l = case l of Nil -> 0; Cons _ xs -> 1 + len xs in len [1,2,3]", 3)
  test("length by recursion defined by clauses", "let len Nil = 0; len (Cons _ xs) = 1 + len xs in len [1,2,3,4]", 4)
  test("dollar", "Cons 1 $ Cons 2 $ Nil", List(1,2))
  test("composition", "Cons 1 . Cons 2 $ Nil", List(1,2))
  test("[1,2,3]", "[1,2,3]", List(1,2,3))
  test("(1,2)", "(1,2)", (1,2))
  test("int pattern match", "case 2+2 of 4 -> 1; 5 -> 2", 1)
  test("int pattern match fallback", "case 2+2 of 3 -> 1; x -> x", 4)
  test("maybe pattern match", "case Just 5 of Nothing -> 0; Just x -> x", 5)
  test("list pattern match", "case [1,2,3] of [a,b,c] -> a+b+c", 6)
  test("prelude length", "length [4,3,2,1]", 4)
  test("type annotation", "[] :: List Int", List[Int]())
  test("string literal", """"abc0\n\x30\u1234"""", "abc0\n\u0030\u1234")
  test("case guards", "case 1 of x | even x -> true | otherwise -> false", false)
  test("list comprehension", "[x+y | x <- [1,2], y <- [3,4]]", List(4,5,5,6))
  test("list comprehension with guards", "[x+y | x <- [1,2,3], y <- [3,4], x /= y]", List(4,5,5,6,7))
}
