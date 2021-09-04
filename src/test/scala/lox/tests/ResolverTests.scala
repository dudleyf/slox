//package lox.tests
//
//import lox.{BlockStmt, Interpreter, Lox, Resolver}
//import org.scalatest.flatspec.AnyFlatSpec
//import org.scalatest.matchers.should.Matchers
//
//import scala.collection.mutable
//
//class ResolverTests extends AnyFlatSpec with Matchers :
//
//  val scopesTest =
//    """var a = "global a";
//      |var b = "global b";
//      |var c = "global c";
//      |{
//      |  var a = "outer a";
//      |  var b = "outer b";
//      |  {
//      |    var a = "inner a";
//      |    print a;
//      |    print b;
//      |    print c;
//      |  }
//      |  print a;
//      |  print b;
//      |  print c;
//      |}
//      |print a;
//      |print b;
//      |print c;""".stripMargin
//
//  def mkResolver() = Resolver(Interpreter())
//
//  "resolving a block statement" should "do the thing" in {
//    val lox = Lox()
//    val ast = lox.parse(
//      """var a = "global a";
//        |var b = "global b";
//        |{
//        |  var a = "outer a";
//        |  var b = "outer b";
//        |  {
//        |    var a = "inner a";
//        |    print a;
//        |    print b;
//        |  }
//        |  print a;
//        |  print b;
//        |}
//        |print a;
//        |print b;""".stripMargin)
//
//    lox.resolver.resolve(ast)
//    lox.interpreter.locals shouldEqual mutable.Map()
//  }
//
