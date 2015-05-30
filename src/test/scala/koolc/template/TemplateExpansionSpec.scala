package koolc
package template

import org.scalatest.FunSpec
import org.scalatest.Inside
import org.scalatest.Matchers

import utils._
import lexer._
import ast._
import analyzer._

import Trees._

class TemplateExpansionSpec extends FunSpec with TestUtils with Matchers with ReporterMatchers with Inside {

  override def pipeline = Parser andThen ClassTemplateExpander andThen NameAnalysis andThen TypeChecking

  describe("The template expanders") {

    they("don't expand unreferenced templates.") {
      val source = """
      object Main { def main(): Unit = {} }
      class Foo<T> {}
      class Bar {
        def bar<T>(): Int = { return 0; }
      }
      """
      checkResultForString(source) { (ctx, program) =>
        ctx.reporter shouldBe errorless
        program should not be None
        program.get.classes.size should be (1)
        inside(program.get.classes.head) { case clazz: ClassDecl =>
          clazz.methods should be (Nil)
        }
      }
    }

    they("expand class templates.") {
      checkResultForFile("class-templates-basic.kpp") { (ctx, program) =>
        ctx.reporter shouldBe errorless
        program should not be None

        inside(program.get.main.stats) { case List(If(expr, _, _)) =>
          expr should be (Equals(
            MethodCall(New(Identifier("Foo$Foo$Bar")), Identifier("bar"), List(New(Identifier("Foo$Bar")))),
            MethodCall(New(Identifier("Foo$Foo$Bar")), Identifier("bar"), List(New(Identifier("Foo$Bar"))))
          ))
        }
        inside(program.get.classes.find(_.id.value == "Foo$Foo$Bar").get) {
            case ClassDecl(id, parent, vars, methods, template) =>
          val idFB = Identifier("Foo$Bar")

          id should be (Identifier("Foo$Foo$Bar"))
          parent should be (Some(Identifier("Boo$Foo$Bar")))
          vars should be (List(VarDecl(idFB, Identifier("a"))))
          methods.size should be (1)

          inside(methods.head) { case MethodDecl(retType, id, args, vars, stats, retExpr, template) =>
            retType should be (idFB)
            id should be (Identifier("bar"))
            args should be (List(Formal(idFB, Identifier("a"))))
            vars should be (List(VarDecl(idFB, Identifier("b"))))

            inside(stats) { case List(assignStat: Assign, ifStat: If) =>
              assignStat.id should be (Identifier("b"))
              assignStat.expr should be (Identifier("a"))

              inside(ifStat) { case If(expr, Block(List(thn)), els) =>
                expr should be (And(
                  LessThan(Plus(Plus(IntLit(0), IntLit(1)), IntLit(2)), IntLit(0)),
                  Equals(New(idFB), Identifier("b"))
                ))
                thn should be (Assign(
                  Identifier("b"),
                  MethodCall(This(), Identifier("bar"), List(New(idFB)))
                ))
                els should be (None)
              }
            }
            retExpr should be (New(idFB))
            template should be (Nil)
          }
        }
      }
    }

    they("expand nested class template arguments.") {
      val source = """
      object Main { def main(): Unit = {
        if(new Foo<Foo<Foo<Foo<Foo<Foo<Foo<Foo<Int>>>>>>>>() == new Foo<Bool>()) {}
      } }
      class Foo<T> {}
      """
      checkResultForString(source) { (ctx, program) =>
        ctx.reporter shouldBe errorless
        program should not be None

        program.get.classes.length should be (9)
        program.get.classes.find(_.id.value == "Foo$Foo$Foo$Foo$Foo$Foo$Foo$Foo$Int") should not be None
      }
    }

    they("allow template classes to reference themselves.") {
      val source = """
      object Main { def main(): Unit = {
        if(new Foo<Int>() == new Foo<Int>()) {}
      }}
      class Foo<T> {
        var a: Foo<T>;
      }
      """
      assertStringSucceeds(source)
    }

    they("allow template methods to call themselves.") {
      val source = """
      object Main { def main(): Unit = {
        if(new Foo().bar<Int>() == 0) {}
      }}
      class Foo {
        def bar<T>(): T = { return this.bar<T>(); }
      }
      """
      assertStringSucceeds(source)
    }

    they("fail if template expansion causes infinite recursion.") {
      val source = """
      object Main { def main(): Unit = {
        if(new Foo<Int>() == new Foo<Int>()) {}
      }}
      class Foo<T> {
        var a: Foo<Foo<T>>;
      }
      """
      assertStringFails(source)
    }

    they("expand class template references in template classes.") {
      val source = """
      object Main { def main(): Unit = {
        if(new Foo<Bar<Bool>>() == new Foo<Bool>()) {}
      } }
      class Foo<T> {
        var a: Bar<T>;
      }
      class Bar<T> {}
      """
      checkResultForString(source) { (ctx, program) =>
        ctx.reporter shouldBe errorless
        program should not be None
        program.get.classes.find(_.id.value == "Foo$Bar$Bool") should not be None
        program.get.classes.find(_.id.value == "Bar$Bool") should not be None
      }
    }

    they("expand method templates.") {
      val source = """
        object Main { def main(): Unit = { } }

        class Bar<T> {}
        class Foo {
          def foo(): Foo = {
            var a: Foo;
            var b: Bar<Foo>;
            var c: Bar<Bar<Foo>>;
            a = this.bar<Foo>(new Foo());
            b = this.bar<Bar<Foo>>(new Bar<Foo>());
            c = this.bar<Bar<Bar<Foo>>>(new Bar<Bar<Foo>>());
            return this.bar<Foo>(a);
          }
          def bar<T>(a: T): T = {
            var b: T;
            return new T();
          }
        }
      """

      checkResultForString(source) { (ctx, program) =>
        ctx.reporter shouldBe errorless
        program should not be None

        inside(program.get.classes.last) { case clazz =>
          clazz.methods.size should be (4)
          inside(clazz.methods) { case List(methBarBarBarFoo, methBarBarFoo, methBarFoo, methFoo) =>
            methFoo.id.name should be ("foo")

            (methBarFoo, "bar$Foo", "Foo") ::
            (methBarBarFoo, "bar$Bar$Foo", "Bar$Foo") ::
            (methBarBarBarFoo, "bar$Bar$Bar$Foo", "Bar$Bar$Foo") ::
            Nil foreach { case (meth, expectedName, expectedType) =>
              meth.id.name should be (expectedName)
              meth.retType should be (Identifier(expectedType))
              meth.retExpr should be (New(Identifier(expectedType)))
              meth.args.head should be (Formal(Identifier(expectedType), Identifier("a")))
              meth.vars.head should be (VarDecl(Identifier(expectedType), Identifier("b")))
            }
          }
        }
      }
    }

    they("expand methods in the class where they're defined.") {
      val source = """
        object Main { def main(): Unit = {
          if(new Bar().bar<Bar>() == new Bar().bar<Foo>()) {}
        } }

        class Foo {
          def bar<T>(): T = {
            return new T();
          }
        }
        class Bar extends Foo {}
      """

      checkResultForString(source) { (ctx, program) =>
        ctx.reporter shouldBe errorless
        program should not be None

        inside(program.get.classes) { case List(fooClass, barClass) =>
          inside(fooClass.methods) { case List(methBarFoo, methBarBar) =>
            (methBarFoo, "bar$Foo", "Foo") ::
            (methBarBar, "bar$Bar", "Bar") ::
            Nil foreach { case (meth, expectedName, expectedType) =>
              meth.id.name should be (expectedName)
              meth.retType should be (Identifier(expectedType))
              meth.retExpr should be (New(Identifier(expectedType)))
            }
          }
          barClass.methods should be (Nil)
        }
      }
    }

    they("detect name collisions between class template parameters.") {
      val source = """
        object Main { def main(): Unit = {} }
        class Foo<T, T> {}
      """
      assertStringFails(source)
    }

    they("detect name collisions between method template parameters.") {
      val source = """
        object Main { def main(): Unit = {} }
        class Foo {
          def bar<T, T>(): Int = { return 0; }
        }
      """
      assertStringFails(source)
    }

    they("detect name collisions between class and method template parameters.") {
      val source = """
        object Main { def main(): Unit = {} }
        class Foo<T> {
          def bar<T>(): Int = { return 0; }
        }
      """
      assertStringFails(source)
    }

    they("detect name collisions between class template parameters and class declarations.") {
      val source = """
        object Main { def main(): Unit = {} }
        class Foo<T> { }
        class T {}
      """
      assertStringFails(source)
    }

    they("detect name collisions between method template parameters and class declarations.") {
      val source = """
        object Main { def main(): Unit = {} }
        class Foo {
          def bar<T>(): Int = { return 0; }
        }
        class T {}
      """
      assertStringFails(source)
    }

    describe("successfully expand") {
      "class-and-method-templates.kpp" ::
      "class-templates-basic.kpp" ::
      "complicated-templates.kpp" ::
      "maybelist-templates.kpp" ::
      "method-templates-basic.kpp" ::
      "simple-template-method.kpp" ::
      "simple-templates.kpp" ::
      "template-method-recursion.kpp" ::
      Nil foreach { name =>
        it(name) {
          assertFileSucceeds(name)
        }
      }
    }
  }
}
