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
              meth.retType should be (Identifier(expectedType, Nil))
              meth.retExpr should be (New(Identifier(expectedType, Nil)))
              meth.args.head should be (Formal(Identifier(expectedType, Nil), Identifier("a", Nil)))
              meth.vars.head should be (VarDecl(Identifier(expectedType, Nil), Identifier("b", Nil)))
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
              meth.retType should be (Identifier(expectedType, Nil))
              meth.retExpr should be (New(Identifier(expectedType, Nil)))
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
  }
}
