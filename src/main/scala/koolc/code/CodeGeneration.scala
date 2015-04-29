package koolc
package code

import ast.Trees._
import analyzer.Symbols._
import analyzer.Types._
import cafebabe._
import AbstractByteCodes.{New => _, _}
import ByteCodes._
import utils._

object CodeGeneration extends Pipeline[ Option[Program], Unit] {

  def run(ctx: Context)(prog: Option[Program]): Unit = {
    import ctx.reporter._

    def typeToString(tpe: Type): String = {
      tpe match{
        case TString => "Ljava/lang/String;"
        case TArray => "[I"
        case TInt => "I"
        case TBoolean => "Z"
        case id@TObject(_) => "L" + id.toString + ";"
      }
      //[warn] It would fail on the following inputs: TError, TUnresolved, TUntyped
    }

    /** Writes the proper .class file in a given directory. An empty string for dir is equivalent to "./". */
    def generateClassFile(sourceName: String, ct: ClassDecl, dir: String): Unit = {
      // TODO: Create code handler, save to files ...

      //Setting up the classfile
      val classFile = new ClassFile(ct.id.value, None)
      classFile.setSourceFile(sourceName)
      classFile.addDefaultConstructor

      //Adding fields
      ct.vars foreach { field =>
        val typeString = typeToString(field.symbol.tpe)

        val fh: FieldHandler = classFile.addField(typeString, field.id.value)
      }

      //Adding methods:
      ct.methods foreach { method =>
        val rType = typeToString(method.retType.getType)
        var parType = ""
        method.args foreach { arg =>
          parType += typeToString(arg.id.symbol.tpe)
        }
        val mh: MethodHandler = classFile.addMethod(rType, method.id.value, parType)
        val ch: CodeHandler = mh.codeHandler
        generateMethodCode(ch, method)
      }

      classFile.writeToFile("./"+ct.id.value+".class")
    }

    def getField(cs: ClassSymbol, field: String):cafebabe.AbstractByteCodes.AbstractByteCodeGenerator ={
      cs.members.get(field) match {
        case Some(vari) => GetField( cs.name, field, typeToString(vari.tpe) )
        case None => getField(cs.parent.get, field)
      }
    }

    // a mapping from variable symbols to positions in the local variables
    // of the stack frame
    def generateMethodCode(ch: CodeHandler, mt: MethodDecl): Unit = {
      val methSym = mt.symbol
      var locals: Map[String,Int] = Map.empty
      var i = 1
      for(x <- methSym.argList ){
        locals += (x.name -> i)
        i+=1
      }
      methSym.members foreach {value => locals += (value._1 -> ch.getFreshVar)}


      mt.stats foreach {compileStat(ch,_,Some(methSym,locals))}
      compileExpr(ch,mt.retExpr,Some(methSym,locals))
      ch << IRETURN

      // TODO: Emit code
      println(">>>>> " + mt.id.value)
      ch.print
      println(">>>>> " + mt.id.value)
      //ch.freeze
    }

    def generateMainMethodCode(ch: CodeHandler, stmts: List[StatTree], cname: String): Unit = {

      stmts foreach {compileStat(ch,_,None)}
      // TODO: Emit code
      ch << RETURN
      println(">>>>> " + "main")
      ch.print
      //ch.freeze
    }

    val outDir = ctx.outDir.map(_.getPath+"/").getOrElse("./")

    val f = new java.io.File(outDir)
    if (!f.exists()) {
      f.mkdir()
    }

    val sourceName = ctx.file.get.getName

    // output code
    prog.get.classes foreach {
      ct => generateClassFile(sourceName, ct, outDir)
    }


    // Now do the main method
    // ...
    val main = prog.get.main
    val classFile = new ClassFile(main.id.value, None)
    classFile.setSourceFile(sourceName)
    classFile.addDefaultConstructor
    val ch = classFile.addMainMethod.codeHandler
    generateMainMethodCode(ch, main.stats, main.id.value)
    classFile.writeToFile("./"+main.id.value+".class")

  }

  def compileStat(ch: CodeHandler, stmt: StatTree, lookup: Option[(MethodSymbol, Map[String,Int])]): Unit = {
    stmt match {
      case Block(stats) => stats foreach {compileStat(ch,_,lookup)}
      case If(expr, thn, els) => {
        val nAfter = ch.getFreshLabel("after")
        val nElse = ch.getFreshLabel("else")
        compileExpr(ch,expr,lookup)
        ch<<IfEq(nElse)
        compileStat(ch,thn,lookup)
        ch<<Goto(nAfter)
        ch<<Label(nElse)
        els match {
          case Some(stat) => compileStat(ch,stat,lookup)
          case None =>
        }
        ch<<Label(nAfter)
        }
      case While(expr, stat) =>
      case Println(expr) => {
        ch << GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;")
        compileExpr(ch,expr,lookup)
        ch << InvokeVirtual("java/io/PrintStream", "println", "(I)V")
      }
      case Assign(id, expr) => {
        compileExpr(ch,expr,lookup)
        ch << IStore((lookup.get._2 get id.value).get)
      }
      case ArrayAssign(id, index, expr) =>
    }
  }


  def compileExpr(ch: CodeHandler, expr: ExprTree, lookup: Option[(MethodSymbol, Map[String,Int])]): Unit = {
    expr match {
      case And(lhs, rhs) => {
        val nAfter = ch.getFreshLabel("after")
        val nElse = ch.getFreshLabel("else")
        compileExpr(ch,lhs,lookup)
        ch<<IfEq(nElse)
        compileExpr(ch,rhs,lookup)
        ch<<Goto(nAfter)
        ch<<Label(nElse)
        ch<<ICONST_0
        ch<<Label(nAfter)
        }
      case Or(lhs, rhs) => {
        val nAfter = ch.getFreshLabel("after")
        val nElse = ch.getFreshLabel("else")
        compileExpr(ch,lhs,lookup)
        ch<<IfEq(nElse)
        ch<<ICONST_1
        ch<<Goto(nAfter)
        ch<<Label(nElse)
        compileExpr(ch,rhs,lookup)
        ch<<Label(nAfter)
      }
      case Plus(lhs, rhs) => {
        compileExpr(ch,lhs,lookup)
        compileExpr(ch,rhs,lookup)
        ch<<IADD}
      case Minus(lhs, rhs) => {
        compileExpr(ch,lhs,lookup)
        compileExpr(ch,rhs,lookup)
        ch<<ISUB}
      case Times(lhs, rhs) => {
        compileExpr(ch,lhs,lookup)
        compileExpr(ch,rhs,lookup)
        ch<<IMUL}
      case Div(lhs, rhs) => {
        compileExpr(ch,lhs,lookup)
        compileExpr(ch,rhs,lookup)
        ch<<IDIV}
      case LessThan(lhs, rhs) => {
        compileExpr(ch,lhs,lookup)
        compileExpr(ch,rhs,lookup)
        val nTrue = ch.getFreshLabel("true")
        val nAfter = ch.getFreshLabel("after")
        ch<<If_ICmpLt(nTrue)
        ch<<ICONST_0
        ch<<Goto(nAfter)
        ch<<Label(nTrue)<<ICONST_1<<Label(nAfter)
        }
      case Equals(lhs, rhs) => {
        compileExpr(ch,lhs,lookup)
        compileExpr(ch,rhs,lookup)
        val nTrue = ch.getFreshLabel("true")
        val nAfter = ch.getFreshLabel("after")
        ch<<If_ICmpEq(nTrue)
        ch<<ICONST_0
        ch<<Goto(nAfter)
        ch<<Label(nTrue)<<ICONST_1<<Label(nAfter)
        }
      case ArrayRead(arr, index) => {}
      case ArrayLength(arr) => {}
      case MethodCall(obj, meth, args) => {
        obj match {
          case New(tpe) => tpe.symbol match {
            case id:ClassSymbol => println(id.methods)
            case _ =>
          }
          case id@Identifier(value) => println(id.symbol.tpe)
          case _ =>
        }
        println(expr)
      }
      case IntLit(value) => ch << Ldc(value)
      case StringLit(value) => {}

      case True()  => ch << ICONST_1
      case False()  => ch << ICONST_0
      case Identifier(value) => ch << ILoad((lookup.get._2 get value).get)

      case This() =>
      case NewIntArray(size)  =>
      case New(tpe)  => println("AAAAAAAAAAAAA>>>>>" + tpe)

      case Not(expr)  =>

    }
  }

}
