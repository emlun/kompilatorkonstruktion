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

  private def typeToString(tpe: Type): String = {
    tpe match {
      case TString       => "Ljava/lang/String;"
      case TArray        => "[I"
      case TInt          => "I"
      case TBoolean      => "Z"
      case TObject(clazz) => "L" + getJvmClassName(clazz) + ";"
      case TError | TUnresolved | TUntyped => ???
    }
    //[warn] It would fail on the following inputs: TError, TUnresolved, TUntyped
  }

  def getJvmClassName(classSymbol: ClassSymbol): String = classSymbol.name

  private def returnInstruction(method: MethodDecl): ByteCode = method.retType.getType match {
    case TInt    | TBoolean                 => IRETURN
    case TString | TArray      | TObject(_) => ARETURN
    case TError  | TUnresolved | TUntyped   => ???
  }

  sealed trait Value {
    def load:  AbstractByteCodeGenerator
    def store: AbstractByteCodeGenerator
    def assign(ch: CodeHandler, pushValue: => Unit): Unit = {
      pushValue
      ch << store
    }
  }
  case class LocalVariable(val symbol: VariableSymbol, val id: Int) extends Value {
    override def load = symbol.tpe match {
      case TInt    | TBoolean                 => ILoad(id)
      case TString | TArray      | TObject(_) => ALoad(id)
      case TError  | TUnresolved | TUntyped   => ???
    }
    override def store = symbol.tpe match {
      case TInt    | TBoolean                 => IStore(id)
      case TString | TArray      | TObject(_) => AStore(id)
      case TError  | TUnresolved | TUntyped   => ???
    }
  }
  case class Field(val clazz: ClassSymbol, val varSym: VariableSymbol) extends Value {
    override def load  = GetField(getJvmClassName(clazz), varSym.name, typeToString(varSym.tpe))
    override def store = PutField(getJvmClassName(clazz), varSym.name, typeToString(varSym.tpe))
    private def loadObject = ALoad(0)
    override def assign(ch: CodeHandler, pushValue: => Unit) = {
      ch << loadObject
      pushValue
      ch << store
    }
  }

  case class InstructionSequence(
      val head: Option[Either[AbstractByteCode, AbstractByteCodeGenerator]],
      val tail: Option[InstructionSequence]
    ) {
    def <<:(h: AbstractByteCode) = InstructionSequence(Some(Left(h)), Some(this))
    def <<:(h: AbstractByteCodeGenerator) = InstructionSequence(Some(Right(h)), Some(this))
    def <<:(h: Either[AbstractByteCode, AbstractByteCodeGenerator]) = InstructionSequence(Some(h), Some(this))
    def <<:(pre: InstructionSequence): InstructionSequence = pre match {
      case InstructionSequence(Some(prehead), Some(pretail)) => prehead <<: (pretail <<: this)
      case InstructionSequence(Some(prehead), None)          => prehead <<: this
      case InstructionSequence(None, None)                   => this
    }
    def dumpInstructions(ch: CodeHandler): Unit =
      head map { wrapper =>
        wrapper match {
          case Left(abytecode)  => ch << abytecode
          case Right(generator) => ch << generator
        }
        tail map { tailSequence =>
          tailSequence.dumpInstructions(ch)
        }
      }
  }
  object InstructionSequence {
    def empty = InstructionSequence(None, None)
  }

  def getField(clazz: ClassSymbol, name: String): Field =
    clazz.members.get(name) map { Field(clazz, _) } getOrElse {
      (clazz.parent map { getField(_, name) }).get
    }

  def run(ctx: Context)(prog: Option[Program]): Unit = {
    import ctx.reporter._

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

      classFile.writeToFile("./" + ct.id.value + ".class")
    }

    // a mapping from variable symbols to positions in the local variables
    // of the stack frame
    def generateMethodCode(ch: CodeHandler, mt: MethodDecl): Unit = {
      val methSym = mt.symbol
      val variables: Map[String, LocalVariable] = (methSym.members ++ methSym.params) map {
        case (name, sym) => (name, LocalVariable(sym, ch.getFreshVar))
      }

      def lookupVar(name: String): Value =
        variables.get(name) getOrElse getField(methSym.classSymbol, name)

      mt.stats foreach { compileStat(ch, _, lookupVar _) }
      compileExpr(ch, mt.retExpr, lookupVar _)

      ch << returnInstruction(mt)

      // TODO: Emit code
      println(">>>>> " + mt.id.value)
      ch.print
      println(">>>>> " + mt.id.value)
      //ch.freeze
    }

    def generateMainMethodCode(ch: CodeHandler, stmts: List[StatTree], cname: String): Unit = {

      stmts foreach { compileStat(ch, _, (_ => ???)) }
      // TODO: Emit code
      ch << RETURN
      println(">>>>> " + "main")
      ch.print
      //ch.freeze
    }

    val outDir = ctx.outDir.map(_.getPath + "/").getOrElse("./")

    val f = new java.io.File(outDir)
    if(!f.exists()) {
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
    classFile.writeToFile("./" + main.id.value + ".class")
  }

  def compileStat(ch: CodeHandler, stmt: StatTree, lookupVar: (String => Value)): Unit = {
    stmt match {
      case Block(stats) => stats foreach {compileStat(ch, _, lookupVar)}
      case If(expr, thn, els) => {
        val nAfter = ch.getFreshLabel("after")
        val nElse = ch.getFreshLabel("else")
        compileExpr(ch, expr, lookupVar)
        ch << IfEq(nElse)
        compileStat(ch, thn, lookupVar)
        ch << Goto(nAfter)
        ch << Label(nElse)
        els match {
          case Some(stat) => compileStat(ch, stat, lookupVar)
          case None =>
        }
        ch << Label(nAfter)
        }
      case While(expr, stat) =>
      case Println(expr) => {
        ch << GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;")
        compileExpr(ch, expr, lookupVar)
        ch << InvokeVirtual("java/io/PrintStream", "println", "(I)V")
      }
      case Assign(id, expr) => {
        lookupVar(id.value).assign(ch, { compileExpr(ch, expr, lookupVar) })
      }
      case ArrayAssign(id, index, expr) =>
    }
  }


  def compileExpr(ch: CodeHandler, expr: ExprTree, lookupVar: (String => Value)): Unit = {
    expr match {
      case And(lhs, rhs) => {
        val nAfter = ch.getFreshLabel("after")
        val nElse = ch.getFreshLabel("else")
        compileExpr(ch, lhs, lookupVar)
        ch << IfEq(nElse)
        compileExpr(ch, rhs, lookupVar)
        ch << Goto(nAfter)
        ch << Label(nElse)
        ch << ICONST_0
        ch << Label(nAfter)
      }
      case Or(lhs, rhs) => {
        val nAfter = ch.getFreshLabel("after")
        val nElse = ch.getFreshLabel("else")
        compileExpr(ch, lhs, lookupVar)
        ch << IfEq(nElse)
        ch << ICONST_1
        ch << Goto(nAfter)
        ch << Label(nElse)
        compileExpr(ch, rhs, lookupVar)
        ch << Label(nAfter)
      }
      case Plus(lhs, rhs) => {
        compileExpr(ch, lhs, lookupVar)
        compileExpr(ch, rhs, lookupVar)
        ch << IADD
      }
      case Minus(lhs, rhs) => {
        compileExpr(ch, lhs, lookupVar)
        compileExpr(ch, rhs, lookupVar)
        ch << ISUB
      }
      case Times(lhs, rhs) => {
        compileExpr(ch, lhs, lookupVar)
        compileExpr(ch, rhs, lookupVar)
        ch << IMUL
      }
      case Div(lhs, rhs) => {
        compileExpr(ch, lhs, lookupVar)
        compileExpr(ch, rhs, lookupVar)
        ch << IDIV
      }
      case LessThan(lhs, rhs) => {
        compileExpr(ch, lhs, lookupVar)
        compileExpr(ch, rhs, lookupVar)
        val nTrue = ch.getFreshLabel("true")
        val nAfter = ch.getFreshLabel("after")
        ch << If_ICmpLt(nTrue)
        ch << ICONST_0
        ch << Goto(nAfter)
        ch << Label(nTrue) << ICONST_1 << Label(nAfter)
      }
      case Equals(lhs, rhs) => {
        compileExpr(ch, lhs, lookupVar)
        compileExpr(ch, rhs, lookupVar)
        val nTrue = ch.getFreshLabel("true")
        val nAfter = ch.getFreshLabel("after")
        ch << If_ICmpEq(nTrue)
        ch << ICONST_0
        ch << Goto(nAfter)
        ch << Label(nTrue) << ICONST_1 << Label(nAfter)
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

      case IntLit(value)     => ch << Ldc(value)
      case StringLit(value)  => {}
      case True()            => ch << ICONST_1
      case False()           => ch << ICONST_0
      case Identifier(value) => ch << lookupVar(value).load
      case This()            =>

      case NewIntArray(size) =>
      case New(tpe)          => println("AAAAAAAAAAAAA>>>>>" + tpe)

      case Not(expr)  =>

    }
  }

}
