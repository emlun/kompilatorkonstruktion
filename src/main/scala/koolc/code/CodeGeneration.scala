package koolc
package code

import ast.Trees._
import analyzer.Symbols._
import analyzer.Types._
import cafebabe._
import AbstractByteCodes.{New => _, _}
import ByteCodes._
import utils._

object CodeGeneration extends Pipeline[Option[Program], Unit] {

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
    def assign(prepareValueInstructions: InstructionSequence): InstructionSequence =
      prepareValueInstructions <<: store <<: InstructionSequence.empty
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
    override def assign(prepareValueInstructions: InstructionSequence) =
      loadObject <<: prepareValueInstructions <<: store <<: InstructionSequence.empty
  }

  case class InstructionSequence(
      val head: Option[Either[AbstractByteCode, AbstractByteCodeGenerator]],
      val tail: Option[InstructionSequence]
    ) extends AbstractByteCodeGenerator {
    def <<:(h: AbstractByteCode) = InstructionSequence(Some(Left(h)), Some(this))
    def <<:(h: AbstractByteCodeGenerator) = InstructionSequence(Some(Right(h)), Some(this))
    def <<:(h: Either[AbstractByteCode, AbstractByteCodeGenerator]) = InstructionSequence(Some(h), Some(this))
    def <<:(pre: InstructionSequence): InstructionSequence = pre match {
      case InstructionSequence(Some(prehead), Some(pretail)) => prehead <<: (pretail <<: this)
      case InstructionSequence(Some(prehead), None)          => prehead <<: this
      case InstructionSequence(None, None)                   => this
    }
    override def apply(ch: CodeHandler): CodeHandler = {
      head map { wrapper =>
        wrapper match {
          case Left(abytecode)  => ch << abytecode
          case Right(generator) => ch << generator
        }
        tail map { tailSequence =>
          ch << tailSequence
        }
      }
      ch
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

      ch << (mt.stats map compileStat(ch.getFreshLabel _, lookupVar _))
            .foldRight(InstructionSequence.empty)(_ <<: _)
      ch << compileExpr(ch.getFreshLabel _, lookupVar _)(mt.retExpr)
      ch << returnInstruction(mt)

      // TODO: Emit code
      println(">>>>> " + mt.id.value)
      ch.print
      println(">>>>> " + mt.id.value)
      //ch.freeze
    }

    def generateMainMethodCode(ch: CodeHandler, stmts: List[StatTree], cname: String): Unit = {

      // TODO: Emit code
      ch << (stmts map compileStat(ch.getFreshLabel _, (_ => ???)))
            .foldRight(InstructionSequence.empty)(_ <<: _)
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

  def compileStat(makeLabel: (String => String), lookupVar: (String => Value))(stmt: StatTree): InstructionSequence = {
    val recurse: (StatTree => InstructionSequence) = compileStat(makeLabel, lookupVar)
    stmt match {
      case Block(stats) => (stats map recurse).foldRight(InstructionSequence.empty)(_ <<: _)
      case If(expr, thn, els) => {
        val nAfter = makeLabel("after")
        val nElse = makeLabel("else")
        compileExpr(makeLabel, lookupVar)(expr) <<:
          IfNe(nElse) <<:
          recurse(thn) <<:
          Goto(nAfter) <<:
          Label(nElse) <<:
          (els map recurse getOrElse InstructionSequence.empty) <<:
          Label(nAfter) <<:
          InstructionSequence.empty
      }
      case While(expr, stat) => ???
      case Println(expr) => {
        GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;") <<:
          compileExpr(makeLabel, lookupVar)(expr) <<:
          InvokeVirtual("java/io/PrintStream", "println", "(I)V") <<:
          InstructionSequence.empty
      }
      case Assign(id, expr) => lookupVar(id.value).assign(compileExpr(makeLabel, lookupVar)(expr))
      case ArrayAssign(id, index, expr) => ???
    }
  }


  def compileExpr(makeLabel: (String => String), lookupVar: (String => Value))(expr: ExprTree): InstructionSequence = {
    val recurse: (ExprTree => InstructionSequence) = compileExpr(makeLabel, lookupVar)
    expr match {
      case And(lhs, rhs) => {
        val nAfter = makeLabel("after")
        val nElse = makeLabel("else")
        recurse(lhs) <<:
          IfEq(nElse) <<:
          recurse(rhs) <<:
          Goto(nAfter) <<:
          Label(nElse) <<:
          ICONST_0 <<:
          Label(nAfter) <<:
          InstructionSequence.empty
      }
      case Or(lhs, rhs) => {
        val nAfter = makeLabel("after")
        val nElse = makeLabel("else")
        recurse(lhs) <<:
          IfEq(nElse) <<:
          ICONST_1 <<:
          Goto(nAfter) <<:
          Label(nElse) <<:
          recurse(rhs) <<:
          Label(nAfter) <<:
          InstructionSequence.empty
      }
      case Plus(lhs, rhs) => {
        recurse(lhs) <<:
          recurse(rhs) <<:
          IADD <<:
          InstructionSequence.empty
      }
      case Minus(lhs, rhs) => {
        recurse(lhs) <<:
          recurse(rhs) <<:
          ISUB <<:
          InstructionSequence.empty
      }
      case Times(lhs, rhs) => {
        recurse(lhs) <<:
          recurse(rhs) <<:
          IMUL <<:
          InstructionSequence.empty
      }
      case Div(lhs, rhs) => {
        recurse(lhs) <<:
          recurse(rhs) <<:
          IDIV <<:
          InstructionSequence.empty
      }
      case LessThan(lhs, rhs) => {
        val nTrue = makeLabel("true")
        val nAfter = makeLabel("after")
        recurse(lhs) <<:
          recurse(rhs) <<:
          If_ICmpLt(nTrue) <<:
          ICONST_0 <<:
          Goto(nAfter) <<:
          Label(nTrue) <<: ICONST_1 <<: Label(nAfter) <<:
          InstructionSequence.empty
      }
      case Equals(lhs, rhs) => {
        val nTrue = makeLabel("true")
        val nAfter = makeLabel("after")
        recurse(lhs) <<:
          recurse(rhs) <<:
          If_ICmpEq(nTrue) <<:
          ICONST_0 <<:
          Goto(nAfter) <<:
          Label(nTrue) <<: ICONST_1 <<: Label(nAfter) <<:
          InstructionSequence.empty
      }
      case ArrayRead(arr, index) => InstructionSequence.empty
      case ArrayLength(arr) => InstructionSequence.empty
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
        ???
      }

      case IntLit(value)     => Ldc(value) <<: InstructionSequence.empty
      case StringLit(value)  => Ldc(value) << InstructionSequence.empty
      case True()            => ICONST_1 <<: InstructionSequence.empty
      case False()           => ICONST_0 <<: InstructionSequence.empty
      case Identifier(value) => lookupVar(value).load <<: InstructionSequence.empty
      case This()            => ???

      case NewIntArray(size) => ???
      case New(tpe)          => {
        println("AAAAAAAAAAAAA>>>>>" + tpe)
        ???
      }

      case Not(expr)  => ???

    }
  }

}
