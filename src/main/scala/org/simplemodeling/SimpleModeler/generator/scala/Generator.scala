package org.simplemodeling.SimpleModeler.generator.scala

import scalaz._, Scalaz.{modify => _, _}
import org.goldenport.scalaz.Recorder
import org.goldenport.scalaz.rwscr._
import org.goldenport.context.Consequence
import org.goldenport.context.Showable
import model._

/*
 * @since   May. 14, 2025
 * @version May. 19, 2025
 * @author  ASAMI, Tomoharu
 */
trait Generator[A, R] {
  def run(a: A): Generator.GenM[R]
}

object Generator {
  case class Config(
    indentSize: Int = 2,
    newline: String = "\n"
  )

  case class Output(
    lines: Vector[String] = Vector.empty,
    strings: Vector[String] = Vector.empty
  ) {
    def println(p: String) = {
      val s = strings.mkString + p
      copy(lines = lines :+ s, strings = Vector.empty)
    }

    def println() = {
      val s = strings.mkString
      copy(lines = lines :+ s, strings = Vector.empty)
    }

    def print(p: String) = copy(strings = strings :+ p)

    def printws(p: String) = copy(strings = strings :+ p :+ " ")

    def separator() = println().copy(lines = lines :+ " ")

    def +(rhs: Output): Output =
      (rhs.lines.isEmpty, rhs.strings.isEmpty) match {
        case (true, true) => this
        case (true, false) => copy(strings = strings ++ rhs.strings)
        case (false, true) =>
          val s = strings.mkString + rhs.lines.head
          copy(lines = (lines :+ s) ++ rhs.lines.tail, strings = Vector.empty)
        case (false, false) =>
          val s = strings.mkString + rhs.lines.head
          copy(lines = (lines :+ s) ++ rhs.lines.tail, strings = rhs.strings)
      }

    def toString(config: Config): Consequence[String] = Consequence {
      val nl = config.newline
      val xs = lines :+ strings.mkString
      xs.mkString("", nl, nl)
    }
  }
  object Output {
    val empty = Output()

    implicit object OutputMonoid extends Monoid[Output] {
      def zero = empty
      def append(lhs: Output, rhs: => Output) = lhs + rhs
    }

    def println(p: String): Output = Output(lines = Vector(p))
    def println(p: Showable): Output = println(p.print)
    def print(p: String): Output = Output(strings = Vector(p))
    def print(p: Showable): Output = print(p.print)
    def printws(p: String): Output = Output(strings = Vector(p, " "))
    def printws(p: Showable): Output = printws(p.print)
    def separator(): Output = Output(lines = Vector("")) // TODO
  }

  case class State(
    indent: Int = 0,
    output: Output = Output.empty
  ) {
    def up = copy(indent = indent + 1)
    def down = copy(indent = indent - 1)

    def println(p: String) = copy(output = output.println(p))
    def print(p: String) = copy(output = output.print(p))
    def printws(p: String) = copy(output = output.printws(p))
    def separator() = copy(output = output.separator())
    def add(p: Output) = copy(output = output + p)
  }

  type GenM[A] = RWSCR[Config, State, A]

  def apply[A, R](implicit G: Generator[A, R]): Generator[A, R] = G

  // def ask: GenM[Config] = ReaderWriterStateT { (config, state) =>
  //   Consequence.success((Output.empty, config, state))
  // }

  // def ask[A](f: Config => A): GenM[A] = ReaderWriterStateT { (config, state) =>
  //   Consequence((Output.empty, f(config), state))
  // }

  // def tell(w: Output): GenM[Unit] = ReaderWriterStateT { (config, state) =>
  //   Consequence((w, (), state))
  // }

  // def get: GenM[State] = ReaderWriterStateT { (config, state) =>
  //   Consequence((Output.empty, state, state))
  // }

  // def set(s: State): GenM[Unit] = ReaderWriterStateT { (config, state) =>
  //   Consequence((Output.empty, (), s))
  // }

  def unit: GenM[Unit] = ReaderWriterStateT { (config, state) =>
    Consequence.success((Recorder.empty, (), state))
  }

  def println(line: String): GenM[Unit] = ReaderWriterStateT { (config, state) =>
    Consequence.success((Recorder.empty, (), state.println(line)))
  }

  def println(line: Showable): GenM[Unit] = println(line.print)

  def println(): GenM[Unit] = println("")

  def print(s: String): GenM[Unit] = ReaderWriterStateT { (config, state) =>
    Consequence.success((Recorder.empty, (), state.print(s)))
  }

  def print(p: Showable): GenM[Unit] = print(p.print)

  def printws(s: String): GenM[Unit] = ReaderWriterStateT { (config, state) =>
    Consequence.success((Recorder.empty, (), state.printws(s)))
  }

  def printws(p: Showable): GenM[Unit] = printws(p.print)

  def separator(): GenM[Unit] = ReaderWriterStateT { (config, state) =>
    Consequence.success((Recorder.empty, (), state.separator()))
  }

  def add(p: Output): GenM[Unit] = ReaderWriterStateT { (config, state) =>
    Consequence.success((Recorder.empty, (), state.add(p)))
  }

  // def modify(f: State => State): GenM[Unit] = ReaderWriterStateT { (r, s) =>
  //   Consequence((Recorder.empty, (), f(s)))
  // }

  def indent: GenM[Unit] = modify(x => x.up)

  def outdent: GenM[Unit] = modify(_.down)

  def build: GenM[String] = ReaderWriterStateT { (config, state) =>
    for {
      r <- state.output.toString(config)
    } yield (Recorder.empty, r, state)
  }

  // implicit object Scala3ClassGenerator extends Generator[SClassBase] {
  // }

  // trait Scala3ClassGeneratorBase extends Generator[SClassBase] {
  //   def run(ast: SClassBase): GenM[Unit] =
  //     for {
  //       _ <- section_package(ast)
  //       _ <- separator
  //       _ <- section_import(ast)
  //       _ <- separator
  //       _ <- section_class(ast)
  //       _ <- separator
  //       _ <- section_object(ast)
  //     } yield ()

  //   protected def section_package(p: SClassBase): GenM[Unit] =
  //     for {
  //       _ <- print("package ")
  //       _ <- println(p.packageName)
  //     } yield ()

  //   protected def section_import(p: SClassBase): GenM[Unit] =
  //     for {
  //       _ <- {
  //         val o = p.importNames.foldLeft(Output.empty)((z, x) =>
  //           z.print("import ").println(x.fullName))
  //         add(o)
  //       }
  //     } yield ()

  //   protected def section_class(p: SClassBase): GenM[Unit] =
  //     for {
  //       _ <- printws(p.declaration)
  //       _ <- print(p.className)
  //       _ <- {
  //         val ps = p.parameterSequence
  //         val s = ps.parameters.map(x => x.name.name + ": " + x.typeName.name).mkString("(", ", ", ")")
  //         print(s)
  //       }
  //       _ <- {
  //         def _extends_(c: String, ts: List[String]) =
  //           s" extends ${c}" + ts.mkString(" with", " with ", " ")
  //         val s = (p.parentClass, p.traitList) match {
  //           case (Some(s), Nil) => s" extends ${s.name} "
  //           case (Some(s), xs) => _extends_(s.name, xs.map(_.name))
  //           case (None, Nil) => " "
  //           case (None, x :: xs) => _extends_(x.name, xs.map(_.name))
  //         }
  //         print(s)
  //       }
  //       _ <- println("{")
  //       _ <- indent
  //       _ <- section_variables(p)
  //       _ <- separator
  //       _ <- section_methods(p)
  //       _ <- separator
  //       _ <- section_reception(p)
  //       _ <- outdent
  //       _ <- println("}")
  //     } yield ()

  //   protected def section_variables(p: SClassBase): GenM[Unit] =
  //     ???

  //   protected def section_methods(p: SClassBase): GenM[Unit] =
  //     ???

  //   protected def section_methods(p: SMethod): GenM[Unit] =
  //     ???

  //   protected def section_reception(p: SClassBase): GenM[Unit] =
  //     ???

  //   protected def section_object(p: SClassBase): GenM[Unit] =
  //     for {
  //       _ <- print("object ")
  //       _ <- print(p.className)
  //       _ <- println(" {")
  //       _ <- indent
  //       _ <- outdent
  //       _ <- println("}")
  //     } yield ()
  // }
}

// object RWSTUtil {
//   def modify[F[_]: Applicative, R, W: Monoid, S](f: S => S): ReaderWriterStateT[F, R, W, S, Unit] =
//     ReaderWriterStateT { (r, s) =>
//       (Monoid[W].zero, (), f(s)).pure[F]
//     }

//   def get[F[_]: Applicative, R, W: Monoid, S]: ReaderWriterStateT[F, R, W, S, S] =
//     ReaderWriterStateT { (r, s) =>
//       (Monoid[W].zero, s, s).pure[F]
//     }

//   def put[F[_]: Applicative, R, W: Monoid, S](newState: S): ReaderWriterStateT[F, R, W, S, Unit] =
//     ReaderWriterStateT { (r, s) =>
//       (Monoid[W].zero, (), newState).pure[F]
//     }
// }
