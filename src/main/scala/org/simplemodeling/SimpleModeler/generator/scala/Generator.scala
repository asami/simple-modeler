package org.simplemodeling.SimpleModeler.generator.scala

import scalaz._, Scalaz.{modify => _, _}
import org.goldenport.scalaz.Recorder
import org.goldenport.scalaz.rwscr
import org.goldenport.scalaz.rwscr._
import org.goldenport.context.Consequence
import org.goldenport.context.Showable
import model._

/*
 * @since   May. 14, 2025
 *  version May. 19, 2025
 *  version Sep. 26, 2025
 *  version Oct. 17, 2025
 *  version Feb. 17, 2026
 * @version Mar. 13, 2026
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
    def println(p: String): Output = {
      val s = strings.mkString + p
      copy(lines = lines :+ s, strings = Vector.empty)
    }

    def println() = {
      val s = strings.mkString
      copy(lines = lines :+ s, strings = Vector.empty)
    }

    def print(p: String): Output = copy(strings = strings :+ p)

    def printws(p: String) = copy(strings = strings :+ p :+ " ")

    def separator() = println.copy(lines = lines :+ " ")

    def indent(width: Int) =
      if (strings.isEmpty)
        print(" " * width)
      else
        this

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
    width: Int = 2,
    output: Output = Output.empty
  ) {
    def up = copy(indent = indent + 1)
    def down = copy(indent = indent - 1)

    def println(p: String) = copy(output = output.indent(indent_width).println(p))
    def print(p: String) = copy(output = output.indent(indent_width).print(p))
    def printws(p: String) = copy(output = output.indent(indent_width).printws(p))
    def separator() = copy(output = output.separator())
    def add(p: Output) = copy(output = output + p)

    protected def indent_width = indent * width
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

  def println(s: String, ss: String*): GenM[Unit] = ReaderWriterStateT { (config, state) =>
    Consequence.success((Recorder.empty, (), state.println(s + ss.mkString)))
  }

  def println(line: Showable): GenM[Unit] = println(line.print)

  def println(): GenM[Unit] = println("")

  def print(s: String, ss: String*): GenM[Unit] = ReaderWriterStateT { (config, state) =>
    Consequence.success((Recorder.empty, (), state.print(s + ss.mkString)))
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

  def block(prefix: String)(body: => GenM[Unit]): GenM[Unit] = {
    val s = prefix.trim
    if (s.endsWith("(") || s.endsWith("."))
      blockSimple(prefix)(body)
    else
      blockOpenClose(prefix)(body)
  }

  def blockR[T](prefix: String)(body: => GenM[T]): GenM[T] = {
    val s = prefix.trim
    if (s.endsWith("(") || s.endsWith("."))
      blockSimpleR(prefix)(body)
    else
      blockOpenCloseR(prefix)(body)
  }

  def blockOpenClose(prefix: String)(body: => GenM[Unit]): GenM[Unit] =
    blockOpenCloseR[Unit](prefix)(body)

  def blockOpenCloseR[T](prefix: String)(body: => GenM[T]): GenM[T] = {
    val head =
      if (prefix.trim.isEmpty)
        "{"
      else if (prefix.trim.endsWith("{"))
        prefix
      else if (prefix.endsWith(" "))
        prefix + "{"
      else
        prefix + " {"
    for {
      _ <- println(head)
      _ <- indent
      r <- body
      _ <- outdent
      _ <- println("}")
    } yield r
  }

  def blockFor(body: => GenM[Unit])(output: => GenM[Unit]): GenM[Unit] =
    for {
      _ <- println("for {")
      _ <- indent
      r <- body
      _ <- outdent
      _ <- println("} yield {")
      _ <- indent
      _ <- output
      _ <- outdent
      _ <- println("}")
    } yield r

  def blockFor(body: String, bodys: String*)(output: String, outputs: String*): GenM[Unit] =
    blockFor(_println_block(body +: bodys))(_println_block(output +: outputs))

  private def _println_block(ps: Seq[String]): GenM[Unit] =
    ps.toVector.foldLeft(unit) { (z, s) =>
      for {
        _ <- z
        _ <- println(s)
      } yield ()
    }

  def blockSimple(prefix: String)(body: => GenM[Unit]): GenM[Unit] =
    blockSimpleR[Unit](prefix)(body)

  def blockSimpleR[T](prefix: String)(body: => GenM[T]): GenM[T] = {
    for {
      _ <- println(prefix)
      _ <- indent
      r <- body
      _ <- outdent
    } yield r
  }

  def blockAfter(prefix: GenM[Unit])(body: => GenM[Unit]): GenM[Unit] =
    for {
      _ <- prefix
      _ <- block("")(body)
    } yield ()

  def blockInline(prefix: String)(body: => GenM[Unit]): GenM[Unit] =
    for {
      _ <- print(
        if (prefix.endsWith(" "))
          prefix + "{ "
        else
          prefix + " { "
      )
      _ <- body
      _ <- println(" }")
    } yield ()

  def blockExpression(prefix: String)(body: Seq[String]): GenM[Unit] =
    for {
      _ <- println(s"$prefix(")
      _ <- intercalateTraverse_(body, println(","))(x => print(x))
      _ <- println(")")
    } yield()

  // def blockIfNonEmpty(prefix: String)(body: => GenM[Unit]): GenM[Unit] =
  //   for {
  //     before <- get
  //     _ <- block(prefix)(body)
  //     after <- get
  //     _ <-
  //       if (before.output == after.output)
  //         unit
  //       else
  //         unit
  //   } yield ()

  def braced(body: => GenM[Unit]): GenM[Unit] =
    block("")(body)

  def intercalateTraverse[A, B](
    xs: Seq[A],
    sep: GenM[B]
  )(f: A => GenM[B]): GenM[Vector[B]] =
    rwscr.intercalateTraverse[Config, State, A, B](xs, sep)(f)

  def intercalateTraverse_[A](
    xs: Seq[A],
    sep: GenM[Unit]
  )(f: A => GenM[Unit]): GenM[Unit] =
    rwscr.intercalateTraverse_[Config, State, A](xs, sep)(f)

  def build: GenM[String] = ReaderWriterStateT { (config, state) =>
    for {
      r <- state.output.toString(config)
    } yield (Recorder.empty, r, state)
  }
}
