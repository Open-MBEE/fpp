package fpp.compiler.tools

import fpp.compiler.analysis._
import fpp.compiler.ast._
import fpp.compiler.codegen._
import fpp.compiler.syntax._
import fpp.compiler.transform._
import fpp.compiler.util._
import scopt.OParser

/** Writes the shell of one topology — its port types, components with their ports and external
 *  contract, instances and connections — as Lean 4, so that an implementation of a component in
 *  that language can be checked against what the FPP model fixes. */
object FPPToLean {

  case class Options(
    files: List[File] = Nil,
    imports: List[File] = Nil,
    output: Option[String] = None,
    namespace: String = "FppGen",
    source: Option[String] = None,
  )

  def command(options: Options) = {
    fpp.compiler.util.Error.setTool(Tool(name))
    val files = options.files.reverse match {
      case Nil  => List(File.StdIn)
      case list => list
    }
    val a = Analysis(inputFileSet = options.files.toSet)
    for {
      tul <- ToolUtils.parseFiles(files ++ options.imports)
      (a, tul) <- CheckSemantics.tuList(a, tul)
      tulFiles = tul.take(files.length)
      topology <- ShellTool.theTopology(a, tulFiles)
      // Definitions are listed dependencies first: the imports, then the input files
      model <- ShellModel(a, tul.drop(files.length) ++ tulFiles, topology._1, topology._2)
      source = options.source.getOrElse(files.map(_.toString).mkString(" "))
      text <- LeanShellWriter.write(model, options.namespace, source)
      _ <- ShellTool.writeOutput(options.output, text)
    } yield ()
  }

  def toolMain(args: Array[String]) =
    Tool(name).mainMethod(args, oparser, Options(), command)

  val builder = OParser.builder[Options]

  val name = "fpp-to-lean"

  val oparser = {
    import builder._
    OParser.sequence(
      programName(name),
      head(name, Version.v),
      help('h', "help").text("print this message and exit"),
      arg[String]("file ...")
        .unbounded()
        .optional()
        .action((f, c) => c.copy(files = File.fromString(f) :: c.files))
        .text(".fpp file(s) defining exactly one topology"),
      opt[Seq[String]]('i', "imports")
        .valueName("<file1>,<file2>...")
        .action((i, c) => c.copy(imports = i.toList.map(File.fromString(_))))
        .text("files to import"),
      opt[String]('o', "output")
        .valueName("<file>")
        .action((o, c) => c.copy(output = Some(o)))
        .text("output file (default: standard output)"),
      opt[String]('n', "namespace")
        .valueName("<name>")
        .action((n, c) => c.copy(namespace = n))
        .text("Lean namespace of the generated module (default: FppGen)"),
      opt[String]('s', "source")
        .valueName("<text>")
        .action((s, c) => c.copy(source = Some(s)))
        .text("how the generated header names its source (default: the input files)"),
    )
  }

}
