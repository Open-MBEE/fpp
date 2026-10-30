package fpp.compiler.tools

import fpp.compiler.analysis._
import fpp.compiler.ast._
import fpp.compiler.util._

/** What fpp-to-lean and fpp-to-sysml share. */
object ShellTool {

  /** The one topology the input files define. */
  def theTopology(a: Analysis, tulFiles: List[Ast.TransUnit]): Result.Result[(Symbol.Topology, Topology)] = {
    def ids(ms: List[Ast.ModuleMember]): List[AstNode.Id] = ms.flatMap(_.node._2 match {
      case Ast.ModuleMember.DefTopology(node) => List(node.id)
      case Ast.ModuleMember.DefModule(node) => ids(node.data.members)
      case _ => Nil
    })
    val defined = tulFiles.flatMap(tu => ids(tu.members)).toSet
    a.topologyMap.toList.filter { case (sym, _) => defined.contains(sym.getNodeId) }.sortBy(_._1.getNodeId) match {
      case List(t) => Right(t)
      case Nil => Left(CodeGenError.ShellError("the input files define no topology"))
      case ts => Left(CodeGenError.ShellError(
        "the input files define several topologies (" + ts.map(t => a.getQualifiedName(t._1).toString).mkString(", ") +
        "); pass the one to generate as an input file and the others with -i"))
    }
  }

  def writeOutput(output: Option[String], text: String): Result.Result[Unit] = output match {
    case None => Right(print(text))
    case Some(path) =>
      for (w <- File.fromString(path).openWrite()) yield {
        w.print(text)
        w.close()
      }
  }

}
