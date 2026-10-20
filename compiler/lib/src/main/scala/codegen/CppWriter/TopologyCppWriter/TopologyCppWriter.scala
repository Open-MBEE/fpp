package fpp.compiler.codegen

import fpp.compiler.analysis._
import fpp.compiler.ast._
import fpp.compiler.util._

/** Writes out C++ for topology definitions */
case class TopologyCppWriter(
  s: CppWriterState,
  aNode: Ast.Annotated[AstNode[Ast.DefTopology]]
) extends TopologyCppWriterUtils(s, aNode) {

  private val topComponents = TopComponents(s, aNode)

  def write: CppDoc = {
    val node = aNode._2
    val data = node.data
    val fileName = ComputeCppFiles.FileNames.getTopology(data.name)
    val includeGuard = s.includeGuardFromQualifiedName(symbol, fileName)
    CppWriter.createCppDoc(
      s"${data.name} topology",
      fileName,
      includeGuard,
      getMembers,
      s.toolName
    )
  }

  private def getMembers: List[CppDoc.Member] =
    List.concat(
      getIncludeMembers,
      getComponentInstanceMembers,
      getTopologyMembers,
      getComponentMembers
    )

  private def getComponentInstanceMembers =
    TopComponentInstances(s, aNode).getMembers

  private def getIncludeMembers: List[CppDoc.Member] = {
    val hpp = {
      val strings = List.concat(
        TopComponentIncludes(s, aNode).getHeaderStrings,
        TopTlmPacketIncludes(s, aNode).getHeaderStrings,
        List(
          CppWriter.headerString(
            s.getIncludePath(symbol, s"${name}TopologyDefs")
          )
        )
      ).sorted
      linesMember(Line.blank :: strings.map(line))
    }
    val cpp = {
      val fileName = ComputeCppFiles.FileNames.getTopology(name)
      val traceInclude = if topComponents.hasTypedOutputPorts then lines(
        s"""|#if FW_PORT_SPAN_TRACING
            |${CppWriter.headerString("Fw/Port/PortTrace.hpp")}
            |#endif
            |"""
      ) else Nil
      linesMember(
        List(Line.blank, CppWriter.headerLine(s.getIncludePath(symbol, fileName))) ++
          traceInclude,
        CppDoc.Lines.Cpp
      )
    }
    List(hpp, cpp)
  }

  private def getTopologyMembers: List[CppDoc.Member] = {
    val hppLines = linesMember(
      TopConstants(s, aNode).getLines ++
      TopConfigObjects(s, aNode).getHppLines ++
      getConnectionIdDeclaration
    )
    val cppLines = linesMember(
      TopConfigObjects(s, aNode).getCppLines,
      CppDoc.Lines.Cpp
    )
    val (helperFnNames, helperFns) = TopHelperFns(s, aNode).getMembers
    val setupTeardownFns =
      TopSetupTeardownFns(s, aNode, helperFnNames).getMembers
    val defs = hppLines :: cppLines :: (helperFns ++ setupTeardownFns)
    wrapInNamespaces(namespaceIdentList, defs)
  }

  private def getConnectionIdDeclaration: List[Line] = lines(
    """|#if FW_PORT_SPAN_TRACING
       |//! Return the dictionary id of the connection from an output port element, or -1 if none
       |FwIndexType getConnectionId(
       |  FwEnumStoreType instanceId, //!< The calling instance (InstanceIds)
       |  FwIndexType portId,         //!< The port trace id
       |  FwIndexType portNum         //!< The port number
       |);
       |#endif
       |"""
  )

  private def getComponentMembers: List[CppDoc.Member] =
    topComponents.getMembers

}
