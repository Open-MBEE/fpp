package fpp.compiler.codegen

import fpp.compiler.ast._
import fpp.compiler.util._
import ShellModel._

/** Writes the structure of one topology as a SysML v2 package (standard textual notation, validated
 *  against OpenSysML `-strict -validate`): a `port def` per port type, an opaque `attribute def` per
 *  non-primitive FPP type in a nested `Types` package, a `part def` per component with one `port` per
 *  port instance (outputs conjugated, arrays expanded to `name_0 … name_{n-1}`) and its external
 *  contract as items, and a `part def` for the topology with its parts and `connect`s. */
object SysmlShellWriter {

  /** SysML v2 / KerML keywords an FPP name may collide with; such a name is written `'name'`. */
  val reserved: Set[String] = Set(
    "abstract", "accept", "action", "actor", "after", "alias", "all", "allocate", "allocation", "analysis",
    "and", "as", "assert", "assign", "assume", "at", "attribute", "bind", "binding", "by", "calc", "case",
    "comment", "concern", "connect", "connection", "constant", "constraint", "decide", "def", "default",
    "defined", "dependency", "derived", "do", "doc", "else", "end", "entry", "enum", "event", "exhibit",
    "exit", "expose", "false", "filter", "first", "flow", "for", "fork", "frame", "from", "hastype", "if",
    "implies", "import", "in", "include", "individual", "inout", "interface", "istype", "item", "join",
    "language", "loop", "merge", "message", "meta", "metadata", "nonunique", "not", "null", "objective",
    "occurrence", "of", "or", "ordered", "out", "package", "parallel", "part", "perform", "port", "private",
    "protected", "public", "readonly", "redefines", "ref", "references", "render", "rendering", "rep",
    "require", "requirement", "return", "satisfy", "send", "snapshot", "specializes", "stakeholder",
    "standard", "state", "subject", "subsets", "succession", "target", "then", "timeslice", "to", "transition",
    "true", "typed", "until", "use", "variant", "variation", "verification", "verify", "via", "view",
    "viewpoint", "when", "while", "xor"
  )

  def ident(s: String): String = {
    val s1 = s.replace(".", "_")
    if reserved.contains(s1) then s"'$s1'" else s1
  }

  def ctor(q: List[String]): String = ident(q.mkString("_"))

  /** FPP primitive types → `ScalarValues`; anything else is an opaque `attribute def` named after it. */
  def primitive(t: String): Option[String] = t match {
    case "U8" | "U16" | "U32" | "U64" | "I8" | "I16" | "I32" | "I64" => Some("Integer")
    case "F32" | "F64" => Some("Real")
    case "bool" => Some("Boolean")
    case "string" => Some("String")
    case _ => None
  }

  /** Opaque types live in the nested `Types` package: a port type and an FPP type may share a name (`Fw.Time`). */
  def typeRef(t: String): String =
    if t.startsWith("string[") then "String" else primitive(t).getOrElse(s"Types::${ident(t)}")

  def portDef(p: PortDef): String = {
    val params = p.params.map(a =>
      s"        ${if a.ref then "inout" else "in"} attribute ${ident(a.name)} : ${typeRef(a.ty)};")
    val ret = p.returns.toList.map(r => s"        out attribute result : ${typeRef(r)};")
    val body = params ++ ret
    if body.isEmpty then s"    port def ${ctor(p.qname)};"
    else s"    port def ${ctor(p.qname)} {\n" + body.mkString("\n") + "\n    }"
  }

  def componentKind(k: ComponentKind): String = k match {
    case ComponentKind.Passive => "passive"
    case ComponentKind.Queued => "queued"
    case ComponentKind.Active => "active"
  }

  def queueFullName(q: QueueFull): String = q match {
    case QueueFull.Assert => "assert"
    case QueueFull.Block => "block"
    case QueueFull.Drop => "drop"
    case QueueFull.Hook => "hook"
  }

  def portKind(k: PortKind): String = k match {
    case PortKind.AsyncInput(q) => s"async input, queue full: ${queueFullName(q)}"
    case PortKind.SyncInput => "sync input"
    case PortKind.GuardedInput => "guarded input"
    case PortKind.Output => "output"
  }

  /** The SysML port names of one FPP port instance: `name` for size 1, `name_0 … name_{n-1}` otherwise. */
  def portNames(p: PortSpec): List[String] =
    if p.size <= 1 then List(ident(p.name)) else (0 until p.size).toList.map(i => ident(s"${p.name}_$i"))

  private def error(msg: String) = Left(CodeGenError.ShellError(msg))

  /** The SysML port name of a connection endpoint, by the port's declared size. */
  def endpointName(m: Model, e: Endpoint): Result.Result[String] =
    for {
      i <- m.instance(e.inst).toRight(CodeGenError.ShellError(s"endpoint instance ${e.inst} is not in the topology"))
      c <- m.component(i.component).toRight(CodeGenError.ShellError(s"instance ${i.name}: component ${i.component} undefined"))
      p <- c.ports.find(_.name == e.port).toRight(CodeGenError.ShellError(s"instance ${i.name} has no port ${e.port}"))
      name <-
        if p.size <= 1 then Right(s"${ident(i.name)}.${ident(p.name)}")
        else if e.num < p.size then Right(s"${ident(i.name)}.${ident(s"${p.name}_${e.num}")}")
        else error(s"${i.name}.${p.name}[${e.num}]: port number out of range (size ${p.size})")
    } yield name

  /** The types a component's contract names (command and event parameters, channels, parameters). */
  def contractTypes(c: ComponentDef): List[String] =
    c.commands.flatMap(_.params.map(_.ty)) ++ c.events.flatMap(_.params.map(_.ty)) ++
      c.telemetry.map(_.ty) ++ c.parameters.map(_.ty)

  def commandKind(k: CommandKind): String = k match {
    case CommandKind.Async(_, q) => s"async, queue full ${queueFullName(q)}"
    case CommandKind.Guarded => "guarded"
    case CommandKind.Sync => "sync"
    case CommandKind.ParamSet => "param set"
    case CommandKind.ParamSave => "param save"
  }

  /** A SysML string literal. */
  def quoted(t: String): String = "\"" + t.replace("\\", "\\\\").replace("\"", "\\\"") + "\""

  def throttleNote(o: Option[BigInt]): String = o.map(n => s", throttle $n").getOrElse("")

  def defaultNote(o: Option[String]): String = o.map(d => s"  // default $d").getOrElse("")

  def attrLines(params: List[Param]): List[String] =
    params.map(p => s"            attribute ${ident(p.name)} : ${typeRef(p.ty)};${if p.ref then "  // ref" else ""}")

  /** A component's external contract as items of the part def: one per command, event, telemetry channel
   *  and parameter, each specializing the `Contract` definitions with its id (relative to the base id). */
  def contractLines(c: ComponentDef): List[String] = {
    val cmds = c.commands.flatMap(k =>
      List(s"        item cmd_${ident(k.name)} : Contract::Command {  // ${commandKind(k.kind)}",
           s"            attribute :>> opcode = ${k.opcode};") ++ attrLines(k.params) ++ List("        }"))
    val evrs = c.events.flatMap(e =>
      List(s"        item evr_${ident(e.name)} : Contract::Event {  // ${e.severity}${throttleNote(e.throttle)}",
           s"            attribute :>> id = ${e.id};",
           s"            attribute :>> format = ${quoted(e.format)};") ++ attrLines(e.params) ++ List("        }"))
    val tlms = c.telemetry.map(t =>
      s"        item tlm_${ident(t.name)} : Contract::Channel { attribute :>> id = ${t.id}; attribute value : ${typeRef(t.ty)}; }  // ${t.update}")
    val prms = c.parameters.map(p =>
      s"        item prm_${ident(p.name)} : Contract::Parameter { attribute :>> id = ${p.id}; attribute :>> setOpcode = ${p.setOpcode}; attribute :>> saveOpcode = ${p.saveOpcode}; attribute value : ${typeRef(p.ty)}; }${defaultNote(p.default)}")
    cmds ++ evrs ++ tlms ++ prms
  }

  def partDef(m: Model, c: ComponentDef): Result.Result[String] = {
    // An array `buffer: [2]` and a scalar `buffer_0` would both become `buffer_0`: refuse rather than emit twice.
    val names = c.ports.flatMap(portNames)
    names.find(n => names.count(_ == n) > 1) match {
      case Some(n) => error(s"component ${c.qname.mkString(".")}: port name $n is used twice once arrays are expanded")
      case None =>
        for {
          portLines <- Result.map(c.ports, (p: PortSpec) =>
            m.port(p.ty) match {
              case None => error(s"component ${c.qname}: port type ${p.ty} undefined")
              case Some(ty) =>
                val conj = if p.kind == PortKind.Output then "~" else ""
                val role = p.special.map(r => s"$r port, ").getOrElse("")
                Right(portNames(p).map(n => s"        port $n : $conj${ctor(ty.qname)};  // $role${portKind(p.kind)}"))
            })
        } yield {
          val contract = contractLines(c)
          s"    part def ${ctor(c.qname)} {  // ${componentKind(c.kind)} component ${c.qname.mkString(".")}\n" +
            portLines.flatten.mkString("\n") +
            (if contract.isEmpty then "" else "\n" + contract.mkString("\n")) + "\n    }"
        }
    }
  }

  def write(m: Model, pkg: String, source: String): Result.Result[String] = {
    if m.instances.isEmpty then return error("the topology has no component instances")
    val ports = m.usedPorts
    val comps = m.usedComponents
    val named = (ports.flatMap(p => p.params.map(_.ty) ++ p.returns.toList) ++ comps.flatMap(contractTypes))
      .filter(t => primitive(t).isEmpty && !t.startsWith("string[")).distinct
    val opaques = named.map(t => s"        attribute def ${ident(t)};  // FPP type $t, opaque here")
    val insts = m.instances.map { i =>
      val c = m.component(i.component).map(c => ctor(c.qname)).getOrElse("?")
      s"        part ${ident(i.name)} : $c;  // base id ${i.baseId}" + i.queueSize.map(q => s", queue size $q").getOrElse("")
    }
    val topology = m.topology.mkString(".")
    for {
      parts <- Result.map(comps, (c: ComponentDef) => partDef(m, c))
      conns <- Result.map(m.connections, (c: Connection) =>
        for {
          s <- endpointName(m, c.src)
          d <- endpointName(m, c.dst)
        } yield s"        connect $s to $d;  // ${c.group}")
    } yield s"""// `$topology` — the FPP model as SysML v2 structure.
// Generated by `fpp-to-sysml` from $source (fpp ${m.fppVersion}). Do not edit: regenerate. Port types, components
// with their ports, the topology's parts and connections — what every implementation of a component must agree
// on — and each component's external contract — commands, events, telemetry channels, parameters, ids relative
// to the base id — as items of its part def. Behaviour is not in FPP and not here. Output ports are conjugated;
// a port array of size n is n ports `name_i`.
package $pkg {
    private import ScalarValues::*;

    package Types {  // the FPP types the ports and the contracts use, opaque here (their layout is FPP's)
${opaques.mkString("\n")}
    }

    package Contract {  // what the dictionary publishes about a component; ids are relative to the instance's base id
        item def Command { attribute opcode : Natural; }
        item def Event { attribute id : Natural; attribute format : String; }
        item def Channel { attribute id : Natural; }
        item def Parameter { attribute id : Natural; attribute setOpcode : Natural; attribute saveOpcode : Natural; }
    }

${ports.map(portDef).mkString("\n\n")}

${parts.mkString("\n\n")}

    part def ${ctor(m.topology)} {
${insts.mkString("\n")}

${conns.mkString("\n")}
    }
}
"""
  }

}
