package fpp.compiler.codegen

import fpp.compiler.analysis._
import fpp.compiler.ast._
import fpp.compiler.util._

/** The part of an FPP model that an implementation of a component in another language must agree
 *  on: port types, components with their ports and external contract (commands, events, telemetry
 *  channels, parameters; ids relative to the instance's base id), the instances of one topology and
 *  its connections with their port numbers. Behaviour is not in FPP, so it is not here. */
object ShellModel {

  final case class Param(name: String, ty: String, ref: Boolean)
  final case class PortDef(id: AstNode.Id, qname: List[String], params: List[Param], returns: Option[String])

  enum QueueFull { case Assert, Block, Drop, Hook }
  enum PortKind {
    case AsyncInput(priority: Option[BigInt], queueFull: QueueFull)
    case SyncInput, GuardedInput, Output
  }
  final case class PortSpec(name: String, kind: PortKind, ty: AstNode.Id, size: Int, special: Option[String])
  enum ComponentKind { case Passive, Queued, Active }
  enum CommandKind {
    case Async(priority: Option[BigInt], queueFull: QueueFull)
    case Guarded, Sync, ParamSet, ParamSave
  }
  final case class CommandDef(name: String, opcode: BigInt, kind: CommandKind, params: List[Param])
  final case class EventDef(name: String, id: BigInt, severity: String, params: List[Param], format: String, throttle: Option[BigInt])
  final case class TlmChannelDef(name: String, id: BigInt, ty: String, update: String, format: Option[String])
  final case class ParamDef(name: String, id: BigInt, ty: String, default: Option[String], setOpcode: BigInt, saveOpcode: BigInt)
  final case class ComponentDef(
    id: AstNode.Id, qname: List[String], kind: ComponentKind, ports: List[PortSpec],
    commands: List[CommandDef], events: List[EventDef], telemetry: List[TlmChannelDef], parameters: List[ParamDef]
  )
  /** `name` is the instance's identifier in the shells: its FPP base name when no other instance of the
   *  topology has the same base name, its qualified name flattened with `_` otherwise; `qname` is the FPP name. */
  final case class InstanceDef(id: AstNode.Id, name: String, qname: List[String], component: AstNode.Id, baseId: BigInt,
    queueSize: Option[BigInt], priority: Option[BigInt])
  final case class Endpoint(inst: AstNode.Id, port: String, num: Int)
  final case class Connection(group: String, src: Endpoint, dst: Endpoint)
  final case class Model(
    fppVersion: String, topology: List[String], ports: List[PortDef], components: List[ComponentDef],
    instances: List[InstanceDef], connections: List[Connection]
  ) {
    def port(id: AstNode.Id): Option[PortDef] = ports.find(_.id == id)
    def component(id: AstNode.Id): Option[ComponentDef] = components.find(_.id == id)
    def instance(id: AstNode.Id): Option[InstanceDef] = instances.find(_.id == id)
    /** The port types the topology's components use, in definition order. */
    /** The port types the topology's components use, in order of first use. */
    def usedPorts: List[PortDef] =
      usedComponents.flatMap(_.ports.map(_.ty)).distinct.flatMap(port)
    /** The components the topology instantiates, in order of first instantiation. */
    def usedComponents: List[ComponentDef] =
      instances.map(_.component).distinct.flatMap(component)
  }

  def endpointLe(a: Endpoint, b: Endpoint): Boolean =
    a.inst < b.inst || (a.inst == b.inst && (a.port < b.port || (a.port == b.port && a.num <= b.num)))
  def connectionLe(a: Connection, b: Connection): Boolean =
    if a.src == b.src then endpointLe(a.dst, b.dst) else endpointLe(a.src, b.src)

  /** The text of a value the analysis evaluated: what the dictionary publishes (a constant's name is
   *  resolved, an enum constant is `Enum.Member` by the enum's qualified name), strings unquoted. */
  def valueText(a: Analysis, v: Value): String = v match {
    case Value.PrimitiveInt(n, _) => n.toString
    case Value.Integer(n) => n.toString
    case Value.Float(f, _) => f.toString
    case Value.Boolean(b) => b.toString
    case Value.String(s) => s
    case Value.EnumConstant((name, _), t) => s"${a.getQualifiedName(Symbol.Enum(t.node))}.$name"
    case other => other.toString
  }

  /** The text of a default expression: its evaluated value, or the literal when the analysis holds none. */
  def defaultText(a: Analysis, node: AstNode[Ast.Expr]): Result.Result[String] =
    a.valueMap.get(node.id).map(valueText(a, _)) match {
      case Some(t) => Right(t)
      case None => node.data match {
        case Ast.ExprLiteralInt(v) => Right(v)
        case Ast.ExprLiteralFloat(v) => Right(v)
        case Ast.ExprLiteralString(v) => Right(v)
        case Ast.ExprLiteralBool(v) => Right(v.toString)
        case _ => Left(CodeGenError.ShellError(s"${Locations.get(node.id)}: the default expression has no evaluated value"))
      }
    }

  /** A qualified name flattened to one identifier, as both shells write it. */
  def flat(q: List[String]): String = q.mkString("_")

  /** A sized string's size is the evaluated expression, so `string size MAX` reads `string[40]`. */
  def typeName(a: Analysis, tn: Ast.TypeName): String = tn match {
    case Ast.TypeNameInt(name) => name.toString
    case Ast.TypeNameFloat(name) => name.toString
    case Ast.TypeNameBool => "bool"
    case Ast.TypeNameString(size) => size match {
      case Some(n) => s"string[${defaultText(a, n).getOrElse(n.data.toString)}]"
      case None => "string"
    }
    case Ast.TypeNameQualIdent(name) => name.data.toIdentList.mkString(".")
  }

  def params(a: Analysis, ps: Ast.FormalParamList): List[Param] =
    ps.map { case (_, node, _) =>
      Param(node.data.name, typeName(a, node.data.typeName.data), node.data.kind == Ast.FormalParam.Ref)
    }

  def queueFull(qf: Ast.QueueFull): QueueFull = qf match {
    case Ast.QueueFull.Assert => QueueFull.Assert
    case Ast.QueueFull.Block => QueueFull.Block
    case Ast.QueueFull.Drop => QueueFull.Drop
    case Ast.QueueFull.Hook => QueueFull.Hook
  }

  def severity(s: Ast.SpecEvent.Severity): String = s.toString

  /** Port definitions in source order, with their qualified names. */
  def portDefs(a: Analysis, tul: List[Ast.TransUnit]): List[PortDef] = {
    def members(q: List[String], ms: List[Ast.ModuleMember]): List[PortDef] = ms.flatMap { m =>
      m.node._2 match {
        case Ast.ModuleMember.DefModule(node) => members(q :+ node.data.name, node.data.members)
        case Ast.ModuleMember.DefPort(node) =>
          val d = node.data
          List(PortDef(node.id, q :+ d.name, params(a, d.params), d.returnType.map(t => typeName(a, t.data))))
        case _ => Nil
      }
    }
    tul.flatMap(tu => members(Nil, tu.members))
  }

  def componentKind(k: Ast.ComponentKind): ComponentKind = k match {
    case Ast.ComponentKind.Passive => ComponentKind.Passive
    case Ast.ComponentKind.Queued => ComponentKind.Queued
    case Ast.ComponentKind.Active => ComponentKind.Active
  }

  def portSpec(p: PortInstance): Result.Result[PortSpec] = p match {
    case g: PortInstance.General =>
      val kind = g.kind match {
        case PortInstance.General.Kind.AsyncInput(priority, qf) => PortKind.AsyncInput(priority, queueFull(qf))
        case PortInstance.General.Kind.SyncInput => PortKind.SyncInput
        case PortInstance.General.Kind.GuardedInput => PortKind.GuardedInput
        case PortInstance.General.Kind.Output => PortKind.Output
      }
      g.ty match {
        case PortInstance.Type.DefPort(sym) => Right(PortSpec(g.getUnqualifiedName, kind, sym.getNodeId, g.size, None))
        case PortInstance.Type.Serial =>
          Left(SemanticError.InvalidPortInstance(g.getLoc, "serial ports are not supported by the shell generators", g.getLoc))
      }
    case s: PortInstance.Special =>
      import Ast.SpecPortInstance._
      val role = s.specifier.kind.toString
      val kind = s.specifier.kind match {
        case CommandRecv | ProductRecv =>
          s.specifier.inputKind match {
            case Some(Sync) => PortKind.SyncInput
            case Some(Guarded) => PortKind.GuardedInput
            case _ => PortKind.AsyncInput(s.priority, s.queueFull.map(queueFull).getOrElse(QueueFull.Assert))
          }
        case _ => PortKind.Output
      }
      Right(PortSpec(s.getUnqualifiedName, kind, s.symbol.getNodeId, 1, Some(role)))
    case t: PortInstance.Topology =>
      Left(SemanticError.InvalidPortInstance(t.getLoc, "topology ports are not supported by the shell generators", t.getLoc))
  }

  def commands(a: Analysis, c: Component): List[CommandDef] =
    c.commandMap.toList.sortBy(_._1).map { case (opcode, cmd) =>
      cmd match {
        case Command.NonParam(aNode, kind) =>
          val d = aNode._2.data
          val k = kind match {
            case Command.NonParam.Async(priority, qf) => CommandKind.Async(priority, queueFull(qf))
            case Command.NonParam.Guarded => CommandKind.Guarded
            case Command.NonParam.Sync => CommandKind.Sync
          }
          CommandDef(d.name, opcode, k, params(a, d.params))
        case Command.Param(aNode, kind) =>
          val name = aNode._2.data.name.toUpperCase
          kind match {
            case Command.Param.Set => CommandDef(s"${name}_PRM_SET", opcode, CommandKind.ParamSet, Nil)
            case Command.Param.Save => CommandDef(s"${name}_PRM_SAVE", opcode, CommandKind.ParamSave, Nil)
          }
      }
    }

  def events(a: Analysis, c: Component): List[EventDef] =
    c.eventMap.toList.sortBy(_._1).map { case (id, e) =>
      val d = e.aNode._2.data
      EventDef(d.name, id, severity(d.severity), params(a, d.params), d.format.data, e.throttle.map(t => BigInt(t.count)))
    }

  def telemetry(a: Analysis, c: Component): List[TlmChannelDef] =
    c.tlmChannelMap.toList.sortBy(_._1).map { case (id, t) =>
      val d = t.aNode._2.data
      val update = t.update match {
        case Ast.SpecTlmChannel.Always => "always"
        case Ast.SpecTlmChannel.OnChange => "on change"
      }
      TlmChannelDef(d.name, id, typeName(a, d.typeName.data), update, d.format.map(_.data))
    }

  def parameters(a: Analysis, c: Component): Result.Result[List[ParamDef]] =
    Result.map(c.paramMap.toList.sortBy(_._1), (id: BigInt, p: fpp.compiler.analysis.Param) => {
      val d = p.aNode._2.data
      Result.mapOpt(d.default, (e: AstNode[Ast.Expr]) => defaultText(a, e)).map(default =>
        ParamDef(d.name, id, typeName(a, d.typeName.data), default, p.setOpcode, p.saveOpcode))
    })

  def componentDef(a: Analysis, sym: Symbol.Component, c: Component, qname: List[String]): Result.Result[ComponentDef] =
    for {
      ports <- Result.map(c.portMap.toList.sortBy(_._1).map(_._2), portSpec)
      params <- parameters(a, c)
    } yield ComponentDef(sym.getNodeId, qname, componentKind(c.aNode._2.data.kind), ports,
      commands(a, c), events(a, c), telemetry(a, c), params)

  /** Two FPP names that flatten to the same identifier cannot both be in a shell. */
  def checkNames(m: Model): Result.Result[Unit] = {
    def check(what: String, named: List[(List[String], String)]): Result.Result[Unit] =
      named.groupBy(_._2).toList.sortBy(_._1).find(_._2.length > 1) match {
        case Some((id, clashing)) =>
          Left(CodeGenError.ShellError(
            s"$what ${clashing.map(_._1.mkString(".")).mkString(" and ")} would all be written $id in the shells"))
        case None => Right(())
      }
    for {
      _ <- check("port types", m.usedPorts.map(p => (p.qname, flat(p.qname))))
      _ <- check("components", m.usedComponents.map(c => (c.qname, flat(c.qname))))
      _ <- check("instances", m.instances.map(i => (i.qname, i.name)))
    } yield ()
  }

  /** The model of one topology. */
  def apply(a: Analysis, tul: List[Ast.TransUnit], tSym: Symbol.Topology, t: Topology): Result.Result[Model] = {
    if t.portMap.nonEmpty then
      return Left(CodeGenError.ShellError(
        s"topology ${a.getQualifiedName(tSym)} exposes ports (${t.portMap.keys.toList.sorted.mkString(", ")}): " +
          "topology ports are not supported by the shell generators"))
    val inTopology: Set[AstNode.Id] =
      t.instanceMap.keys.flatMap(_.getComponentInstanceOpt).map(_.aNode._2.id).toSet
    val cis = a.componentInstanceMap.toList.sortBy(_._1.getNodeId).collect {
      case (sym, ci) if inTopology.contains(sym.getNodeId) => (sym, ci)
    }
    val instances = cis.map { case (sym, ci) =>
      val q = ci.qualifiedName.toIdentList
      val unique = cis.count(_._2.qualifiedName.base == ci.qualifiedName.base) == 1
      InstanceDef(sym.getNodeId, if unique then ci.qualifiedName.base else flat(q), q,
        ci.component.aNode._2.id, ci.baseId, ci.queueSize, ci.priority)
    }
    val used = instances.map(_.component).toSet
    for {
      // Only the components the topology instantiates: an unrelated component (say, one with a serial port)
      // must not stop this topology's shell.
      components <- Result.map(
        a.componentMap.toList.sortBy(_._1.getNodeId).filter { case (sym, _) => used.contains(sym.getNodeId) },
        (sym: Symbol.Component, c: Component) => componentDef(a, sym, c, a.getQualifiedName(sym).toIdentList)
      )
      connections <- Result.map(
        t.connectionMap.toList.sortBy(_._1),
        (group: String, cs: List[fpp.compiler.analysis.Connection]) => connectionsOf(t, group, cs)
      )
      m = Model(Version.v, a.getQualifiedName(tSym).toIdentList, portDefs(a, tul), components, instances, connections.flatten)
      _ <- checkNames(m)
    } yield m
  }

  private def endpoint(e: fpp.compiler.analysis.Connection.Endpoint, num: Int): Result.Result[Endpoint] =
    e.port.interfaceInstance.getComponentInstanceOpt match {
      case Some(ci) => Right(Endpoint(ci.aNode._2.id, e.port.portInstance.getUnqualifiedName, num))
      case None => Left(SemanticError.InvalidPortInstance(e.loc, "topology ports are not supported by the shell generators", e.loc))
    }

  private def connectionsOf(t: Topology, group: String, cs: List[fpp.compiler.analysis.Connection]): Result.Result[List[Connection]] =
    for {
      list <- Result.map(cs, (c: fpp.compiler.analysis.Connection) =>
        for {
          src <- endpoint(c.from, t.fromPortNumberMap(c))
          dst <- endpoint(c.to, t.toPortNumberMap(c))
        } yield (c.from.loc, Connection(group, src, dst)))
    } yield {
      // An explicit group is in source order (the analysis prepends as it reads, so the map's order is not
      // the file's); a pattern's connections all come from one position and the analysis lists them in no
      // stable order, so they are sorted by endpoints.
      if list.map(_._1).distinct.length <= 1 then list.map(_._2).sortWith(connectionLe)
      else list.sortBy { case (loc, _) => (loc.file.toString, loc.pos.line, loc.pos.column) }.map(_._2)
    }

}
