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
    case AsyncInput(queueFull: QueueFull)
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
  final case class InstanceDef(id: AstNode.Id, name: String, component: AstNode.Id, baseId: BigInt,
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

  /** The text of a literal expression; any other expression is its FPP text. */
  def literalText(e: Ast.Expr): String = e match {
    case Ast.ExprLiteralInt(v) => v
    case Ast.ExprLiteralFloat(v) => v
    case Ast.ExprLiteralString(v) => v
    case Ast.ExprLiteralBool(v) => v.toString
    case other => other.toString
  }

  def typeName(tn: Ast.TypeName): String = tn match {
    case Ast.TypeNameInt(name) => name.toString
    case Ast.TypeNameFloat(name) => name.toString
    case Ast.TypeNameBool => "bool"
    case Ast.TypeNameString(size) => size match {
      case Some(n) => s"string[${literalText(n.data)}]"
      case None => "string"
    }
    case Ast.TypeNameQualIdent(name) => name.data.toIdentList.mkString(".")
  }

  def params(ps: Ast.FormalParamList): List[Param] =
    ps.map { case (_, node, _) =>
      Param(node.data.name, typeName(node.data.typeName.data), node.data.kind == Ast.FormalParam.Ref)
    }

  def queueFull(qf: Ast.QueueFull): QueueFull = qf match {
    case Ast.QueueFull.Assert => QueueFull.Assert
    case Ast.QueueFull.Block => QueueFull.Block
    case Ast.QueueFull.Drop => QueueFull.Drop
    case Ast.QueueFull.Hook => QueueFull.Hook
  }

  def severity(s: Ast.SpecEvent.Severity): String = s.toString

  /** Port definitions in source order, with their qualified names. */
  def portDefs(tul: List[Ast.TransUnit]): List[PortDef] = {
    def members(q: List[String], ms: List[Ast.ModuleMember]): List[PortDef] = ms.flatMap { m =>
      m.node._2 match {
        case Ast.ModuleMember.DefModule(node) => members(q :+ node.data.name, node.data.members)
        case Ast.ModuleMember.DefPort(node) =>
          val d = node.data
          List(PortDef(node.id, q :+ d.name, params(d.params), d.returnType.map(t => typeName(t.data))))
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
        case PortInstance.General.Kind.AsyncInput(_, qf) => PortKind.AsyncInput(queueFull(qf))
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
            case _ => PortKind.AsyncInput(s.queueFull.map(queueFull).getOrElse(QueueFull.Assert))
          }
        case _ => PortKind.Output
      }
      Right(PortSpec(s.getUnqualifiedName, kind, s.symbol.getNodeId, 1, Some(role)))
    case t: PortInstance.Topology =>
      Left(SemanticError.InvalidPortInstance(t.getLoc, "topology ports are not supported by the shell generators", t.getLoc))
  }

  def commands(c: Component): List[CommandDef] =
    c.commandMap.toList.sortBy(_._1).map { case (opcode, cmd) =>
      cmd match {
        case Command.NonParam(aNode, kind) =>
          val d = aNode._2.data
          val k = kind match {
            case Command.NonParam.Async(priority, qf) => CommandKind.Async(priority, queueFull(qf))
            case Command.NonParam.Guarded => CommandKind.Guarded
            case Command.NonParam.Sync => CommandKind.Sync
          }
          CommandDef(d.name, opcode, k, params(d.params))
        case Command.Param(aNode, kind) =>
          val name = aNode._2.data.name.toUpperCase
          kind match {
            case Command.Param.Set => CommandDef(s"${name}_PRM_SET", opcode, CommandKind.ParamSet, Nil)
            case Command.Param.Save => CommandDef(s"${name}_PRM_SAVE", opcode, CommandKind.ParamSave, Nil)
          }
      }
    }

  def events(c: Component): List[EventDef] =
    c.eventMap.toList.sortBy(_._1).map { case (id, e) =>
      val d = e.aNode._2.data
      EventDef(d.name, id, severity(d.severity), params(d.params), d.format.data, e.throttle.map(t => BigInt(t.count)))
    }

  def telemetry(c: Component): List[TlmChannelDef] =
    c.tlmChannelMap.toList.sortBy(_._1).map { case (id, t) =>
      val d = t.aNode._2.data
      val update = t.update match {
        case Ast.SpecTlmChannel.Always => "always"
        case Ast.SpecTlmChannel.OnChange => "on change"
      }
      TlmChannelDef(d.name, id, typeName(d.typeName.data), update, d.format.map(_.data))
    }

  def parameters(c: Component): List[ParamDef] =
    c.paramMap.toList.sortBy(_._1).map { case (id, p) =>
      val d = p.aNode._2.data
      ParamDef(d.name, id, typeName(d.typeName.data), d.default.map(e => literalText(e.data)), p.setOpcode, p.saveOpcode)
    }

  def componentDef(sym: Symbol.Component, c: Component, qname: List[String]): Result.Result[ComponentDef] =
    for {
      ports <- Result.map(c.portMap.toList.sortBy(_._1).map(_._2), portSpec)
    } yield ComponentDef(sym.getNodeId, qname, componentKind(c.aNode._2.data.kind), ports,
      commands(c), events(c), telemetry(c), parameters(c))

  /** The model of one topology. */
  def apply(a: Analysis, tul: List[Ast.TransUnit], tSym: Symbol.Topology, t: Topology): Result.Result[Model] = {
    val inTopology: Set[AstNode.Id] =
      t.instanceMap.keys.flatMap(_.getComponentInstanceOpt).map(_.aNode._2.id).toSet
    val instances = a.componentInstanceMap.toList.sortBy(_._1.getNodeId).collect {
      case (sym, ci) if inTopology.contains(sym.getNodeId) =>
        InstanceDef(sym.getNodeId, ci.qualifiedName.base, ci.component.aNode._2.id, ci.baseId, ci.queueSize, ci.priority)
    }
    for {
      components <- Result.map(
        a.componentMap.toList.sortBy(_._1.getNodeId),
        (sym: Symbol.Component, c: Component) => componentDef(sym, c, a.getQualifiedName(sym).toIdentList)
      )
      connections <- Result.map(
        t.connectionMap.toList.sortBy(_._1),
        (group: String, cs: List[fpp.compiler.analysis.Connection]) => connectionsOf(t, group, cs)
      )
    } yield Model(Version.v, a.getQualifiedName(tSym).toIdentList, portDefs(tul), components, instances, connections.flatten)
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
      // An explicit group keeps its source order; a pattern's connections all come from one position
      // and the analysis lists them in no stable order, so they are sorted by endpoints.
      if list.map(_._1).distinct.length <= 1 then list.map(_._2).sortWith(connectionLe) else list.map(_._2)
    }

}
