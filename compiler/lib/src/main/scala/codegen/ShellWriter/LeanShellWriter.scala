package fpp.compiler.codegen

import fpp.compiler.ast._
import fpp.compiler.util._
import ShellModel._

/** Writes the shell of one topology as a self-contained Lean 4 module (`module` header, core only):
 *  the port types, components, instances and connections as inductives and total functions on them,
 *  so that a handler body, an agent model or a property can be written over them. */
object LeanShellWriter {

  /** Lean keywords an FPP name may collide with; such a name is written `«name»`. */
  val reserved: Set[String] = Set(
    "at", "by", "calc", "class", "def", "deriving", "do", "else", "end", "example", "for", "from", "fun",
    "have", "if", "import", "in", "inductive", "instance", "let", "match", "mut", "mutual", "namespace",
    "open", "partial", "private", "protected", "return", "section", "show", "structure", "then", "theorem",
    "unless", "universe", "unsafe", "variable", "where", "with", "true", "false", "Type", "Prop", "Sort"
  )

  def ident(s: String): String = {
    val s1 = s.replace(".", "_")
    if reserved.contains(s1) then s"«$s1»" else s1
  }

  def ctor(q: List[String]): String = ident(q.mkString("_"))

  def strLit(s: String): String = "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"") + "\""

  def optNat(o: Option[BigInt]): String = o.map(n => s"some $n").getOrElse("none")

  def optStr(o: Option[String]): String = o.map(s => s"some ${strLit(s)}").getOrElse("none")

  def queueFull(q: QueueFull): String = q match {
    case QueueFull.Assert => ".assert"
    case QueueFull.Block => ".block"
    case QueueFull.Drop => ".drop"
    case QueueFull.Hook => ".hook"
  }

  def portKind(k: PortKind): String = k match {
    case PortKind.AsyncInput(q) => s".asyncInput ${queueFull(q)}"
    case PortKind.SyncInput => ".syncInput"
    case PortKind.GuardedInput => ".guardedInput"
    case PortKind.Output => ".output"
  }

  def componentKind(k: ComponentKind): String = k match {
    case ComponentKind.Passive => ".passive"
    case ComponentKind.Queued => ".queued"
    case ComponentKind.Active => ".active"
  }

  def commandKind(k: CommandKind): String = k match {
    case CommandKind.Async(p, q) => s".async ${p.map(n => s"(some $n)").getOrElse("none")} ${queueFull(q)}"
    case CommandKind.Guarded => ".guarded"
    case CommandKind.Sync => ".sync"
    case CommandKind.ParamSet => ".paramSet"
    case CommandKind.ParamSave => ".paramSave"
  }

  def param(p: Param): String =
    s"{ name := ${strLit(p.name)}, ty := ${strLit(p.ty)}, ref := ${p.ref} }"

  def list(xs: List[String], indent: String = "    "): String = xs match {
    case Nil => "[]"
    case _ => "[\n" + xs.map(indent + _).mkString(",\n") + " ]"
  }

  /** One `| .ctor => rhs` arm. */
  def arm(c: String, rhs: String): String = s"  | .$c => $rhs"

  private def error(msg: String) = Left(CodeGenError.ShellError(msg))

  def write(m: Model, ns: String, source: String): Result.Result[String] = {
    val ports = m.usedPorts
    val comps = m.usedComponents
    if m.instances.isEmpty then return error("the topology has no component instances")
    if ports.isEmpty then return error("the topology's components have no ports")
    def portCtor(id: AstNode.Id): Result.Result[String] = m.port(id) match {
      case Some(p) => Right(ctor(p.qname))
      case None => error(s"port type $id is used but not defined")
    }
    def compCtor(id: AstNode.Id): Result.Result[String] = m.component(id) match {
      case Some(c) => Right(ctor(c.qname))
      case None => error(s"component $id is instantiated but not defined")
    }
    def instCtor(id: AstNode.Id): Result.Result[String] = m.instance(id) match {
      case Some(i) => Right(ident(i.name))
      case None => error(s"instance $id is connected but not in the topology")
    }
    def portArms(f: PortDef => String): String =
      ports.map(p => arm(ctor(p.qname), f(p))).mkString("\n")
    def compArms(f: ComponentDef => Result.Result[String]): Result.Result[String] =
      Result.map(comps, (c: ComponentDef) => f(c).map(rhs => arm(ctor(c.qname), rhs))).map(_.mkString("\n"))
    def instArms(f: InstanceDef => Result.Result[String]): Result.Result[String] =
      Result.map(m.instances, (i: InstanceDef) => f(i).map(rhs => arm(ident(i.name), rhs))).map(_.mkString("\n"))
    def portLine(p: PortSpec): Result.Result[String] =
      portCtor(p.ty).map(t =>
        s"{ name := ${strLit(p.name)}, kind := ${portKind(p.kind)}, ty := .$t, size := ${p.size}, special := ${optStr(p.special)} }")
    def commandLine(c: CommandDef): String =
      s"{ name := ${strLit(c.name)}, opcode := ${c.opcode}, kind := ${commandKind(c.kind)}, params := ${list(c.params.map(param), "      ")} }"
    def eventLine(e: EventDef): String =
      s"{ name := ${strLit(e.name)}, id := ${e.id}, severity := ${strLit(e.severity)}, params := ${list(e.params.map(param), "      ")}, format := ${strLit(e.format)}, throttle := ${optNat(e.throttle)} }"
    def tlmLine(t: TlmChannelDef): String =
      s"{ name := ${strLit(t.name)}, id := ${t.id}, ty := ${strLit(t.ty)}, update := ${strLit(t.update)}, format := ${optStr(t.format)} }"
    def paramLine(p: ParamDef): String =
      s"{ name := ${strLit(p.name)}, id := ${p.id}, ty := ${strLit(p.ty)}, default := ${optStr(p.default)}, setOpcode := ${p.setOpcode}, saveOpcode := ${p.saveOpcode} }"
    def connLine(c: Connection): Result.Result[String] =
      for {
        s <- instCtor(c.src.inst)
        d <- instCtor(c.dst.inst)
      } yield s"{ group := ${strLit(c.group)}, src := ⟨.$s, ${strLit(c.src.port)}, ${c.src.num}⟩, dst := ⟨.$d, ${strLit(c.dst.port)}, ${c.dst.num}⟩ }"
    val topology = m.topology.mkString(".")
    for {
      compName <- compArms(c => Right(strLit(c.qname.mkString("."))))
      compKind <- compArms(c => Right(componentKind(c.kind)))
      compPorts <- compArms(c => Result.map(c.ports, portLine).map(list(_)))
      compCommands <- compArms(c => Right(list(c.commands.map(commandLine))))
      compEvents <- compArms(c => Right(list(c.events.map(eventLine))))
      compTelemetry <- compArms(c => Right(list(c.telemetry.map(tlmLine))))
      compParameters <- compArms(c => Right(list(c.parameters.map(paramLine))))
      instName <- instArms(i => Right(strLit(i.name)))
      instComponent <- instArms(i => compCtor(i.component).map(c => s".$c"))
      instBaseId <- instArms(i => Right(i.baseId.toString))
      instQueueSize <- instArms(i => Right(optNat(i.queueSize)))
      instPriority <- instArms(i => Right(optNat(i.priority)))
      connLines <- Result.map(m.connections, connLine)
    } yield s"""module

/-! # `$topology` — the FPP model as Lean

Generated by `fpp-to-lean` from $source (fpp ${m.fppVersion}). Do not edit: regenerate. What is here is exactly
what every implementation of a component of this topology must agree on: port types, components with their
ports and their external contract (commands, events, telemetry channels, parameters, with ids relative to the
instance's base id), instances, connections. Behaviour is not in FPP and not here. -/

@[expose] public section

namespace $ns

def fppVersion : String := ${strLit(m.fppVersion)}

def topology : String := ${strLit(topology)}

/-- Queue-full behaviour of an `async input` port (`hook`: the component's overflow hook runs). -/
inductive QueueFull
  | assert
  | block
  | drop
  | hook
  deriving DecidableEq, Repr

/-- `async`/`sync`/`guarded input` or `output`. -/
inductive PortKind
  | asyncInput (queueFull : QueueFull)
  | syncInput
  | guardedInput
  | output
  deriving DecidableEq, Repr

def PortKind.isInput : PortKind → Bool
  | .output => false
  | _ => true

/-- A parameter of a port type; `ref` parameters are written by the callee. -/
structure Param where
  name : String
  ty : String
  ref : Bool
  deriving DecidableEq, Repr

/-- The port types this topology uses. -/
inductive PortType
${ports.map(p => s"  | ${ctor(p.qname)}").mkString("\n")}
  deriving DecidableEq, Repr

def PortType.name : PortType → String
${portArms(p => strLit(p.qname.mkString(".")))}

def PortType.params : PortType → List Param
${portArms(p => list(p.params.map(param)))}

def PortType.returns : PortType → Option String
${portArms(p => optStr(p.returns))}

inductive ComponentKind
  | passive
  | queued
  | active
  deriving DecidableEq, Repr

/-- The components this topology instantiates. -/
inductive Component
${comps.map(c => s"  | ${ctor(c.qname)}").mkString("\n")}
  deriving DecidableEq, Repr

def Component.name : Component → String
$compName

def Component.kind : Component → ComponentKind
$compKind

/-- A port of a component. `special` is the FPP role of a special port (`command recv`, `event`, …). -/
structure Port where
  name : String
  kind : PortKind
  ty : PortType
  size : Nat
  special : Option String
  deriving DecidableEq, Repr

def Component.ports : Component → List Port
$compPorts

def Component.port? (c : Component) (name : String) : Option Port := c.ports.find? (·.name == name)

/-- `async` (priority, queue-full policy), `guarded` or `sync`; `paramSet`/`paramSave` are derived from a `param`. -/
inductive CommandKind
  | async (priority : Option Nat) (queueFull : QueueFull)
  | guarded
  | sync
  | paramSet
  | paramSave
  deriving DecidableEq, Repr

/-- A command of a component; `opcode` is relative to the instance's base id. -/
structure Command where
  name : String
  opcode : Nat
  kind : CommandKind
  params : List Param
  deriving DecidableEq, Repr

/-- An event; `id` relative to the base id, `severity` in FPP's words, `format` as written. -/
structure Event where
  name : String
  id : Nat
  severity : String
  params : List Param
  format : String
  throttle : Option Nat
  deriving DecidableEq, Repr

/-- A telemetry channel; `id` relative to the base id, `update` is `always` or `on change`. -/
structure TlmChannel where
  name : String
  id : Nat
  ty : String
  update : String
  format : Option String
  deriving DecidableEq, Repr

/-- A parameter; `id` and the opcodes of its set/save commands relative to the base id. -/
structure Parameter where
  name : String
  id : Nat
  ty : String
  default : Option String
  setOpcode : Nat
  saveOpcode : Nat
  deriving DecidableEq, Repr

def Component.commands : Component → List Command
$compCommands

def Component.events : Component → List Event
$compEvents

def Component.telemetry : Component → List TlmChannel
$compTelemetry

def Component.parameters : Component → List Parameter
$compParameters

def Component.command? (c : Component) (name : String) : Option Command := c.commands.find? (·.name == name)
def Component.event? (c : Component) (name : String) : Option Event := c.events.find? (·.name == name)
def Component.channel? (c : Component) (name : String) : Option TlmChannel := c.telemetry.find? (·.name == name)
def Component.parameter? (c : Component) (name : String) : Option Parameter := c.parameters.find? (·.name == name)

/-- The instances of the topology. -/
inductive Instance
${m.instances.map(i => s"  | ${ident(i.name)}").mkString("\n")}
  deriving DecidableEq, Repr

def Instance.all : List Instance :=
${list(m.instances.map(i => s".${ident(i.name)}"), "  ")}

def Instance.name : Instance → String
$instName

def Instance.component : Instance → Component
$instComponent

def Instance.baseId : Instance → Nat
$instBaseId

def Instance.queueSize : Instance → Option Nat
$instQueueSize

def Instance.priority : Instance → Option Nat
$instPriority

def Instance.port? (i : Instance) (name : String) : Option Port := i.component.port? name

/-- One end of a connection: the instance, the port and the port number the topology assigned. -/
structure Endpoint where
  inst : Instance
  port : String
  num : Nat
  deriving DecidableEq, Repr

/-- A connection of the topology, in its `connections` group (pattern connections — `command`, `time`,
`text event` … — are in a group named after the pattern). -/
structure Connection where
  group : String
  src : Endpoint
  dst : Endpoint
  deriving DecidableEq, Repr

def connections : List Connection :=
${list(connLines, "  ")}

/-- The connections between two instances, in either direction. -/
def connectionsBetween (a b : Instance) : List Connection :=
  connections.filter fun c => (c.src.inst == a && c.dst.inst == b) || (c.src.inst == b && c.dst.inst == a)

end $ns
"""
  }

}
