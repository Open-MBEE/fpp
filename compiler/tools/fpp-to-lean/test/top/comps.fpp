module Demo {

  constant BURST = 4

  struct Sample { value: U32, ok: bool }
  array Readings = [3] F32

  @ A value port with a reference parameter and a sized string
  port Data(ref sample: Sample, label: string size 16)
  @ A port with a return type
  port Query(readings: Readings) -> U32

  active component Worker {
    async input port dataIn: [2] Data priority 3 drop
    sync input port queryIn: Query
    guarded input port guardedIn: Query
    output port dataOut: [2] Data
    output port queryOut: Query

    command recv port cmdIn
    command reg port cmdRegOut
    command resp port cmdResponseOut
    event port eventOut
    text event port textEventOut
    time get port timeGetOut
    telemetry port tlmOut
    param get port paramGetOut
    param set port paramSetOut

    async command START(count: U32, name: string size 8) opcode 0x10 priority 2 drop
    sync command STOP opcode 0x11
    guarded command RESET(hard: bool) opcode 0x12
    @ `end` is reserved in SysML: the item name must be escaped as a whole
    sync command end opcode 0x13

    event Started(count: U32) severity activity high id 1 format "started {} times"
    event Overrun severity warning low id 2 format "overrun" throttle 5

    telemetry Count: U32 id 1 update on change format "{} items"
    telemetry Last: Sample id 2

    param Rate: F64 default 1.5 id 1 set opcode 0x20 save opcode 0x21
    param Enabled: bool default true id 2
    @ A default that is a constant: the shells carry its value
    param Burst: U32 default BURST id 3
  }

  passive component Sink {
    sync input port dataIn: Data
    output port queryOut: Query
  }

  passive component Dispatcher {
    output port compCmdSend: [4] Fw.Cmd
    sync input port compCmdReg: [4] Fw.CmdReg
    sync input port compCmdStat: Fw.CmdResponse
  }

  passive component Clock {
    sync input port timeGetPort: Fw.Time
  }

}
