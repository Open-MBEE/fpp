# The framework ports and types a component with commands, events, telemetry and parameters needs
module Fw {
  type Time
  type CmdArgBuffer
  type LogBuffer
  type TlmBuffer
  type ParamBuffer
  enum CmdResponse { OK, INVALID_OPCODE, VALIDATION_ERROR }
  enum ParamValid { INVALID, VALID }
  enum LogSeverity { FATAL, WARNING_HI, WARNING_LO, COMMAND, ACTIVITY_HI, ACTIVITY_LO, DIAGNOSTIC }
  port Cmd(opCode: FwOpcodeType, cmdSeq: U32, ref args: CmdArgBuffer)
  port CmdReg(opCode: FwOpcodeType)
  port CmdResponse(opCode: FwOpcodeType, cmdSeq: U32, response: CmdResponse)
  port Log($id: FwEventIdType, ref timeTag: Time, $severity: LogSeverity, ref args: LogBuffer)
  port LogText($id: FwEventIdType, ref timeTag: Time, $severity: LogSeverity, ref $text: string size 40)
  port Time(ref $time: Time)
  port Tlm($id: FwChanIdType, ref timeTag: Time, ref val: TlmBuffer)
  port PrmGet($id: U32, ref val: ParamBuffer) -> ParamValid
  port PrmSet($id: U32, ref val: ParamBuffer)
}
