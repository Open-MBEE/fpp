module Demo {

  @ Not instantiated by the topology: its serial port must not stop the shell
  passive component SerialLogger {
    sync input port logIn: serial
  }

  passive component Probe {
    output port dataOut: Data
  }

  instance probe: Probe base id 0x700
  instance sinkA: Sink base id 0x800

  topology Typed {
    instance probe
    instance sinkA
    connections Data {
      probe.dataOut -> sinkA.dataIn
    }
  }

}
