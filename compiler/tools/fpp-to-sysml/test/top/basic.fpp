module Demo {

  instance worker: Worker base id 0x100 queue size 10 priority 5
  instance sink: Sink base id 0x200
  @ `end` is reserved in Lean and in SysML: the generators must escape it
  instance end: Sink base id 0x300
  instance dispatcher: Dispatcher base id 0x400
  instance clock: Clock base id 0x500

  topology Basic {
    instance worker
    instance sink
    instance end
    instance dispatcher
    instance clock

    connections Data {
      worker.dataOut[0] -> sink.dataIn
      worker.dataOut[1] -> end.dataIn
      sink.queryOut -> worker.queryIn
      end.queryOut -> worker.guardedIn
    }

    command connections instance dispatcher
    time connections instance clock
  }

}
