module A {
  instance worker: Demo.Worker base id 0x100 queue size 10
}

module B {
  instance worker: Demo.Worker base id 0x200 queue size 10
}

module Demo {

  instance worker: Worker base id 0x300 queue size 10
  instance clock: Clock base id 0x400
  instance dispatcher: Dispatcher base id 0x500

  @ Three instances named `worker`: the shells write their qualified names
  topology SameName {
    instance A.worker
    instance B.worker
    instance worker
    instance clock
    instance dispatcher

    connections Data {
      A.worker.queryOut -> worker.queryIn
      B.worker.queryOut -> worker.guardedIn
    }

    command connections instance dispatcher
    time connections instance clock
  }

}
