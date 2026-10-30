module Demo {

  instance sinkA: Sink base id 0x800

  @ A topology port: the generators refuse the topology rather than drop the port
  topology Exposed {
    instance sinkA
    port dataIn = sinkA.dataIn
  }

}
