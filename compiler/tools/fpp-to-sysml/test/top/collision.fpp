module Demo {

  module A_B { port C }
  module A { port B_C }

  @ `A_B.C` and `A.B_C` both flatten to `A_B_C`: the generators refuse the topology
  passive component Twice {
    sync input port one: A_B.C
    sync input port two: A.B_C
  }
  instance twice: Twice base id 0x100

  topology Collision {
    instance twice
  }

}
