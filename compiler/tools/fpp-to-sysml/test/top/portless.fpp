module Demo {

  @ A component with no ports at all: the shell is its instance and nothing else
  passive component Idle {}
  instance idle: Idle base id 0x600

  topology Portless {
    instance idle
  }

}
