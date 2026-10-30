module Demo {
  instance sink: Sink base id 0x200
  instance end: Sink base id 0x300
  topology First { instance sink }
  topology Second { instance end }
}
