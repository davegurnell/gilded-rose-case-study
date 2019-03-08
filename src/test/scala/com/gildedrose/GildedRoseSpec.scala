package com.gildedrose

import org.scalatest._

class GildedRoseTest  extends WordSpec with Matchers {
  "the update method" should {
    "foo" in {
      var items = List(new Item("foo", 0, 0))
      GildedRose.updateQuality(items)
      app.items.head.name should equal ("fixme")
    }
  }
}
