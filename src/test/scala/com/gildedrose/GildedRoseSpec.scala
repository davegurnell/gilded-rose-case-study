package com.gildedrose

import org.scalatest._

class GildedRoseSpec extends WordSpec with Matchers {
  "the update method" should {
    "foo" in {
      val items = List(Item("foo", 0, 0))
      GildedRose.updateQuality(items)
      items.head.name should equal ("fixme")
    }
  }
}
