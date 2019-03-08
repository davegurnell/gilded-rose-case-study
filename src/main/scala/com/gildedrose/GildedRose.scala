package com.gildedrose

case class Item(name: String, sellIn: Int, quality: Int) {
  import Item.{minQuality, maxQuality}

  def incQuality(amount: Int): Item =
    copy(quality = math.min(maxQuality, math.max(minQuality, quality + amount)))

  def decQuality(amt: Int): Item =
    incQuality(-amt)

  def setQuality(quality: Int): Item =
    copy(quality = quality)

  def decSellIn(): Item =
    copy(sellIn = sellIn - 1)
}

object Item {
  val minQuality = 0
  val maxQuality = 50
}

object GildedRose {
  val AgedBrie        = "Aged Brie"
  val Sulfuras        = "Sulfuras, Hand of Ragnaros"
  val BackstagePasses = "Backstage passes to a TAFKAL80ETC concert"

  def updateQuality(items: List[Item]): List[Item] =
    items.map(updateQuality)

  def updateQuality(item: Item): Item = {
    item.name match {
      case AgedBrie =>
        item
          .decSellIn()
          .incQuality(1)

      case Sulfuras =>
        item

      case BackstagePasses if item.sellIn < 1 =>
        item
          .decSellIn()
          .setQuality(0)

      case BackstagePasses =>
        val change = if(item.sellIn <= 5) 3 else if(item.sellIn <= 10) 2 else 1
        item
          .decSellIn()
          .incQuality(change)

      case _ =>
        item
          .decSellIn()
          .decQuality(if(item.sellIn < 1) 2 else 1)
    }
  }
}
