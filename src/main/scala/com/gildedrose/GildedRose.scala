package com.gildedrose

object GildedRose {
  val Sulfuras      = "Sulfuras, Hand of Ragnaros"
  val AgedBrie      = "Aged Brie"
  val BackstagePass = "Backstage passes to a TAFKAL80ETC concert"

  def update(items: Seq[Item]): Seq[Item] =
    items.map(update)

  def update(item: Item): Item =
    updateSellIn(updateQuality(item))

  private def updateQuality(item: Item): Item = item match {
    case Item(AgedBrie, sellIn, quality) =>
      sellIn match {
        case n if n < 0  => boundQuality(item, quality + 2)
        case n           => boundQuality(item, quality + 1)
      }

    case Item(BackstagePass, sellIn, quality) =>
      sellIn match {
        case n if n <  0 => boundQuality(item, 0)
        case n if n <  6 => boundQuality(item, quality + 3)
        case n if n < 11 => boundQuality(item, quality + 2)
        case n           => boundQuality(item, quality + 1)
      }

    case Item(Sulfuras, sellIn, quality) =>
      boundQuality(item, quality)

    case Item(_, sellIn, quality) =>
      sellIn match {
        case n if n <  0 => boundQuality(item, quality - 2)
        case n           => boundQuality(item, quality - 1)
      }
  }

  private def updateSellIn(item: Item): Item = item.name match {
    case Sulfuras => item // Don't update sellIn
    case _        => item.copy(sellIn = item.sellIn - 1)
  }

  private def boundQuality(item: Item, quality: Int): Item = item.name match {
    case Sulfuras => item.copy(quality = trim(quality, 80, 80))
    case _        => item.copy(quality = trim(quality,  0, 50))
  }

  private def trim(number: Int, lowerBound: Int, upperBound: Int): Int =
    math.min(upperBound, math.max(lowerBound, number))
}