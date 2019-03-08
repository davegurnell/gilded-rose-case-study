package com.gildedrose

case class Item(name: String, var sellIn: Int, var quality: Int)

object GildedRose {
  def updateQuality(items: List[Item]): List[Item] =
    items.map(updateQuality)

  def updateQuality(item: Item): Item = {
    val copy = item.copy()

    if (!copy.name.equals("Aged Brie")
      && !copy.name.equals("Backstage passes to a TAFKAL80ETC concert")) {
      if (copy.quality > 0) {
        if (!copy.name.equals("Sulfuras, Hand of Ragnaros")) {
          copy.quality = copy.quality - 1
        }
      }
    } else {
      if (copy.quality < 50) {
        copy.quality = copy.quality + 1

        if (copy.name.equals("Backstage passes to a TAFKAL80ETC concert")) {
          if (copy.sellIn < 11) {
            if (copy.quality < 50) {
              copy.quality = copy.quality + 1
            }
          }

          if (copy.sellIn < 6) {
            if (copy.quality < 50) {
              copy.quality = copy.quality + 1
            }
          }
        }
      }
    }

    if (!copy.name.equals("Sulfuras, Hand of Ragnaros")) {
      copy.sellIn = copy.sellIn - 1
    }

    if (copy.sellIn < 0) {
      if (!copy.name.equals("Aged Brie")) {
        if (!copy.name.equals("Backstage passes to a TAFKAL80ETC concert")) {
          if (copy.quality > 0) {
            if (!copy.name.equals("Sulfuras, Hand of Ragnaros")) {
              copy.quality = copy.quality - 1
            }
          }
        } else {
          copy.quality = copy.quality - copy.quality
        }
      } else {
        if (copy.quality < 50) {
          copy.quality = copy.quality + 1
        }
      }
    }

    copy
  }
}
