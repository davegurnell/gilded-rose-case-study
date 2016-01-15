package com.gildedrose

import org.scalatest._
import org.scalatest.prop._
import org.scalacheck._
import org.scalacheck.Prop.BooleanOperators

class GildedRoseTest extends FreeSpec
  with Matchers
  with Checkers
  with GildedRoseFixtures {

  "\"Non-special\" products" - {
    val itemNameGen = Gen.oneOf(itemNames diff Seq(agedBrie, backstagePass, sulfuras))

    "Once the sell by date has passed, Quality degrades twice as fast" in {
      implicit val arbitraryItem = Arbitrary(itemGen(
        itemNameGen = itemNameGen,       // non-special items
        sellInGen   = Gen.choose(1, 50), // one or more days left
        qualityGen  = Gen.choose(1, 50)  // quality of one or more
      ))

      check { (item: Item) =>
        val original = item.quality
        // Update when sellIn > 0:
        val updated1 = update(item).quality
        // Update when sellIn < 0:
        val updated2 = update(item.copy(sellIn = -item.sellIn)).quality

        // Quality always decreases towards zero:
        val assertion1 = (updated1 > 0) ==> (updated1 > updated2)
        // Quality decreases double after sellIn date:
        val assertion2 = (updated2 > 0) ==> ((original - updated2) == (original - updated1) * 2)

        assertion1 && assertion2
      }
    }
  }

  "The Quality of an item is never negative" - {
    implicit val arbitraryItem = Arbitrary(anyItemGen)

    "when initially generated" in {
      check { (item: Item) =>
        item.quality >= 0
      }
    }

    "after update" in {
      check { (item: Item) =>
        update(item).quality >= 0
      }
    }
  }

  "\"Aged Brie\"" - {
    implicit val arbitraryItem = Arbitrary(agedBrieGen)

    "actually increases in Quality the older it gets (but doesn't go above 50)" in {
      check { (item: Item) =>
        val original   = item.quality
        val updated    = update(item).quality

        val assertion1 = (original == 50) ==> (updated == original)
        val assertion2 = (original  < 50) ==> (updated > original)

        assertion1 || assertion2
      }
    }
  }

  "The Quality of non-legendary items" - {
    implicit val arbitraryItem = Arbitrary(nonLegendaryGen())

    "is never more than 50 when initially generated" in {
      check { (item: Item) =>
        item.quality <= 50
      }
    }

    "is never more than 50 after update" in {
      check { (item: Item) =>
        update(item).quality <= 50
      }
    }
  }

  "The Quality of Sulfuras" - {
    implicit val arbitraryItem = Arbitrary(sulfurasGen)

    "is always 80 when initially generated" in {
      check { (item: Item) =>
        item.quality == 80
      }
    }

    "is always 80 after update" in {
      check { (item: Item) =>
        update(item).quality == 80
      }
    }
  }

  "\"Sulfuras\"" - {
    implicit val arbitraryItem = Arbitrary(sulfurasGen)

    "never has to be sold" in {
      check { (item: Item) =>
        update(item).sellIn == item.sellIn
      }
    }

    "never decreases in Quality" in {
      check { (item: Item) =>
        update(item).quality == item.quality
      }
    }
  }

  "\"Backstage passes\", like aged brie, increases in Quality as it's SellIn value approaches" - {
    "Quality drops to 0 after the concert" in {
      implicit val arbitraryItem = Arbitrary(backstagePassGen(sellInGen = Gen.choose(-50, -1)))

      check { (item: Item) =>
        val actual   = update(item).quality
        val expected = 0
        actual == expected
      }
    }

    "Quality increases by by 3 when there are 5 days or less (but doesn't go above 50)" in {
      implicit val arbitraryItem = Arbitrary(backstagePassGen(sellInGen = Gen.choose(1, 5)))

      check { (item: Item) =>
        val actual   = update(item).quality
        val expected = math.min(50, item.quality + 3)
        actual == expected
      }
    }

    "Quality increases by 2 when there are 10 days or less (but doesn't go above 50)" in {
      implicit val arbitraryItem = Arbitrary(backstagePassGen(sellInGen = Gen.choose(6, 10)))

      check { (item: Item) =>
        val actual   = update(item).quality
        val expected = math.min(50, item.quality + 2)
        actual == expected
      }
    }

    "Quality increases by 1 when there are more than 10 days lef (but doesn't go above 50)t" in {
      implicit val arbitraryItem = Arbitrary(backstagePassGen(sellInGen = Gen.choose(11, 500)))

      check { (item: Item) =>
        val actual   = update(item).quality
        val expected = math.min(50, item.quality + 1)
        actual == expected
      }
    }
  }
}

trait GildedRoseFixtures extends GildedRoseGenerators with GildedRoseHelpers

trait GildedRoseGenerators {
  val agedBrie      = "Aged Brie"
  val backstagePass = "Backstage passes to a TAFKAL80ETC concert"
  val sulfuras      = "Sulfuras, Hand of Ragnaros"
  val woolCloth     = "Woll Cloth"
  val itemNames     = Seq(agedBrie, backstagePass, sulfuras, woolCloth)

  val itemNameGen = Gen.oneOf(itemNames)
  val sellInGen   = Gen.choose(-50, 50)
  def qualityGen(itemName: String): Gen[Int] =
    if(itemName == sulfuras) Gen.const(80) else Gen.choose(0, 50)

  def itemGen(itemNameGen: Gen[String]): Gen[Item] =
    for {
      itemName <- itemNameGen
      sellIn   <- sellInGen
      quantity <- qualityGen(itemName)
    } yield new Item(itemName, sellIn, quantity)

  def itemGen(itemNameGen: Gen[String], sellInGen: Gen[Int]): Gen[Item] =
    for {
      itemName <- itemNameGen
      sellIn   <- sellInGen
      quantity <- qualityGen(itemName)
    } yield new Item(itemName, sellIn, quantity)

  def itemGen(itemNameGen: Gen[String], sellInGen: Gen[Int], qualityGen: Gen[Int]): Gen[Item] =
    for {
      itemName <- itemNameGen
      sellIn   <- sellInGen
      quantity <- qualityGen
    } yield new Item(itemName, sellIn, quantity)

  val anyItemGen  = itemGen(Gen.oneOf(itemNames))
  val agedBrieGen = itemGen(Gen.const(agedBrie))
  val sulfurasGen = itemGen(Gen.const(sulfuras))

  def nonLegendaryGen(sellInGen: Gen[Int] = sellInGen) =
    itemGen(itemNameGen filter (_ != sulfuras), sellInGen)

  def backstagePassGen(sellInGen: Gen[Int] = sellInGen) =
    itemGen(Gen.const(backstagePass), sellInGen)

  def nonBackstagePassGen(sellInGen: Gen[Int] = sellInGen) =
    itemGen(itemNameGen filter (_ != backstagePass), sellInGen)
}

trait GildedRoseHelpers {
  def copyItems(items: Seq[Item]) : Seq[Item] =
    items.map(_.copy())

  def update(item: Item): Item =
    GildedRose.update(item)

  def update(items: Seq[Item]): Seq[Item] =
    GildedRose.update(items)
}