package com.gildedrose

import org.scalatest._

class GildedRoseSpec extends WordSpec with Matchers with Inspectors {
  import GildedRose.updateQuality

  val randomStuff     = "Stuff"
  val agedBrie        = "Aged Brie"
  val sulfuras        = "Sulfuras, Hand of Ragnaros"
  val backstagePasses = "Backstage passes to a TAFKAL80ETC concert"

  "updateQuality applied to a single item" should {
    "copy the argument and leave it unaffected" in {
      val original = Item(randomStuff, 1, 10)
      val copied   = original.copy()
      val updated  = updateQuality(original)

      original should not be theSameInstanceAs(copied)
      original should not be theSameInstanceAs(updated)

      original should equal(copied)
      original should not equal updated
    }

    "obey the conditions set out in the README" should {
      "double quality degradation rate once the sell by date passes" in {
        val unexpiredItem1 = Item(randomStuff, 1, 10)
        val unexpiredItem2 = updateQuality(unexpiredItem1)
        val unexpiredDelta = unexpiredItem2.quality - unexpiredItem1.quality

        val expiredItem1 = Item(randomStuff, 0, 10)
        val expiredItem2 = updateQuality(expiredItem1)
        val expiredDelta = expiredItem2.quality - expiredItem1.quality

        expiredDelta should be(2 * unexpiredDelta)
      }

      "not affect the quality of an item with quality >= 50" in {
        val initial = Item(randomStuff, 10, 50)
        val updated = updateQuality(initial)

        updated.quality should be < initial.quality
      }

      "increase the quality of 'Aged Brie' if it starts with quality < 50" in {
        val initial = Item(agedBrie, 10, 10)
        val updated = updateQuality(initial)

        updated.quality should be > initial.quality
      }

      "not affect the quality of 'Aged Brie' if it starts with quality >= 50" in {
        val initial = Item(agedBrie, 10, 50)
        val updated = updateQuality(initial)

        updated.quality should be(initial.quality)
      }

      "not affect the quality or sell in date of 'Sulfuras, Hand of Ragnaros'" in {
        val initial = Item(sulfuras, 10, 50)
        val updated = updateQuality(initial)

        updated should be(initial)
      }

      "increase the quality of backstage passes by 2 when there are 10 days or less left" in {
        val initial1 = Item(backstagePasses, 10, 40)
        val updated1 = updateQuality(initial1)

        updated1.quality should be(initial1.quality + 2)

        val initial2 = Item(backstagePasses, 6, 40)
        val updated2 = updateQuality(initial1)

        updated2.quality should be(initial2.quality + 2)
      }

      "not increase the quality of backstage passes when there are 10 days or less left and the quality is 50" in {
        val initial = Item(backstagePasses, 10, 50)
        val updated = updateQuality(initial)

        updated.quality should be(initial.quality)
      }

      "increase the quality of backstage passes by 3 when there are 5 days or less left" in {
        val initial1 = Item(backstagePasses, 5, 40)
        val updated1 = updateQuality(initial1)

        updated1.quality should be(initial1.quality + 3)

        val initial2 = Item(backstagePasses, 1, 40)
        val updated2 = updateQuality(initial1)

        updated2.quality should be(initial2.quality + 3)
      }

      "not increase the quality of backstage passes when there are 5 days or less left and the quality is 50" in {
        val initial = Item(backstagePasses, 5, 50)
        val updated = updateQuality(initial)

        updated.quality should be(initial.quality)
      }

      "set the quality of backstage passes to 0 when there no days left" in {
        val initial = Item(backstagePasses, 0, 40)
        val updated = updateQuality(initial)

        updated.quality should be(0)
      }

      "set the quality of backstage passes to 0 when there no days left, even if the quality starts at 50" in {
        val initial = Item(backstagePasses, 0, 50)
        val updated = updateQuality(initial)

        updated.quality should be(0)
      }

      "increase the quality of backstage passes by 1 when there are more than 10 days left (inferred from the README)" in {
        val initial = Item(backstagePasses, 11, 40)
        val updated = updateQuality(initial)

        updated.quality should be(initial.quality + 1)
      }

      "not increase the quality of backstage passes when there are more than 10 days left and the quality is 50 (inferred from the README)" in {
        val initial = Item(backstagePasses, 11, 50)
        val updated = updateQuality(initial)

        updated.quality should be(initial.quality)
      }
    }

    "updateQuality applied to a list of items" should {
      "update the entire list without affecting the originals" in {
        val originals = List(Item(randomStuff, 1, 10), Item(agedBrie, 2, 20))
        val copies    = originals.map(_.copy())
        val updated   = updateQuality(originals)

        forAll(originals zip updated) {
          case (original, copied) =>
            original.name should be(copied.name)
        }

        forAll(originals) { original =>
          forAll(copies) { copied =>
            original should not be theSameInstanceAs(copied)
          }

          forAll(updated) { updated =>
            original should not be theSameInstanceAs(updated)
          }
        }
      }
    }
  }
}
