package com.williamyaoh
        
import cats.syntax.traverse._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.Inspectors

// TODO add a unit test of no notcombinablewith at all, should just return
// a single combo

class PromotionsSpec extends BaseSpec {
  import Promotions.{Promotion, PromotionCombo}

  val genPromotionName: Gen[String] = Gen.stringOfN(2, Gen.alphaNumChar)
  val genPromotions: Gen[Seq[Promotion]] = for {
    names <- Gen.listOf(genPromotionName).map(_.distinct)
    invalids <- names.traverse { name => 
      val others = names.filter(_ != name)
      if others.isEmpty then Gen.const(Seq.empty)
      else Gen.listOf(Gen.oneOf(others)).map(_.distinct)
    }
  } yield names.zip(invalids).map { case (name, invalid) =>
    Promotion(code = name, notCombinableWith = invalid)
  }

  implicit val arbitraryPromotions: Arbitrary[Seq[Promotion]] = Arbitrary(genPromotions)

  describe("sanity checks for generators") {
    it("codes are unique") {
      forAll("promotions") { (promos: Seq[Promotion]) =>
        val allCodes = promos.map(_.code)
        allCodes.size should equal(allCodes.to(Set).size)
      }
    }

    it("generator always produces notCombinableWith that exists") {
      forAll("promotions") { (promos: Seq[Promotion]) =>
        val allCodes = promos.map(_.code).to(Set)
        val allInvalids = promos.flatMap(_.notCombinableWith).to(Set)

        assert(allInvalids.subsetOf(allCodes))
      }
    }
  }

  describe("Promotions") {
    it("returns the right combos for the sample data") {
      assert(Promotions.areSameCombos(
        Promotions.allCombinablePromotions(ExampleData.promotions),
        Seq(
          PromotionCombo(Seq("P1", "P2")),
          PromotionCombo(Seq("P1", "P4", "P5")),
          PromotionCombo(Seq("P2", "P3")),
          PromotionCombo(Seq("P3", "P4", "P5")),
        )
      ))

      assert(Promotions.areSameCombos(
        Promotions.combinablePromotions("P1", ExampleData.promotions),
        Seq(
          PromotionCombo(Seq("P1", "P2")),
          PromotionCombo(Seq("P1", "P4", "P5")),
        )
      ))

      assert(Promotions.areSameCombos(
        Promotions.combinablePromotions("P3", ExampleData.promotions),
        Seq(
          PromotionCombo(Seq("P3", "P2")),
          PromotionCombo(Seq("P3", "P4", "P5")),
        )
      ))
    }

    it("combos returned by combinablePromotions are always valid") {
      forAll("promotions") { (promos: Seq[Promotion]) =>
        if promos.isEmpty then 
          succeed
        else
          val codeLookup = promos.map(promo => promo.code -> promo.notCombinableWith.to(Set)).to(Map)
          val allCodes = promos.map(_.code).to(Set)
          val singleCode = promos.head.code
          val output = Promotions.combinablePromotions(singleCode, promos)
          // All of the promo codes in the output exist
          assert(output.flatMap(_.promotionCodes).to(Set).subsetOf(allCodes))
          // None of the combos contain codes that are invalid together
          Inspectors.forAll(output) { combo =>
            Inspectors.forAll(combo.promotionCodes) { code =>
              assert(codeLookup(code).intersect(combo.promotionCodes.to(Set)).isEmpty)
            }
          }
      }
    }

    it("combos returned by allCombinablePromotions are always valid") {
      forAll("promotions") { (promos: Seq[Promotion]) =>
        val codeLookup = promos.map(promo => promo.code -> promo.notCombinableWith.to(Set)).to(Map)
        val allCodes = promos.map(_.code).to(Set)
        val output = Promotions.allCombinablePromotions(promos)
        // All of the promo codes in the output exist
        assert(output.flatMap(_.promotionCodes).to(Set).subsetOf(allCodes))
        // None of the combos contain codes that are invalid together
        Inspectors.forAll(output) { combo =>
          Inspectors.forAll(combo.promotionCodes) { code =>
            assert(codeLookup(code).intersect(combo.promotionCodes.to(Set)).isEmpty)
          }
        }
      }
    }

    it("no combo in the output of allCombinablePromotions is the subset of another") {
      forAll("promotions") { (promos: Seq[Promotion]) =>
        val output = Promotions.allCombinablePromotions(promos)
        Inspectors.forAll(output.combinations(2).to(Seq)) { case Seq(x, y) =>
          val left = x.promotionCodes.to(Set)
          val right = y.promotionCodes.to(Set)

          assert(!left.subsetOf(right))
          assert(!right.subsetOf(left))
        }
      }
    }

    it("allCombinablePromotions is the union of all individual combinablePromotions") {
      import cats.syntax.foldable._

      def sortCodes(combo: PromotionCombo) =
        combo.copy(promotionCodes = combo.promotionCodes.sorted)

      forAll("promotions") { (promos: Seq[Promotion]) =>
        val allCodes = promos.map(_.code)
        val allCombos = Promotions.allCombinablePromotions(promos).map(sortCodes).to(Set)
        val individualCombos = allCodes.map { code => 
          Promotions.combinablePromotions(code, promos)
        }

        individualCombos.foldMap(_.map(sortCodes).to(Set)) should equal(allCombos)
      }
    }

    it("permuting the input promotions doesn't change the output of allCombinablePromotions") {
      forAll("promotions") { (promos: Seq[Promotion]) =>
        val oracle = Promotions.allCombinablePromotions(promos)
        val permutations = promos.permutations.take(3).to(Seq)

        Inspectors.forAll(permutations) { permutation =>
          assert(Promotions.areSameCombos(
            Promotions.allCombinablePromotions(permutation),
            oracle
          ))
        }             
      }
    }
  }
}