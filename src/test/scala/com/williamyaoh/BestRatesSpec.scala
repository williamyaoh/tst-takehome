package com.williamyaoh

import cats.Foldable
import cats.syntax.traverse._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.Inspectors

class BestRatesSpec extends BaseSpec {
  import BestRates.{Rate, CabinPrice, BestGroupPrice}

  /** It's easier to work with individual groups rather than loose rates. */
  case class RateGroup(group: String, codes: Seq[String]) {
    def toRates: Seq[Rate] = 
      codes.map(Rate(_, group))
  }

  case class InputData(
    cabinCodes: Seq[String],
    rateGroups: Seq[RateGroup],
    cabinPrices: Seq[CabinPrice],
    rates: Seq[Rate]
  )

  val genCabinCode: Gen[String] =
    Gen.stringOfN(1, Gen.alphaUpperChar)
  val genCabinCodes: Gen[Set[String]] = 
    Gen.listOf(genCabinCode).map(_.to(Set))
  val genRateGroupCodes: Gen[Set[String]] =
    Gen.listOf(Gen.identifier).map(_.to(Set))
  def genRateGroups(groups: Set[String]): Gen[Seq[RateGroup]] =
    groups
      .to(Seq)
      .traverse(group => Gen.nonEmptyListOf(genCabinCode).map(rates => RateGroup(group, rates.distinct)))
  def genCabinPrices(cabinCodes: Seq[String], groups: Seq[RateGroup]): Gen[Seq[CabinPrice]] =
    // Need to deduplicate here because a given rate code might be in multiple groups,
    // don't want to output multiple prices for a given (cabin, rate) tuple
    val allRateCodes = groups.flatMap(_.codes).distinct
    (for {
      cabin <- cabinCodes
      rate <- allRateCodes
    } yield Gen.chooseNum(1.00, 1000.00).map(price => CabinPrice(cabin, rate, price)))
      .sequence
  def genCabinPricesWithHoles(cabinCodes: Seq[String], groups: Seq[RateGroup]): Gen[Seq[CabinPrice]] =
    for {
      prices <- genCabinPrices(cabinCodes, groups)
      withHoles <- Gen.someOf(prices)
    } yield withHoles.to(Seq)

  def genInputData(
    genCabin: ((Seq[String], Seq[RateGroup]) => Gen[Seq[CabinPrice]])
  ): Gen[InputData] = for {
    cabinCodes <- genCabinCodes.map(_.to(Seq))
    rateGroups <- genRateGroupCodes.flatMap(genRateGroups)
    prices <- genCabin(cabinCodes, rateGroups)
  } yield InputData(cabinCodes, rateGroups, prices, rateGroups.flatMap(_.toRates))
    
  describe("BestRates") {
    it("returns the right rates for the example data") {
      val output = BestRates.getBestGroupPrices(ExampleData.rates, ExampleData.cabinPrices)
      output should contain theSameElementsAs Seq(
        BestGroupPrice("CA", "M1", 200.00, "Military"),
        BestGroupPrice("CA", "S1", 225.00, "Senior"),
        BestGroupPrice("CB", "M1", 230.00, "Military"),
        BestGroupPrice("CB", "S1", 245.00, "Senior"),
      )
    }

    it("returns the right rates when some cabins don't have certain rate codes") {
      val output = BestRates.getBestGroupPrices(
        Seq(
          Rate("M1", "Military"),
          Rate("M2", "Military"),
        ),
        Seq(
          CabinPrice("CA", "M1", 225.00),
          CabinPrice("CB", "S1", 200.00),
        ),
      )
      output should contain theSameElementsAs Seq(
        BestGroupPrice("CA", "M1", 225.00, "Military"),
      )
    }

    it("returns the right rates when the rate groups overlap") {
      // There's no reason why different rate groups can't have the same rate codes.
      val output = BestRates.getBestGroupPrices(
        Seq(
          Rate("S1", "Senior"),
          Rate("S2", "Senior"),
          Rate("S2", "Priority"),
          Rate("P1", "Priority"),
        ),
        Seq(
          CabinPrice("CA", "S1", 300.00),
          CabinPrice("CA", "S2", 225.00),
          CabinPrice("CA", "P1", 275.00),
        )
      )
      output should contain theSameElementsAs Seq(
        BestGroupPrice("CA", "S2", 225.00, "Senior"),
        BestGroupPrice("CA", "S2", 225.00, "Priority"),
      )
    }

    // The example data shows a scenario where in every cabin type, there is always a
    // price for every rate code. These tests replicate that assumption in their data
    // generation.
    describe("with complete price data") {
      implicit val arbitraryInputData: Arbitrary[InputData] =
        Arbitrary(genInputData(genCabinPrices))
    
      it("all BestPrices returned must actually exist") {
        forAll { (input: InputData) =>
          val output = BestRates.getBestGroupPrices(input.rates, input.cabinPrices)
          Inspectors.forAll(output) { bestPrice =>
            assert(input.cabinPrices.find(_ == bestPrice.toCabinPrice).isDefined)
          }
        }
      }

      it("finding BestPrices is idempotent") {
        forAll { (input: InputData) =>
          val output1 = BestRates.getBestGroupPrices(input.rates, input.cabinPrices)
          val output2 = BestRates.getBestGroupPrices(input.rates, output1.map(_.toCabinPrice))
          output1 should contain theSameElementsAs output2
        }
      }

      it("total results equal the results for each individual rate group, combined") {
        forAll { (input: InputData) =>
          val totalResults = BestRates.getBestGroupPrices(input.rates, input.cabinPrices)
          val individualResults = input.rateGroups.map { group => 
            BestRates.getBestGroupPrices(group.toRates, input.cabinPrices)
          }
          Foldable[Seq].fold(individualResults) should contain theSameElementsAs totalResults
        }
      }

      it("total results equal the results for each individual cabin type, combined") {
        forAll { (input: InputData) =>
          val totalResults = BestRates.getBestGroupPrices(input.rates, input.cabinPrices)
          val individualResults = input.cabinCodes.map { code =>
            BestRates.getBestGroupPrices(input.rates, input.cabinPrices.filter(_.cabinCode == code))
          }
          Foldable[Seq].fold(individualResults) should contain theSameElementsAs totalResults
        }
      }

      it("permuting the prices doesn't change the output") {
        forAll { (input: InputData) =>
          val oracle = BestRates.getBestGroupPrices(input.rates, input.cabinPrices)
          val permutations = input.cabinPrices.permutations.take(3).to(Seq)
          Inspectors.forAll(permutations) { permutation =>
            BestRates.getBestGroupPrices(input.rates, permutation) should contain theSameElementsAs oracle
          }
        }
      }
    }

    // The example data shows a scenario where in every cabin type, there is always a
    // price for every rate code. However, to be more conservative, it would be better
    // to test when that assumption is violated as well, and certain prices may be
    // missing for a cabin type and rate code.
    describe("with incomplete price data") {
      implicit val arbitraryInputData: Arbitrary[InputData] =
        Arbitrary(genInputData(genCabinPricesWithHoles))

      it("all BestPrices returned must actually exist") {
        forAll { (input: InputData) =>
          val output = BestRates.getBestGroupPrices(input.rates, input.cabinPrices)
          Inspectors.forAll(output) { bestPrice =>
            assert(input.cabinPrices.find(_ == bestPrice.toCabinPrice).isDefined)
          }
        }
      }

      it("finding BestPrices is idempotent") {
        forAll { (input: InputData) =>
          val output1 = BestRates.getBestGroupPrices(input.rates, input.cabinPrices)
          val output2 = BestRates.getBestGroupPrices(input.rates, output1.map(_.toCabinPrice))
          output1 should contain theSameElementsAs output2
        }
      }

      it("total results equal the results for each individual rate group, combined") {
        forAll { (input: InputData) =>
          val totalResults = BestRates.getBestGroupPrices(input.rates, input.cabinPrices)
          val individualResults = input.rateGroups.map { group => 
            BestRates.getBestGroupPrices(group.toRates, input.cabinPrices)
          }
          Foldable[Seq].fold(individualResults) should contain theSameElementsAs totalResults
        }
      }

      it("total results equal the results for each individual cabin type, combined") {
        forAll { (input: InputData) =>
          val totalResults = BestRates.getBestGroupPrices(input.rates, input.cabinPrices)
          val individualResults = input.cabinCodes.map { code =>
            BestRates.getBestGroupPrices(input.rates, input.cabinPrices.filter(_.cabinCode == code))
          }
          Foldable[Seq].fold(individualResults) should contain theSameElementsAs totalResults
        }
      }

      it("permuting the prices doesn't change the output") {
        forAll { (input: InputData) =>
          val oracle = BestRates.getBestGroupPrices(input.rates, input.cabinPrices)
          val permutations = input.cabinPrices.permutations.take(3).to(Seq)
          Inspectors.forAll(permutations) { permutation =>
            BestRates.getBestGroupPrices(input.rates, permutation) should contain theSameElementsAs oracle
          }
        }
      }
    }
  }
  // A further test enhancement that we could make here is to "generate the answer,"
  // as described here: https://wickstrom.tech/2019-04-17-property-based-testing-in-a-screencast-editor-case-study-2.html
  // which is a powerful technique for giving PBT more oomph.
}