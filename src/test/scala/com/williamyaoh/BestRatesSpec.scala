package com.williamyaoh

class BestRatesSpec extends BaseSpec {
  import BestRates.{Rate, CabinPrice, BestGroupPrice}

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
  }

  // A further test enhancement that we could make here is to "generate the answer,"
  // as described here: https://wickstrom.tech/2019-04-17-property-based-testing-in-a-screencast-editor-case-study-2.html
  // which is a powerful technique for giving PBT more oomph.
}