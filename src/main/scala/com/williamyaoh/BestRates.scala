package com.williamyaoh

object BestRates {
  case class Rate(rateCode: String, rateGroup: String)
  case class CabinPrice(cabinCode: String, rateCode: String, price: BigDecimal)
  case class BestGroupPrice(cabinCode: String, rateCode: String, price: BigDecimal, rateGroup: String) {
    def toCabinPrice: CabinPrice =
      CabinPrice(cabinCode, rateCode, price)
  }

  import BestRates._

  /**
   * For each (Group, CabinType) tuple, find the lowest price ticket that the
   * group can take of that type of cabin. A rate group can take any ticket matching
   * one of its associated rate codes.
   */
  def getBestGroupPrices(rates: Seq[Rate], prices: Seq[CabinPrice]): Seq[BestGroupPrice] =
    val cabinCodes = prices.map(_.cabinCode).to(Set)
    val rateGroups: Map[String, Set[String]] = rates
      .groupBy(_.rateGroup)
      .map { case (k, v) => k -> v.map(_.rateCode).to(Set) }

    (for {
      cabin <- cabinCodes.to(Seq)
      (group, rates) <- rateGroups
    } yield {
      // We could speed this up by making a lookup from (cabincode, ratecode) -> price
      val groupPrices = rates.to(Seq).flatMap(rate => prices.find { price =>
        price.cabinCode == cabin && price.rateCode == rate
      }.map(p => rate -> p.price))
      groupPrices
        .minByOption { case (_, price) => price }
        .map { case (rate, price) => BestGroupPrice(cabin, rate, price, group) }
    }).flatMap(_.to(Seq))
}