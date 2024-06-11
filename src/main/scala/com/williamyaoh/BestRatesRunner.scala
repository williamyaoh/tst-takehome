package com.williamyaoh

object BestRatesRunner extends App {
  println(">>> calculating best rates for sample data")
  println(">>> rates:")
  println(ExampleData.rates.mkString("\n"))
  println(">>> cabin prices:")
  println(ExampleData.cabinPrices.mkString("\n"))
  println(">>> best group prices:")
  println(BestRates.getBestGroupPrices(ExampleData.rates, ExampleData.cabinPrices).mkString("\n"))
}