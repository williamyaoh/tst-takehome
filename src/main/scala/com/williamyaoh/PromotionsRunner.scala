package com.williamyaoh

object PromotionsRunner extends App {
  println(">>> calculating promotion combos for sample data")
  println(">>> promotions:")
  println(ExampleData.promotions.mkString("\n"))
  println(">>> allCombinablePromotions():")
  println(Promotions.allCombinablePromotions(ExampleData.promotions).mkString("\n"))
  println(">>> combinablePromotions(P1):")
  println(Promotions.combinablePromotions("P1", ExampleData.promotions).mkString("\n"))
  println(">>> combinablePromotions(P3):")
  println(Promotions.combinablePromotions("P3", ExampleData.promotions).mkString("\n"))
}