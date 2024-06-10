package com.williamyaoh

object Promotions {
  case class Promotion(code: String, notCombinableWith: Seq[String])
  case class PromotionCombo(promotionCodes: Seq[String])

  import Promotions._

  /**
   * Because the output of this is "maximal" combos, where we want to include
   * as many promotion codes as we can, we can define this recursively:
   * 
   *   allCombinablePromotions({}) = {}
   *   allCombinablePromotions({X}) = {X}
   *   allCombinablePromotions({X1, X2, ...}) =
   *     X1 added to as many combos in the output of allCombinablePromotions({X2, ...})
   *     as possible
   *   where "adding X1 to the combos" means, say we have S = allCombinablePromotions({X2, ...})
   *   some combos in S can also be combined with X1, while others will already contain
   *   promotions that cannot be combined with X1. We add X1 to the former, while
   *   keeping the latter unchanged, in the output of allCombinablePromotions. Note that
   *   if *none* of the combos from the recursive call can include X1, the singleton
   *   combo {X1} is maximal.
   *
   * The base cases are maximal and correct by inspection. For the recursive case,
   */
  def allCombinablePromotions(allPromotions: Seq[Promotion]): Seq[PromotionCombo] =
    ???

  /**
   * It's at least as hard to write this directly for a single promotion as it is
   * for all promotions, so it's easier to just filter down the output of
   * allCombinablePromotions to combos that contain this code.
   */
  def combinablePromotions(promotionCode: String, allPromotions: Seq[Promotion]): Seq[PromotionCombo] =
    allCombinablePromotions(allPromotions).filter { combo =>
      combo.promotionCodes.find(_ == promotionCode).isDefined
    }

  // My assumption is that neither the order of the combos in the output, nor the order 
  // of the codes inside a combo matter in terms of equality. Depending on how this
  // data gets used, it might be better to store everything as Set instead of Seq above.
  // But I'll keep it the way it is in the example input. Let's say it's legacy code or
  // an external library we have to interface with ;)

  def areSameCombos(combos1: Seq[PromotionCombo], combos2: Seq[PromotionCombo]): Boolean =
    def sortCodes(combo: PromotionCombo) = 
      combo.copy(promotionCodes = combo.promotionCodes.sorted)
    combos1.map(sortCodes).to(Set) == combos2.map(sortCodes).to(Set)
}