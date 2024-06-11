package com.williamyaoh

import scala.collection.immutable.Queue

object Promotions {
  case class Promotion(code: String, notCombinableWith: Seq[String])
  case class PromotionCombo(promotionCodes: Seq[String])

  import Promotions._

  /**
   * Given a set of promotions, find the maximum combos that can be created by
   * combining promotions together.
   */
  def allCombinablePromotions(allPromotions: Seq[Promotion]): Seq[PromotionCombo] =
    // Try as I might, I can't think of a way to get this faster than O(2^n), since
    // we essentially have to iterate over (n choose n) + (n choose n-1) + (n choose n-2)...
    // combinations. This shouldn't be used for any more than 10 promotions.
    //
    // The approach here is essentially exactly that: we iterate over all the (n choose k)
    // combinations, starting from the largest and working our way down to (n choose 1).
    // We check each combination we find for validity (i.e. nothing in the combination
    // is uncombinable with something else in the combination), and accumulate valid combos.
    //
    // To avoid including non-maximal combos (e.g. if {P1, P2, P4} is a valid combo, then
    // {P2, P4} is as well, but we only want to output the former), we take advantage of
    // the fact that if some combo is valid, then *all of its subsets can be skipped*,
    // since that combo would subsume the subsets. Since we're iterating from the *largest*
    // combinations first, we'll encounter the maximal combos first and thus remove all
    // of its subsets from consideration.
    val lookup = allPromotions.map(promo => promo.code -> promo.notCombinableWith).to(Map)

    // We assume that:
    //   1. The notCombinableWith relation is symmetrical, but
    //   2. It may not necessarily be *provided* symmetrically in the input data.
    // Additionally, if a promo is marked as uncombinable with itself, we honor that
    // and refuse to include it in any combos. Unclear whether that behavior should
    // be allowed, but the types don't prevent it, so...

    def toValidCombo(promos: Seq[String]): Option[PromotionCombo] =
      val allValid = promos.forall { code =>
        lookup(code).intersect(promos).isEmpty
      }
      if allValid && promos.nonEmpty then Some(PromotionCombo(promos)) else None

    def loop(rem: Queue[Seq[String]], seen: Set[Set[String]], acc: Seq[PromotionCombo]): Seq[PromotionCombo] =
      rem.dequeueOption match
        case None => acc
        case Some((codes, nextq)) => 
          if seen.contains(codes.to(Set)) then loop(nextq, seen, acc)
          else toValidCombo(codes) match
            case Some(combo) => 
              loop(nextq, seen ++ codes.to(Set).subsets, combo +: acc)
            case None => 
              val updated = if codes.size > 1 
                then nextq.enqueueAll(codes.combinations(codes.size - 1).to(Seq))
                else nextq
              loop(updated, seen, acc)

    loop(Queue(allPromotions.map(_.code)), Set.empty, Seq.empty)

  /** Like allCombinablePromotions, but only return combos containing a specific promo. */
  def combinablePromotions(promotionCode: String, allPromotions: Seq[Promotion]): Seq[PromotionCombo] =
    // It's at least as hard to write this directly for a single promotion as it is
    // for all promotions, so it's easier to just filter down the output of
    // allCombinablePromotions to combos that contain this code.
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