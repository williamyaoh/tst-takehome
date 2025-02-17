package com.williamyaoh

import cats.Applicative
import org.scalacheck.Gen
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

trait BaseSpec extends AnyFunSpec with Matchers with ScalaCheckPropertyChecks {
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 250, minSize = 0, sizeRange = 8)

  implicit val applicativeGen: Applicative[Gen] = new Applicative[Gen] {
    override def pure[A](x: A): Gen[A] = Gen.const(x)
    override def map[A, B](fa: Gen[A])(f: A => B): Gen[B] = fa.map(f)
    override def ap[A, B](ff: Gen[A => B])(fa: Gen[A]): Gen[B] = for {
      f <- ff
      a <- fa
    } yield f(a)
  }
}