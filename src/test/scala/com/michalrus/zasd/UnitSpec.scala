package com.michalrus.zasd

import org.scalatest.concurrent.Timeouts
import org.scalatest.{ Matchers, WordSpec }
import org.scalatest.prop.Checkers

abstract class UnitSpec extends WordSpec with Checkers with Matchers with Timeouts
