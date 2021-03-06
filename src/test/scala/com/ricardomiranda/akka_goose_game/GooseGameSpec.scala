package com.ricardomiranda.akka_goose_game

import akka.NotUsed
import akka.actor.testkit.typed.Effect.Spawned
import akka.actor.typed.scaladsl.Behaviors.Receive
import akka.actor.testkit.typed.scaladsl.BehaviorTestKit
import org.scalatest.{BeforeAndAfterAll, GivenWhenThen, Matchers, WordSpec}

class GooseGameSpec
  extends WordSpec
    with Matchers
    with BeforeAndAfterAll
    with GivenWhenThen {

  "A goose game" when {
    "created" should {
      "create players" that {
        "are two in number" in {
          Given("An empty game")
          When("it is created")
          val given: BehaviorTestKit[NotUsed] = BehaviorTestKit(GooseGame.root(List("Joe", "Anna"))(Some(true)))

          Then("two players should exist")
          val expected: Int = 2
          val actual: Int = given
            .retrieveAllEffects
            .filter(_.isInstanceOf[Spawned[Receive[Player]]])
            .size

          actual shouldEqual expected
        }

        "are named Ric and Anna" in {
          Given("An empty game")
          When("it is created")
          val given: BehaviorTestKit[NotUsed] = BehaviorTestKit(GooseGame.root(List("Ric", "Anna"))(Some(true)))

          Then("players Ric and Anna should exist")
          val expected = Set("Ric", "Anna")
          val actual: Set[String] = given
            .retrieveAllEffects
            .filter(_.isInstanceOf[Spawned[Receive[Player]]])
            .map(_.asInstanceOf[Spawned[Receive[Player]]].childName)
            .toSet

          actual shouldEqual expected
        }
      }
    }
  }
}