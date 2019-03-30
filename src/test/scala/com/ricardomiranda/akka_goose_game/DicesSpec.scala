package com.ricardomiranda.akka_goose_game

import akka.actor.typed.{ActorRef, Behavior}
import akka.testkit.typed.scaladsl.{ActorTestKit, BehaviorTestKit, TestProbe}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpec}

class DicesSpec
  extends WordSpec
    with ActorTestKit
    with Matchers
    with BeforeAndAfterAll {

  "A set of 2 dices" must {
    "be alive" in {
      val testKit: BehaviorTestKit[DCommand] = BehaviorTestKit(new Dices().rest)
      testKit.run(StartGame(is_automatic_dices = true))
      assert(testKit.isAlive)
    }
  }

  "A set of 2 dices" must {
    "send message" in {
      val probe: TestProbe[DCommand] = TestProbe[DCommand]()
      val dices: ActorRef[DCommand] = spawn(new Dices().rest)

      dices ! StartGame(is_automatic_dices = true)
      dices ! RollDices(from = probe.ref)

      probe.expectMessageType[DCommand]
    }
  }

  "A set of 2 dices with seed 0" must {
    "send message with dices" in {
      val probe: TestProbe[DCommand] = TestProbe[DCommand]()
      val dices: ActorRef[DCommand] = spawn(new Dices().rest)

      dices ! StartGame(is_automatic_dices = true)
      dices ! RollDices(from = probe.ref)

      probe.expectMessage(DicesValue(1, 2))
    }
  }

  override def afterAll(): Unit = shutdownTestKit()
}