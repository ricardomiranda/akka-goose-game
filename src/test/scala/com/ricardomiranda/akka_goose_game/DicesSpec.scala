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
      testKit.run(StartGame(isAutomaticDices = true))
      assert(testKit.isAlive)
    }
  }

  "A set of 2 dices" must {
    "send message" in {
      val probe: TestProbe[DCommand] = TestProbe[DCommand]()
      val dices: ActorRef[DCommand] = spawn(new Dices().rest)

      dices ! StartGame(isAutomaticDices = true)
      dices ! RollDices(from = probe.ref)

      probe.expectMessageType[DCommand]
    }
  }

  "A set of 2 dices with seed 0" must {
    "send message with dices (1, 5)" in {
      val probe: TestProbe[DCommand] = TestProbe[DCommand]()
      val dices: ActorRef[DCommand] = spawn(new Dices().rest)

      dices ! StartGame(isAutomaticDices = true)
      dices ! RollDices(from = probe.ref)

      probe.expectMessage(DicesValue(1, 5))
    }
  }

  "A set of 2 dices with seed 1090" must {
    "send message with dices (4, 3)" in {
      val probe: TestProbe[DCommand] = TestProbe[DCommand]()
      val dices: ActorRef[DCommand] = spawn(new Dices().rest)

      dices ! StartGame(isAutomaticDices = true)
      dices ! RollDices(from = probe.ref, seed = 1090)

      probe.expectMessage(DicesValue(4, 3))
    }
  }

  override def afterAll(): Unit = shutdownTestKit()
}