package com.ricardomiranda.akka_goose_game

import akka.actor.typed.{ActorRef, Behavior}
import akka.testkit.typed.scaladsl.{ActorTestKit, BehaviorTestKit, TestProbe}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpec}

class PlayerSpec
  extends WordSpec
    with ActorTestKit
    with Matchers
    with BeforeAndAfterAll {

  "A Player" must {
    "be alive" in {
      val testKit: BehaviorTestKit[PCommand] = BehaviorTestKit(new Player().rest)
      testKit.run(StartPlayer(automaticDices = true))
      assert(testKit.isAlive)
    }
  }

  "A player" must {
    "send message" in {
      val probe: TestProbe[PCommand] = TestProbe[PCommand]()
      val player: ActorRef[PCommand] = spawn(new Player().rest, "Test_Player")

      player ! StartPlayer(automaticDices = true)
      player ! StartMove(from = probe.ref)

      probe.expectMessageType[PCommand]
    }
  }

  "A player with seed 0" must {
    "send message with position 6" in {
      val probe: TestProbe[PCommand] = TestProbe[PCommand]()
      val player: ActorRef[PCommand] = spawn(new Player().rest, "Other_Player")

      player ! StartPlayer(automaticDices = true)
      player ! StartMove(from = probe.ref)

      probe.expectMessage(EndMove(player, 6))
    }
  }

  "A player with seed 1090" must {
    "send message with position 7" in {
      val probe: TestProbe[PCommand] = TestProbe[PCommand]()
      val player: ActorRef[PCommand] = spawn(new Player().rest, "Test_other_Player")

      player ! StartPlayer(automaticDices = true)
      player ! StartMove(from = probe.ref, seed = 1090)

      probe.expectMessage(EndMove(player, 7))
    }
  }

  "An invalidad message in resting behaviour" must {
    "not change state" in {
      val probe: TestProbe[PCommand] = TestProbe[PCommand]()
      val player: ActorRef[PCommand] = spawn(new Player().rest, "Yet_Another_Player")

      player ! StartMove(from = probe.ref)

      probe.expectNoMessage()
    }
  }

  "An invalidad message in playing behaviour" must {
    "not change state" in {
      val probe: TestProbe[PCommand] = TestProbe[PCommand]()
      val player: ActorRef[PCommand] = spawn(new Player().rest, "Another")

      player ! StartPlayer(automaticDices = true)
      player ! StartPlayer(automaticDices = true)

      probe.expectNoMessage()
    }
  }

  "An EndPalyer message" must {
    "stop player" in {
      val probe: TestProbe[PCommand] = TestProbe[PCommand]()
      val player: ActorRef[PCommand] = spawn(new Player().rest)

      player ! StartPlayer(automaticDices = true)
      player ! EndPlayer
      player ! StartMove(from = probe.ref)

      probe.expectNoMessage()
    }
  }

  override def afterAll(): Unit = shutdownTestKit()
}