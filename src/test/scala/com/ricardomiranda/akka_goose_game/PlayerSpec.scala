package com.ricardomiranda.akka_goose_game

import akka.actor.typed.{ActorRef, Behavior}
import akka.testkit.typed.scaladsl.{ActorTestKit, BehaviorTestKit, TestProbe}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpec}

class PlayerSpec
  extends WordSpec
    with ActorTestKit
    with Matchers
    with BeforeAndAfterAll {

  "A player" must {
    "send message" in {
      val probe: TestProbe[PCommand] = TestProbe[PCommand]()
      val dices: ActorRef[DCommand] = spawn(new Dices().rest)
      val player: ActorRef[PCommand] = spawn(new Player().rest, "Test_Player")

      player ! StartPlayer(dices = dices.ref)
      player ! StartMove(from = probe.ref)

      probe.expectMessageType[PCommand]
    }
  }

  "A player with seed 0" must {
    "send message with position 5" in {
      val probe: TestProbe[PCommand] = TestProbe[PCommand]()
      val dices: ActorRef[DCommand] = spawn(new Dices().rest)
      val player: ActorRef[PCommand] = spawn(new Player().rest, "Test_other_Player")

      dices ! StartGame(isAutomaticDices = true)
      player ! StartPlayer(dices = dices.ref)
      player ! StartMove(from = probe.ref)

      probe.expectMessage(EndMove(player, 0))
    }
  }

  // "A set of 2 dices with seed 1090" must {
  //   "send message with dices (4, 3)" in {
  //     val probe: TestProbe[DCommand] = TestProbe[DCommand]()
  //     val dices: ActorRef[DCommand] = spawn(new Dices().rest)

  //     dices ! StartGame(isAutomaticDices = true)
  //     dices ! RollDices(from = probe.ref, seed = 1090)

  //     probe.expectMessage(DicesValue(4, 3))
  //   }
  // }

  // "An invalidad message in resting behaviour" must {
  //   "not change state" in {
  //     val probe: TestProbe[PCommand] = TestProbe[PCommand]()
  //     val dices: ActorRef[PCommand] = spawn(new Player().rest)

  //     dices ! RollDices(from = probe.ref, seed = 1090)

  //     probe.expectNoMessage()
  //   }
  // }

  // "An invalidad message in rolling behaviour" must {
  //   "not change state" in {
  //     val probe: TestProbe[DCommand] = TestProbe[DCommand]()
  //     val dices: ActorRef[DCommand] = spawn(new Dices().rest)

  //     dices ! StartGame(isAutomaticDices = true)
  //     dices ! StartGame(isAutomaticDices = true)

  //     probe.expectNoMessage()
  //   }
  // }

  // "An EndGame message" must {
  //   "stop dices" in {
  //     val probe: TestProbe[DCommand] = TestProbe[DCommand]()
  //     val dices: ActorRef[DCommand] = spawn(new Dices().rest)

  //     dices ! StartGame(isAutomaticDices = true)
  //     dices ! EndGame
  //     dices ! RollDices(from = probe.ref, seed = 1090)

  //     probe.expectNoMessage()
  //   }
  // }

  override def afterAll(): Unit = shutdownTestKit()
}