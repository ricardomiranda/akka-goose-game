package com.ricardomiranda.akka_goose_game

import akka.actor.typed._
import akka.actor.testkit.typed.scaladsl.{ActorTestKit, BehaviorTestKit, TestProbe}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpec}

class PlayerSpec
  extends WordSpec
    with Matchers
    with BeforeAndAfterAll {

  val testKit = ActorTestKit()


  "A player" must {
    "be alive" in {
      val testKit: BehaviorTestKit[PCommand] = BehaviorTestKit(new Player().rest)
      assert(testKit.isAlive)
    }
  }

  "A player" must {
    "send message" in {
      val player_1: ActorRef[PCommand] = testKit.spawn(new Player().rest, "Test_Player_01")
      val player_2: ActorRef[PCommand] = testKit.spawn(new Player().rest, "Test_Player_02")
      val probe: TestProbe[PCommand] = testKit.createTestProbe()

      player_1 ! StartPlayer(nextPlayer = player_2)
      player_2 ! StartPlayer(nextPlayer = player_1)
      player_1 ! AskSpace(from = probe.ref)

      probe.expectMessageType[PCommand]
    }
  }

   "A player with seed 0" must {
     "send message with position 12" in {
       val player_1: ActorRef[PCommand] = testKit.spawn(new Player().rest, "Test_Player_11")
       val player_2: ActorRef[PCommand] = testKit.spawn(new Player().rest, "Test_Player_12")
       val probe: TestProbe[PCommand] = testKit.createTestProbe()

       player_1 ! StartPlayer(nextPlayer = player_2)
       player_2 ! StartPlayer(nextPlayer = player_1)
       player_1 ! AskSpace(from = probe.ref)

       probe.expectMessage(TellSpace(space = 0))
     }
   }

  "A player with seed 1090" must {
    "send message with position 7" in {
      val player_1: ActorRef[PCommand] = testKit.spawn(new Player().rest, "Test_Player_21")
      val player_2: ActorRef[PCommand] = testKit.spawn(new Player().rest, "Test_Player_22")
      val probe: TestProbe[PCommand] = testKit.createTestProbe()

      player_1 ! StartPlayer(nextPlayer = player_2)
      player_2 ! StartPlayer(nextPlayer = player_1)
      player_1 ! Move()
      player_1 ! AskSpace(from = probe.ref)

      probe.expectMessage(TellSpace(space = 12))
    }
  }

  // "An invalidad message in resting behaviour" must {
  //   "not change state" in {
  //     val probe: TestProbe[PCommand] = TestProbe[PCommand]()
  //     val player: ActorRef[PCommand] = spawn(new Player().rest, "Yet_Another_Player")

  //     player ! StartMove(from = probe.ref)

  //     probe.expectNoMessage()
  //   }
  // }

  // "An invalidad message in playing behaviour" must {
  //   "not change state" in {
  //     val probe: TestProbe[PCommand] = TestProbe[PCommand]()
  //     val player: ActorRef[PCommand] = spawn(new Player().rest, "Another")

  //     player ! StartPlayer(automaticDices = true)
  //     player ! StartPlayer(automaticDices = true)

  //     probe.expectNoMessage()
  //   }
  // }

  // "An EndPalyer message" must {
  //   "stop player" in {
  //     val probe: TestProbe[PCommand] = TestProbe[PCommand]()
  //     val player: ActorRef[PCommand] = spawn(new Player().rest, "Player_stopped")

  //     player ! StartPlayer(automaticDices = true)
  //     player ! EndPlayer
  //     player ! StartMove(from = probe.ref)

  //     probe.expectNoMessage()
  //   }
  // }

  // "A SetSpace to 4 message" must {
  //   "send player to space 4" in {
  //     val probe: TestProbe[PCommand] = TestProbe[PCommand]()
  //     val player: ActorRef[PCommand] = spawn(new Player().rest, "Player_on_space_4")

  //     player ! StartPlayer(automaticDices = true)
  //     player ! SetSpace(from = probe.ref, newSpace = 4)

  //     probe.expectMessage(EndMove(player, 4))
  //   }
  // }

  // "A SetSpace to 5 (Goose) and dices 1, 2 message" must {
  //   "send player to space 8" in {
  //     val probe: TestProbe[PCommand] = TestProbe[PCommand]()
  //     val player: ActorRef[PCommand] = spawn(new Player().rest, "Player_on_space_5")

  //     player ! StartPlayer(automaticDices = true)
  //     player ! SetSpace(from = probe.ref, newSpace = 5, dices = (1, 2))

  //     probe.expectMessage(EndMove(player, 8))
  //   }
  // }

  // "A SetSpace to 14 (Goose) and dices 1, 3 message" must {
  //   "send player to space 22" in {
  //     val probe: TestProbe[PCommand] = TestProbe[PCommand]()
  //     val player: ActorRef[PCommand] = spawn(new Player().rest, "Player_on_space_14_1_3")

  //     player ! StartPlayer(automaticDices = true)
  //     player ! SetSpace(from = probe.ref, newSpace = 14, dices = (1, 3))

  //     probe.expectMessage(EndMove(player, 22))
  //   }
  // }

  // "A SetSpace to 6 (Bridge) and dices 1, 2 message" must {
  //   "send player to space 12" in {
  //     val probe: TestProbe[PCommand] = TestProbe[PCommand]()
  //     val player: ActorRef[PCommand] = spawn(new Player().rest, "Player_on_space_6_1_2")

  //     player ! StartPlayer(automaticDices = true)
  //     player ! SetSpace(from = probe.ref, newSpace = 6, dices = (1, 2))

  //     probe.expectMessage(EndMove(player, 12))
  //   }
  // }

  // "A SetSpace to 6 (Bridge) message" must {
  //   "send player to space 12" in {
  //     val probe: TestProbe[PCommand] = TestProbe[PCommand]()
  //     val player: ActorRef[PCommand] = spawn(new Player().rest, "Player_on_space_6")

  //     player ! StartPlayer(automaticDices = true)
  //     player ! SetSpace(from = probe.ref, newSpace = 6)

  //     probe.expectMessage(EndMove(player, 12))
  //   }
  // }

  // "A SetSpace to 57 message" must {
  //   "win" in {
  //     val probe: TestProbe[PCommand] = TestProbe[PCommand]()
  //     val player: ActorRef[PCommand] = spawn(new Player().rest, "Winner")

  //     player ! StartPlayer(automaticDices = true)
  //     player ! SetSpace(from = probe.ref, newSpace = 57)
  //     player ! StartMove(from = probe.ref)

  //     probe.expectMessageType[PCommand]
  //      probe.expectMessage(Winner(player))
  //   }
  // }

  // "A SetSpace to 62 message" must {
  //   "send player to space 58" in {
  //     val probe: TestProbe[PCommand] = TestProbe[PCommand]()
  //     val player: ActorRef[PCommand] = spawn(new Player().rest, "Player_on_space_62")

  //     player ! StartPlayer(automaticDices = true)
  //     player ! SetSpace(from = probe.ref, newSpace = 62)
  //     player ! StartMove(from = probe.ref)

  //     probe.expectMessageType[PCommand]
  //     probe.expectMessage(EndMove(player, 58))
  //   }
  // }

  // "A SetSpace to 60 message" must {
  //   "send player to space 60" in {
  //     val probe: TestProbe[PCommand] = TestProbe[PCommand]()
  //     val player: ActorRef[PCommand] = spawn(new Player().rest, "Player_on_space_60")

  //     player ! StartPlayer(automaticDices = true)
  //     player ! SetSpace(from = probe.ref, newSpace = 60)
  //     player ! StartMove(from = probe.ref)

  //     probe.expectMessageType[PCommand]
  //     probe.expectMessage(EndMove(player, 60))
  //   }
  // }

  // "A Prank message" must {
  //   "send player to space 30" in {
  //     val probe: TestProbe[PCommand] = TestProbe[PCommand]()
  //     val player: ActorRef[PCommand] = spawn(new Player().rest, "Prank_on_space_30")

  //     player ! StartPlayer(automaticDices = true)
  //     player ! SetSpace(from = probe.ref, newSpace = 33)
  //     player ! Prank(from = probe.ref, prankSpace = 33, returnSpace = 30)

  //     probe.expectMessageType[PCommand]
  //     probe.expectMessage(EndMove(player, 30))
  //   }
  // }

  override def afterAll(): Unit = testKit.shutdownTestKit()
}