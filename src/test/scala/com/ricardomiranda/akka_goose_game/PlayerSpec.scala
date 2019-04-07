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
    "send message with position 12" in {
      val player_1: ActorRef[PCommand] = testKit.spawn(new Player().rest, "Test_Player_21")
      val player_2: ActorRef[PCommand] = testKit.spawn(new Player().rest, "Test_Player_22")
      val probe: TestProbe[PCommand] = testKit.createTestProbe()

      player_1 ! StartPlayer(nextPlayer = player_2)
      player_2 ! StartPlayer(nextPlayer = player_1)
      player_1 ! SetSpace(newSpace = 12)
      player_1 ! AskSpace(from = probe.ref)

      probe.expectMessage(TellSpace(space = 12))
    }
  }

  "An invalidad message in resting behaviour" must {
    "not change state" in {
      val player_1: ActorRef[PCommand] = testKit.spawn(new Player().rest, "Test_Player_31")
      val player_2: ActorRef[PCommand] = testKit.spawn(new Player().rest, "Test_Player_32")
      val probe: TestProbe[PCommand] = testKit.createTestProbe()

      player_1 ! Move()
      player_1 ! AskSpace(from = probe.ref)

      probe.expectNoMessage()
    }
  }

  "A SetSpace to 4 message" must {
    "send player to space 4" in {
      val player_1: ActorRef[PCommand] = testKit.spawn(new Player().rest, "Test_Player_41")
      val player_2: ActorRef[PCommand] = testKit.spawn(new Player().rest, "Test_Player_42")
      val probe: TestProbe[PCommand] = testKit.createTestProbe()

      player_1 ! StartPlayer(nextPlayer = player_2)
      player_1 ! SetSpace(newSpace = 4)
      player_1 ! AskSpace(from = probe.ref)

      probe.expectMessage(TellSpace(space = 4))
    }
  }

  "A SetSpace to 5 (Goose) and dices 1, 2 message" must {
    "send player to space 8" in {
      val player_1: ActorRef[PCommand] = testKit.spawn(new Player().rest, "Test_Player_51")
      val player_2: ActorRef[PCommand] = testKit.spawn(new Player().rest, "Test_Player_52")
      val probe: TestProbe[PCommand] = testKit.createTestProbe()

      player_1 ! StartPlayer(nextPlayer = player_2)
      player_1 ! SetSpace(newSpace = 5, dices = (1 ,2))
      player_1 ! AskSpace(from = probe.ref)

      probe.expectMessage(TellSpace(space = 8))
    }
  }

  "A SetSpace to 14 (Goose) and dices 1, 3 message" must {
    "send player to space 22" in {
      val player_1: ActorRef[PCommand] = testKit.spawn(new Player().rest, "Test_Player_61")
      val player_2: ActorRef[PCommand] = testKit.spawn(new Player().rest, "Test_Player_62")
      val probe: TestProbe[PCommand] = testKit.createTestProbe()

      player_1 ! StartPlayer(nextPlayer = player_2)
      player_1 ! SetSpace(newSpace = 14, dices = (1 ,3))
      player_1 ! AskSpace(from = probe.ref)

      probe.expectMessage(TellSpace(space = 22))
    }
  }

  "A SetSpace to 6 (Bridge) and dices 1, 2 message" must {
    "send player to space 12" in {
      val player_1: ActorRef[PCommand] = testKit.spawn(new Player().rest, "Test_Player_71")
      val player_2: ActorRef[PCommand] = testKit.spawn(new Player().rest, "Test_Player_72")
      val probe: TestProbe[PCommand] = testKit.createTestProbe()

      player_1 ! StartPlayer(nextPlayer = player_2)
      player_1 ! SetSpace(newSpace = 6, dices = (1 ,2))
      player_1 ! AskSpace(from = probe.ref)

      probe.expectMessage(TellSpace(space = 12))
    }
  }

  "A SetSpace to 6 (Bridge) message" must {
    "send player to space 12" in {
      val player_1: ActorRef[PCommand] = testKit.spawn(new Player().rest, "Test_Player_81")
      val player_2: ActorRef[PCommand] = testKit.spawn(new Player().rest, "Test_Player_82")
      val probe: TestProbe[PCommand] = testKit.createTestProbe()

      player_1 ! StartPlayer(nextPlayer = player_2)
      player_1 ! SetSpace(newSpace = 6)
      player_1 ! AskSpace(from = probe.ref)

      probe.expectMessage(TellSpace(space = 12))
    }
  }

  "A SetSpace to 62 message" must {
    "send player to space 58" in {
      val player_1: ActorRef[PCommand] = testKit.spawn(new Player().rest, "Test_Player_91")
      val player_2: ActorRef[PCommand] = testKit.spawn(new Player().rest, "Test_Player_92")
      val probe: TestProbe[PCommand] = testKit.createTestProbe()

      player_1 ! StartPlayer(nextPlayer = player_2)
      player_1 ! SetSpace(newSpace = 62)
      player_1 ! Move()
      player_1 ! AskSpace(from = probe.ref)

      probe.expectMessage(TellSpace(space = 58))
    }
  }

  "A SetSpace to 60 message" must {
    "send player to space 60" in {
      val player_1: ActorRef[PCommand] = testKit.spawn(new Player().rest, "Test_Player_101")
      val player_2: ActorRef[PCommand] = testKit.spawn(new Player().rest, "Test_Player_102")
      val probe: TestProbe[PCommand] = testKit.createTestProbe()

      player_1 ! StartPlayer(nextPlayer = player_2)
      player_1 ! SetSpace(newSpace = 60)
      player_1 ! Move()
      player_1 ! AskSpace(from = probe.ref)

      probe.expectMessage(TellSpace(space = 60))
    }
  }

  // "A Prank message" must {
  //   "send player to space 30" in {
  //     val player_1: ActorRef[PCommand] = testKit.spawn(new Player().rest, "Test_Player_111")
  //     val player_2: ActorRef[PCommand] = testKit.spawn(new Player().rest, "Test_Player_112")
  //     val probe: TestProbe[PCommand] = testKit.createTestProbe()

  //     player_1 ! StartPlayer(nextPlayer = player_2)
  //     player_1 ! SetSpace(newSpace = 33)
  //     player_1 ! Prank(pranker = "Test_Player_112", prankSpace = 33, returnSpace = 30)
  //     player_1 ! AskSpace(from = probe.ref)

  //     probe.expectMessage(TellSpace(space = 30))
  //   }
  // }

  override def afterAll(): Unit = testKit.shutdownTestKit()
}