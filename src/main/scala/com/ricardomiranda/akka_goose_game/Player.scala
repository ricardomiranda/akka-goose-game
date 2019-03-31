package com.ricardomiranda.akka_goose_game

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}

sealed trait PCommand
case class EndMove(who: ActorRef[PCommand], position: Int) extends PCommand
case object EndPlayer extends PCommand
case class StartMove(from: ActorRef[PCommand], seed: Int = 0) extends PCommand
case class StartPlayer(dices: ActorRef[DCommand]) extends PCommand

class Player {
  case class State_(position: Int = 0, dices: ActorRef[DCommand])

  val rest: Behavior[PCommand] = resting()

  private def resting(): Behavior[PCommand] =
    Behaviors.receive[PCommand] { (ctx, msg) =>
      msg match {
        case StartPlayer(dices: ActorRef[DCommand]) =>
          println(s"Player ${ctx.self.path.toString.split("/").last} joined the Goose Game")
          playing(state = State_(position = 0, dices = dices))
        case _ =>
          Behaviors.same
      }
    }

  private[akka_goose_game] def playing(state: State_): Behavior[PCommand] =
    Behaviors.receive[PCommand] { (ctx, msg) =>
      msg match {
        case StartMove(from, seed) =>
          val newState = move(state = state, seed)
          from ! EndMove(ctx.self, newState.position)
          playing(newState)
        case EndPlayer =>
          Behaviors.stopped
        case _ =>
          Behaviors.same
      }
    }
 
  private[akka_goose_game] def move(state: State_, seed: Int): State_ = {
    state
  }
}