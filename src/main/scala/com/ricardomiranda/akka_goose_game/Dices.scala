package com.ricardomiranda.akka_goose_game

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}

sealed trait DCommand
case class DicesValue(dices: (Int, Int)) extends DCommand
case object EndGame extends DCommand
case class RollDices(from: ActorRef[DCommand], seed: Int = 0) extends DCommand
case class StartGame(isAutomaticDices: Boolean = true) extends DCommand

class Dices {
  case class State_(isAutomaticDices: Boolean)

  val rest: Behavior[DCommand] = resting()

  private def resting(): Behavior[DCommand] =
    Behaviors.receive[DCommand] { (ctx, msg) => //leave ctx for logging
      msg match {
        case StartGame(isAutomaticDices) =>
          rolling(state = State_(isAutomaticDices = isAutomaticDices))
        case _ =>
          Behaviors.same
      }
    }

  private[akka_goose_game] def rolling(state: State_): Behavior[DCommand] =
    Behaviors.receive[DCommand] { (ctx, msg) =>
      msg match {
        case RollDices(from, seed) =>
          from ! DicesValue(rollDices(playerName = ctx.self.path.toString, state = state, seed = seed))
          Behaviors.same
        case EndGame =>
          Behaviors.stopped
        case _ =>
          Behaviors.same
      }
    }

  private[akka_goose_game] def rollDices(playerName: String, state: State_, seed: Int): (Int, Int) =
    state.isAutomaticDices match { 
      case true => 
        //todo: random dices with seed
        (1, 2)
      case false => 
        println(s"please insert dices for player ${playerName}")
        //todo: read from cli dices value
        (2, 3)
    }
}




