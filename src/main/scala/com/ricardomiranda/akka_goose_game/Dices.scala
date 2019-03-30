package com.ricardomiranda.akka_goose_game

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}

sealed trait DCommand
case class DicesValue(dices: (Int, Int)) extends DCommand
case object EndGame extends DCommand
case class RollDices(from: ActorRef[DCommand], seed: Int = 0) extends DCommand
case class StartGame(is_automatic_dices: Boolean = true) extends DCommand

class Dices {
  case class State_(is_automatic_dices: Boolean)

  val rest: Behavior[DCommand] = resting()

  private def resting(): Behavior[DCommand] =
    Behaviors.receive[DCommand] { (ctx, msg) => //leave ctx for logging
      msg match {
        case StartGame(is_automatic_dices) =>
          rolling(state = State_(is_automatic_dices = is_automatic_dices))
        case _ =>
          Behaviors.same
      }
    }

  private[akka_goose_game] def rolling(state: State_): Behavior[DCommand] =
    Behaviors.receive[DCommand] { (ctx, msg) =>
      msg match {
        case RollDices(from, seed) =>
          from ! DicesValue(roll_dices(player_name = ctx.self.path.toString, state = state, seed = seed))
          Behaviors.same
        case EndGame =>
          Behaviors.stopped
        case _ =>
          Behaviors.same
      }
    }

  private[akka_goose_game] def roll_dices(player_name: String, state: State_, seed: Int): (Int, Int) =
    state.is_automatic_dices match { 
      case true => 
        //todo: random dices with seed
        (1, 2)
      case false => 
        println(s"please insert dices for player ${player_name}")
        //todo: read from cli dices value
        (2, 3)
    }
}




