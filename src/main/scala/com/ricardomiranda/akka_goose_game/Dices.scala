package com.ricardomiranda.akka_goose_game

import java.util.Scanner

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
          from ! DicesValue(roll(playerName = from.path.toString, state = state, seed = seed))
          Behaviors.same
        case EndGame =>
          Behaviors.stopped
        case _ =>
          Behaviors.same
      }
    }


  private[akka_goose_game] def roll(playerName: String, state: State_, seed: Int): (Int, Int) =
    state.isAutomaticDices match { 
      case true => 
        val r = new scala.util.Random(seed)
        val max = 6
        (r.nextInt(max) + 1, r.nextInt(max) + 1)
      case false => 
        askUser(playerName = playerName)
    }

  private[akka_goose_game] def askUser(playerName: String): (Int, Int) = {
    println(s"Player ${playerName} please write your play - 2 dices separated by a space.")
    val input = readLine()
    val ExpectedPattern = "\\s*([1-6])\\s*([1-6])\\s*".r
    val ExpectedPattern(dice1, dice2) = input
    (dice1.toInt, dice2.toInt)
  }
}




