package com.ricardomiranda.akka_goose_game

import java.util.Scanner

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}

sealed trait PCommand
case class EndMove(who: ActorRef[PCommand], position: Int) extends PCommand
case object EndPlayer extends PCommand
case class StartMove(from: ActorRef[PCommand], seed: Int = 0) extends PCommand
case class StartPlayer(automaticDices: Boolean = true) extends PCommand

class Player {
  case class State_(automaticDices: Boolean, name: String, position: Int = 0)

  val rest: Behavior[PCommand] = resting()

  private def resting(): Behavior[PCommand] =
    Behaviors.receive[PCommand] { (ctx, msg) =>
      msg match {
        case StartPlayer(automaticDices) =>
          val state = State_(automaticDices = automaticDices, name = ctx.self.path.toString.split("/").reverse.head, position = 0)
          println(s"Player ${state.name} joined the Goose Game")
          playing(state = state)
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
    val dices = rollDices(state = state, seed = seed)
    val newState = state.copy(position = state.position + dices._1 + dices._2)
    newState
  }

  private[akka_goose_game] def rollDices(state: State_, seed: Int): (Int, Int) =
    state.automaticDices match { 
      case true => 
        val r = new scala.util.Random(seed)
        val max = 6
        (r.nextInt(max) + 1, r.nextInt(max) + 1)
      case false => 
        askUserForDices(playerName = state.name)
    }

  private[akka_goose_game] def askUserForDices(playerName: String): (Int, Int) = {
    println(s"Player ${playerName} please write your play - 2 dices separated by a space.")
    val input = readLine()
    val ExpectedPattern = "\\s*([1-6])\\s*([1-6])\\s*".r
    val ExpectedPattern(dice1, dice2) = input
    (dice1.toInt, dice2.toInt)
  }
}