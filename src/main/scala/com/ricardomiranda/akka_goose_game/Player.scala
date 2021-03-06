package com.ricardomiranda.akka_goose_game

import java.util.Scanner

import scala.annotation.tailrec
import scala.util.Random

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}

sealed trait PCommand
case class AskSpace(from: ActorRef[PCommand]) extends PCommand
case class Move(r: Random = new Random(0)) extends PCommand
case class Prank(pranker: String, prankSpace: Int, r: Random = new Random(0), returnSpace: Int) extends PCommand
case class SetSpace(newSpace: Int, dices: (Int, Int) = (0, 0)) extends PCommand
case class StartPlayer(automaticDices: Boolean = true, nextPlayer: ActorRef[PCommand]) extends PCommand
case class TellSpace(space: Int) extends PCommand

class Player {
  case class State_(automaticDices: Boolean, name: String, nextPlayer: ActorRef[PCommand], space: Int = 0)

  val rest: Behavior[PCommand] = resting()

  val whatHappens: Map[String, Set[Int]] = Map(
    "Goose" -> Set(5, 9, 14, 18, 23, 27), 
    "Bridge" -> Set(6),
    "Final" -> Set(63))

  private def resting(): Behavior[PCommand] =
    Behaviors.receive[PCommand] { (ctx, msg) =>
      msg match {
        case StartPlayer(automaticDices, nextPlayer) =>
          val state: State_ = State_(
            automaticDices = automaticDices, 
              name = ctx.self.path.toString.split("/").last,
            nextPlayer = nextPlayer,
            space = 0)
          println(s"Player ${state.name} joined the Goose Game")
          playing(state = state)
        case _ =>
          Behaviors.same
      }
    }

  private[akka_goose_game] def playing(state: State_): Behavior[PCommand] =
    Behaviors.receive[PCommand] { (ctx, msg) =>
      msg match {
        case AskSpace(from) =>
          from ! TellSpace(space = state.space)
          Behaviors.same
        case Move(r) =>
          val newState: State_ = move(state = state, r = r)
          exceedsBoard(state = newState) match {
            case 0 =>
              println(s"Player ${state.name} wins the game!")
              Behaviors.stopped
            case _ =>
              state.nextPlayer ! Prank(pranker = state.name, prankSpace = newState.space, r = r, returnSpace = state.space)
          }
          playing(newState)
        case Prank(pranker, prankSpace, r, returnSpace) =>
          val newState: State_ = 
            pranker match {
              case x if x != state.name =>
                state.nextPlayer ! Prank(pranker = pranker, prankSpace = prankSpace, r = r, returnSpace = returnSpace)
                prank(state = state, prankSpace = prankSpace, returnSpace = returnSpace)
              case _ =>
                state.nextPlayer ! Move(r = r)
                state
            }
          playing(newState)
        case SetSpace(newSpace, dices) =>
          val newState: State_ = setSpace(state = state, newSpace = newSpace, dices = dices)
          playing(newState)
        case _ =>
          Behaviors.same
      }
    }

  private[akka_goose_game] def prank(state: State_, prankSpace: Int, returnSpace: Int): State_ = 
    prankSpace match {
      case state.space =>
        val newState: State_ = state.copy(space = returnSpace)
        println(s"On ${state.space} there is ${state.name}, who returns to ${newState.space}")
        newState
      case _ =>
        state
    }

  private[akka_goose_game] def setSpace(state: State_, newSpace: Int, dices: (Int, Int)): State_ = {
    val setSpaceState: State_ = state.copy(space = newSpace)
    val newState: State_ = checkSpace(state = setSpaceState, dices = dices)
    println(s"${state.name} is now on space ${newState.space}")
    newState
  }
 
  private[akka_goose_game] def move(state: State_, r: Random): State_ = {
    val dices: (Int, Int) = rollDices(state = state, r = r)
    val moveState: State_ = state.copy(space = state.space + dices._1 + dices._2)
    println(s"${state.name} rolls ${dices._1}, ${dices._2}. ${state.name} moves from ${state.space} to ${moveState.space}.")
    val newState: State_ = checkSpace(state = moveState, dices = dices)
    newState
  }

  private[akka_goose_game] def rollDices(state: State_, r: Random): (Int, Int) =
    state.automaticDices match { 
      case true => 
        val max: Int = 6
        (r.nextInt(max) + 1, r.nextInt(max) + 1)
      case false => 
        askUserForDices(playerName = state.name)
    }

  private[akka_goose_game] def askUserForDices(playerName: String): (Int, Int) = {
    println(s"Player ${playerName} please write your play - 2 dices separated by a space.")
    val input: String = readLine()
    val ExpectedPattern = "\\s*([1-6])\\s*([1-6])\\s*".r
    val ExpectedPattern(dice1, dice2) = input
    (dice1.toInt, dice2.toInt)
  }

  @tailrec
  private def checkSpace(state: State_, dices: (Int, Int)): State_ = 
    state.space match {
      case x if whatHappens("Goose").contains(x) =>
        val newState: State_ = state.copy(space = state.space + dices._1 + dices._2)
        println(s"The Goose. ${state.name} moves again and goes to ${newState.space}.")
        checkSpace(state = newState, dices = dices)
      case x if whatHappens("Bridge").contains(x) =>
        val newState: State_ = state.copy(space = 12)
        println(s"The Bridge. ${state.name} jumps to 12")
        checkSpace(state = newState, dices = dices)
      case _ if exceedsBoard(state = state) > 0 =>
        val newState: State_ = state.copy(space = whatHappens("Final").toList.head - List(0, exceedsBoard(state = state)).max)
        checkSpace(state = newState, dices = (0, 0))
      case _ =>
        state
    } 

  private[akka_goose_game] def exceedsBoard(state: State_): Int = state.space - whatHappens("Final").toList.head
}