package com.ricardomiranda.akka_goose_game

import akka.actor.typed.{ActorRef, Behavior, Terminated}
import akka.actor.typed.scaladsl.Behaviors
import akka.NotUsed
import scala.util.Random
import scala.annotation.tailrec

object GooseGame {
  val root: List[String] => Option[Boolean] => Boolean => Behavior[NotUsed] = playersInit => automaticDicesInit => play => Behaviors.setup { ctx =>
    @tailrec
    def addPlayers(automaticDices: Boolean, playersName: List[String], players: List[ActorRef[PCommand]] = Nil): List[ActorRef[PCommand]] =
      playersName match {
        case x :: xs =>
          val player: ActorRef[PCommand] = ctx.spawn(new Player().rest, x)
          ctx.watch(player)
          addPlayers(automaticDices = automaticDices, playersName = xs, players = player :: players)
        case Nil =>
          players
      }

    @tailrec
    def startPlayers(players: List[ActorRef[PCommand]], firstPlayer: ActorRef[PCommand]): Unit = 
      players match {
        case x :: y :: xs =>
          x ! StartPlayer(automaticDices = true, nextPlayer = y)
          startPlayers(players = y :: xs, firstPlayer = firstPlayer)
        case x :: Nil =>
          x ! StartPlayer(automaticDices = true, nextPlayer = firstPlayer)
          startPlayers(players = Nil, firstPlayer = firstPlayer)
        case Nil => // exit
      }

    val listOfPlayers: List[String] = 
      playersInit match {
        case Nil => 
          goosePlayers()
        case xs =>
          xs
      }

    val areAutomaticDices: Boolean =
      automaticDicesInit match { 
        case None => 
          automaticDices
        case Some(b) =>
          b
      }

    val players: List[ActorRef[PCommand]] = addPlayers(automaticDices = areAutomaticDices, playersName = listOfPlayers)
    startPlayers(players = players, firstPlayer = players.head)
    
    if (play) {
      println(s":::::::::::::::::::::::::::::::::::::::")
      players.head ! Move()
    }

    Behaviors.receiveSignal {
      case (_, Terminated(ref)) => Behaviors.stopped
    }
  }
  
  @tailrec
  private[akka_goose_game] def goosePlayers(currentPlayers: List[String] = Nil): List[String] = {
    @tailrec
    def askPlayerName: String = {
      println(s"Current users are: ${currentPlayers.mkString(", ")}")

      val name: String = readLine("What's your name? \n").trim
      name match {
        case x if x.isEmpty() =>
          println(s"Name cannot be empty")
          askPlayerName
        case x if currentPlayers.contains(x) =>
          println(s"Name already taken")
          askPlayerName
        case x =>
          x 
      }
    }

    val players: List[String] = askPlayerName :: currentPlayers

    val morePlayers: String = readLine("Are there more players (y/n)? \n").trim
    morePlayers match {
      case x if x == "y" =>
        goosePlayers(currentPlayers = players)
      case _ =>
        players
    }
  }

  private[akka_goose_game] def automaticDices: Boolean = {
    val automaticDices: String = readLine("Are dices automatic (y/n)? ").trim
    automaticDices match {
      case x if x == "y" =>
        true
      case _ =>
        false
    }
  }
}