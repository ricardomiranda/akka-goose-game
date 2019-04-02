package com.ricardomiranda.akka_goose_game

import akka.actor.typed.{ActorRef, Behavior, Terminated}
import akka.actor.typed.scaladsl.Behaviors
import akka.NotUsed
import scala.util.Random
import scala.annotation.tailrec

object GooseGame {
  val root: Behavior[NotUsed] =
    Behaviors.setup { ctx =>
      @tailrec
      def addPlayers(automaticDices: Boolean, players: List[ActorRef[PCommand]] = Nil, continue: Boolean = true): List[ActorRef[PCommand]] =
        continue match {
          case true =>
            val currentPlayers: List[String] = players.map(_ => ctx.self.path.toString.split("/").reverse.head)
            val player: ActorRef[PCommand] = ctx.spawn(new Player().rest, askPlayerName(currentPlayers))
            ctx.watch(player)
            player ! StartPlayer(automaticDices = true)

            val morePlayers: String = readLine("Are there more players (y/n)? ").trim
              morePlayers match {
                case x if x == "y" =>
                  addPlayers(automaticDices = automaticDices, players = player :: players, continue = true)
                case _ =>
                  addPlayers(automaticDices = automaticDices, players = player :: players, continue = false)
              }
          case false =>
            players
        }

      val players:List[ActorRef[PCommand]] = addPlayers(automaticDices = automaticDices)

      Behaviors.receiveSignal {
        case (_, Terminated(ref)) => Behaviors.stopped
      }
    }

  @tailrec
  private[akka_goose_game] def askPlayerName(currentPlayers: List[String] = Nil): String = {
    println(s"Current users are: ${currentPlayers.mkString(", ")}")

    val name: String = readLine("What's your name? ").trim
    name match {
      case x if x.isEmpty() =>
        println(s"Name cannot be empty")
        askPlayerName(currentPlayers = currentPlayers)

      case x if currentPlayers.contains(x) =>
        println(s"Name already taken")
        askPlayerName(currentPlayers = currentPlayers)
      case x =>
        x 
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