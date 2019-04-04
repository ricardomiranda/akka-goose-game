package com.ricardomiranda.akka_goose_game

import akka.actor.typed.ActorSystem
import akka.NotUsed

object Main extends App {
  print("Goose game")
  val system: ActorSystem[NotUsed] = ActorSystem(GooseGame.root(Nil)(None)(true), "GooseGame")
}
