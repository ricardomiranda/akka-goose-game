package com.ricardomiranda.akka_goose_game

import akka.actor.typed.ActorSystem
import akka.NotUsed

object Main extends App {
  val system: ActorSystem[NotUsed] = ActorSystem(Game.root, "GooseGame")
}
