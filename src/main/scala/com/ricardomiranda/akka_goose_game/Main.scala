package com.ricardomiranda.akka_goose_game

import akka.actor.typed.ActorSystem
import akka.NotUsed

object Main extends App {
  println("Goose game - Start")
  val system: ActorSystem[NotUsed] = ActorSystem(GooseGame.root(Nil)(None), "GooseGame")
}
