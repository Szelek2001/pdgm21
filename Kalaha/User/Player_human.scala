package Kalaha.User

import Kalaha.Gameboard.Gameboard


class Player_human(val game: Gameboard) extends Player {

  override def make_move():Int =
    println("Podaj pole od 0")

    val number = scala.io.StdIn.readInt()
    number
}
