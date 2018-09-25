import scala.io.StdIn
import scala.util.Random

object ScalaGoose {
  def readInput(input: String, positions: Map[String, Int]): (String, Map[String, Int]) = {
    val add = """add player (\w+)""".r
    val movDice = """move (\w+) ([1-6]), ([1-6])""".r
    val mov = """move (\w+)""".r
    input match {
      case add(player) =>
        if (positions.contains(player)) (s"$player: already existing player", positions)
        else {
          val temp = positions.+((player, 0))
          ("players: " + temp.keys.mkString(", "), temp)
        }
      case movDice(player, dice1, dice2) if positions.contains(player) =>
        movePlayer(player, dice1.toInt + dice2.toInt, s"$player rolls $dice1, $dice2. ", positions)
      case mov(player) if positions.contains(player) => {
        val r = new Random()
        val dice1 = r.nextInt(6) + 1
        val dice2 = r.nextInt(6) + 1
        movePlayer(player, dice1.toInt + dice2.toInt, s"$player rolls $dice1, $dice2. ", positions)
      }
      case _ => ("undefined input", positions)
    }
  }

  def movePlayer(player: String, dices: Int, msg: String, positions: Map[String, Int]): (String, Map[String, Int]) = {
    val init = positions(player)
    val initialStr = if (init==0) "Start" else init
    val finPos = movePlayer2(player, dices, positions(player), msg + s"$player moves from $initialStr to ")
    pranking(player, positions(player), finPos._1, positions, finPos._2)
  }

  def movePlayer2(player: String, dices: Int, startPos: Int, msg: String): (Int, String) = {
    val pos = startPos + dices
    pos match {
      case 5 | 9 | 14 | 18 | 23 | 27 =>
        movePlayer2(player, dices, pos, msg + s"$pos, The Goose. $player moves again and goes to ")
      case 6 => (12, msg + s"The Bridge. $player jumps to 12")
      case x if x > 63 => (126 - x, msg + s"$x. $player bounces! $player returns to " + (126 - x))
      case 63 => (pos, msg + s"$pos. $player wins!")
      case _ => (pos, msg + s"$pos")
    }
  }

  def pranking(player: String, startPos: Int, finPos: Int, positions: Map[String, Int], msg: String):
  (String, Map[String, Int]) = {
    positions.find(_._2 == finPos) match {
      case Some(sfigato) => {
        val temp = positions.updated(sfigato._1, startPos).updated(player, finPos)
        (msg + s"on $finPos there is $sfigato, who returns to $startPos", temp)
      }
      case None => (msg, positions.updated(player, finPos))
    }
  }

  def main(args: Array[String]): Unit = {
    def loop(string: String, map: Map[String, Int]): Unit = {
      val (output, positions) = readInput(string, map)
      println(output)
      loop(StdIn.readLine(), positions)
    }

    loop(StdIn.readLine(), Map())
  }
}