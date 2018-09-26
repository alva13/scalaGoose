import scala.io.StdIn
import scala.util.Random

object ScalaGoose {

  class dice {
    val r = new Random()

    def launch: Int = r.nextInt(6) + 1
  }

  type table = Map[String, Int]

  val dice = new dice
  val add = """add player (\w+)""".r
  val movDice = """move (\w+) ([1-6]), ([1-6])""".r
  val mov = """move (\w+)""".r

  def addPlayer(player: String, positions: table): (String, table) = {
    if (positions.contains(player)) (s"$player: already existing player", positions)
    else {
      val temp = positions.+((player, 0))
      ("players: " + temp.keys.mkString(", "), temp)
    }
  }

  def movDice(player: String, dice1: String, dice2: String, positions: table): (String, table) = {
    movePlayer(player, dice1.toInt + dice2.toInt, s"$player rolls $dice1, $dice2. ", positions)
  }

  def mov(player: String, positions: table): (String, table) = {
    val dice1 = dice.launch
    val dice2 = dice.launch
    movePlayer(player, dice1 + dice2, s"$player rolls $dice1, $dice2. ", positions)
  }

  def readInput(input: String, positions: table): (String, table) = {
    input match {
      case add(player) => addPlayer(player, positions)
      case movDice(player, dice1, dice2) if positions.contains(player) => movDice(player, dice1, dice2, positions)
      case mov(player) if positions.contains(player) => mov(player, positions)
      case _ => ("undefined input", positions)
    }
  }

  def movePlayer(player: String, dices: Int, msg: String, positions: table): (String, table) = {
    val init = positions(player)
    val initialStr = if (init == 0) "Start" else init
    val finPos = movePlayer2(player, dices, positions(player), msg + s"$player moves from $initialStr to ")
    pranking(player, positions(player), finPos._1, positions, finPos._2)
  }

  def movePlayer2(player: String, dices: Int, startPos: Int, msg: String): (Int, String) = {
    val pos = startPos + dices
    pos match {
      case 5 | 9 | 14 | 18 | 23 | 27 =>
        movePlayer2(player, dices, pos, msg + s"$pos, The Goose. $player moves again and goes to ")
      case 6 => (12, msg + s"The Bridge. $player jumps to 12")
      case x if x > 63 => (126 - x, msg + s"$x. $player bounces! $player returns to " + (126 - x) + ". ")
      case 63 => (pos, msg + s"$pos. $player wins!")
      case _ => (pos, msg + s"$pos. ")
    }
  }

  def pranking(player: String, startPos: Int, finPos: Int, positions: table, msg: String):
  (String, table) = {
    positions.find(_._2 == finPos) match {
      case Some((sfigato, _)) => {
        val temp = positions.updated(sfigato, startPos).updated(player, finPos)
        (msg + s"on $finPos there is $sfigato, who returns to $startPos. ", temp)
      }
      case None => (msg, positions.updated(player, finPos))
    }
  }

  def main(args: Array[String]): Unit = {
    def loop(string: String, map: table): Unit = {
      val (output, positions) = readInput(string, map)
      println(output)
      loop(StdIn.readLine(), positions)
    }

    loop(StdIn.readLine(), Map())
  }
}