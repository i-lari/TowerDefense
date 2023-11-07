
import scala.collection.mutable._
import scala.util.Random
import scala.math._

class RoundGenerator(g: Game) {

  val attacker1Str = 1
  val attacker2Str = 5
  val attacker3Str = 10
  val startingStrength = 4
  val randGen = new Random(System.nanoTime())

  def generateRound(strength: Int, roundNumber: Int): Round = {
    var currentStrength = 0
    val attackerCounts = Array[Int](0, 0, 0)
    while (currentStrength < strength) {
      val rand = randGen.nextInt(100)
      if (roundNumber > 9 && currentStrength + attacker3Str <= strength && rand <= 10) {
        currentStrength += attacker3Str
        attackerCounts(2) += 1
      } else if (roundNumber > 3 && currentStrength + attacker2Str <= strength && rand <= 50) {
        currentStrength += attacker2Str
        attackerCounts(1) += 1
      } else {
        currentStrength += attacker1Str
        attackerCounts(0) += 1
      }
    }
    val attackers = Array[(Int, String)]((attackerCounts(0), "Attacker1"), (attackerCounts(1), "Attacker2"), (attackerCounts(2), "Attacker3"))
    new Round(g, attackers.toVector)
  }

  def generateRounds(howMany: Int): Buffer[Round] = {
    val roundBuffer: Buffer[Round] = Buffer()
    var multiplier = 1.5
    var roundStrenght = startingStrength
    var roundNumber = 1
    while (roundBuffer.size < howMany) {
      roundBuffer += generateRound(roundStrenght, roundNumber)
      roundStrenght = (roundStrenght * multiplier).toInt
      if (roundNumber == 4) multiplier = 1.25
      if (roundNumber == 8) multiplier = 1.2
      if (roundNumber == 11) multiplier = 1.15
      if (roundNumber == 13) multiplier = 1.1
      if (roundNumber == 16) multiplier = 1.07
      roundNumber += 1
    }
    roundBuffer
  }

}


class Round(g: Game, v: Vector[(Int, String)]) { // vector is amount of attackers and type as a string
  var attackers = v.toBuffer
  val game = g

  var ticker = 0
  val randGen = new Random(System.nanoTime())

  def spawnAttackers() = {
    if (!this.isDone && ticker >= 200) {
      val rand = randGen.nextInt(100)
      if (!this.isDone) {
        if (rand < 5 && attackers(2)._1 > 0) {
          attackers(2) = (attackers(2)._1 - 1, attackers(2)._2)
          game.createObject(new Attacker3(game, game.startTileLocation, (300 * game.scalingMultiplier).toInt, (5 * game.scalingMultiplier).toInt))
          ticker = min(game.scalingMultiplier.toInt * 10, 150) // spawn intervall is minimum of 50 ticks
        } else if (rand < 15 && attackers(1)._1 > 0) {
          attackers(1) = (attackers(1)._1 - 1, attackers(1)._2)
          game.createObject(new Attacker2(game, game.startTileLocation, (100 * game.scalingMultiplier).toInt))
          ticker = min(game.scalingMultiplier.toInt * 33 + 30, 180)
        } else {
          attackers(0) = (attackers(0)._1 - 1, attackers(0)._2)
          game.createObject(new Attacker1(game, game.startTileLocation, (30 * game.scalingMultiplier.toInt)))
          ticker = min(game.scalingMultiplier.toInt * 50 + 20, 195)
        }
      }
    }
    ticker += 1
  }

  def strength: Int = {
    var gatherer = 0
    for (a <- attackers) {
      a._2 match {
        case "Attacker1" => gatherer += a._1 * 1
        case "Attacker2" => gatherer += a._1 * 5
        case "Attacker3" => gatherer += a._1 * 10
      }
    }
    gatherer
  }

  def isDone: Boolean = attackers.map(_._1).forall(_ <= 0)

}
