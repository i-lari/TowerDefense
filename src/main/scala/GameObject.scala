
import scala.math._

abstract class GameObject(g: Game, l: (Double, Double)) {
  val name: String
  val game: Game = g
  def update(): Unit
  var location: (Double, Double) = l
  def x: Int = round(location._1).toInt // rounded x and y should only be used for GUI
  def y: Int = round(location._2).toInt

  def delete() = {
    this match {
      case t: Tower => game.towers -= t
      case t: TileObject => game.tiles -= t
      case a: Attacker => game.attackers -= a
      case p: Projectile => game.projectiles -= p
      case explosion: Explosion => game.explosions -= explosion
    }
  }

  def xDiff(other : GameObject) = this.location._1-other.location._1
  def yDiff(other : GameObject) = this.location._2-other.location._2
  def distance(other: GameObject) = sqrt(pow(xDiff(other), 2) + pow(yDiff(other), 2))
  def distance(location: (Double, Double)) = sqrt(pow(this.location._1 - location._1, 2) + pow(this.location._2 - location._2, 2))

}


class Explosion(g: Game, val l: (Double, Double), var radius: Int, val damage: Double, var time: Int) extends GameObject(g, l) {
  override val name: String = "boom"
  var ticker = 0

  override def update(): Unit = {
    time -= 1
    ticker += 1
    if (ticker > time / 2) radius -= 10 else radius += 10
    if (time == 0) this.delete()

  }
}