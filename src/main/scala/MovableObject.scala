
import scala.collection.mutable._
import scala.math._

abstract class MovableObject(g:Game,l: (Double, Double)) extends GameObject(g,l) {
  var speed : Double
  var destination: (Double, Double)
}

abstract class Projectile(g: Game, l: (Double, Double),d:(Double,Double)) extends MovableObject(g,l) {
  override val game: Game = g
  override var destination: (Double, Double) = d

  var dx = ((destination._1 - this.location._1) / distance(destination))
  var dy = ((destination._2 - this.location._2) / distance(destination))

  def move(speed: Double) = location = (this.location._1 + dx * speed, this.location._2 + dy * speed)

}

class Bullet(g: Game,val damage:Int, l: (Double, Double), d: (Double, Double)) extends Projectile(g, l, d) {

  override val name: String = "bullet"
  var speed = 15
  val dist = distance(destination)
  var ticker = 0
  val hitbox = 30

  override def update(): Unit = {
    ticker += 1
    this.move(speed)
    if (!g.attackers.map(_.distance(this)).forall(_ > hitbox)) game.impact(this, game.attackers.minBy(_.distance(this))) // if bullet gets closer than its hitbox it will impact the attacker
    if (ticker * speed > dist) this.delete()       // bullet will not go past the towers' range
  }
}

class Missile(g: Game, l: (Double, Double), attacker: Attacker, val baseDamage: Double, val baseSpeed: Double, explodeRadius: Int) extends Projectile(g, l, attacker.location) {
  override var speed: Double = baseSpeed
  override val name: String = "missile"

  private def explode() = {
  game.createObject(new Explosion(g,this.location,explodeRadius,((baseDamage*speed)/60),20))  //missile will deal damage that scales with its speed
  this.delete()
  }

  override def update(): Unit = {
   dx = ((destination._1 - this.location._1) / distance(destination))
   dy = ((destination._2 - this.location._2) / distance(destination))
   destination = Option(attacker.location).getOrElse(destination)   // this projectile is seeking. If the original target dies it explodes where it died.
    move(speed)
    if(speed<100) speed = speed * 1.05
    if(this.distance(destination) < 25) this.explode()
  }
}
class SpikeBall(g: Game,val damage:Int, durability : Int, l: (Double, Double), d: (Double, Double)) extends Projectile(g, l, d) {

  override val name: String = "bullet"
  var speed = 10
  var hitsLeft = durability
  val enemiesHit: Buffer[Attacker] = Buffer()
  var ticker = 0
  val hitbox = 40

  override def update(): Unit = {
    this.move(speed)
    if (!g.attackers.map(_.distance(this)).forall(_ > hitbox) && !enemiesHit.contains(game.attackers.minBy(_.distance(this)))) {
      var attackers = game.attackers.filter(_.distance(this) < hitbox) // making sure it doesnt hit the same attacker twice
      while (hitsLeft > 0 && attackers.nonEmpty) {
        attackers.headOption match {
          case Some(attacker) => {
            game.impact(this, attacker)
            enemiesHit += attacker
            attackers -= attacker
          }
          case None => attackers = Buffer()
        }
      }
    }
    if (hitsLeft <= 0) this.delete()
    ticker += 1
    if (ticker == 100) this.delete() // this deletes instances that go out of the map
  }

}

/**
 * Each attacker will move directly to their destination, which changes when they reach cornerpoints.
 * Their speed may also change as their state changes. For example attacker1 gets faster as they lose health
 */

abstract class Attacker(n: String, g: Game, l: (Double, Double), startingHealth: Int) extends MovableObject(g, l) {
  override val game: Game = g
  var health: Double = startingHealth
  val bounty: Int
  var healthPercentage = 100   // this is fur gui

  val pathCorners: Queue[(Double, Double)] = Queue(game.pathCorners: _*)
  override var destination = pathCorners.head

  var dx = ((destination._1 - this.location._1) / distance(destination)) // this should be at movable object but it caused some errors
  var dy = ((destination._2 - this.location._2) / distance(destination))
  def move(speed: Double) = location = (this.location._1 + dx * speed, this.location._2 + dy * speed)

  def takeDamage(damage: Double) = health -= damage

  def onDeath() = {
    this.delete()
    game.money += bounty
  }

  def attack(damageToPLayer: Int, speed: Double) = {
    if (health <= 0) onDeath()
    this.move(speed)
    dx = ((destination._1 - this.location._1) / distance(destination))
    dy = ((destination._2 - this.location._2) / distance(destination))
    if (this.distance(game.endTileLocation) < 10) { // if they reach the goal tile player loses health and the attacker is deleted
      this.delete()
      game.loseHealth(damageToPLayer)
      game.money += bounty / 2 // player gets some money even if attacker reaches the goal
    }
    if (this.pathCorners.size > 1 && this.distance(pathCorners.head) < 1 + speed) { // if an attacker reaches a path corner, their destination will be the next corner
      pathCorners.dequeue()
      this.destination = pathCorners.head
    }
  }
}


class Attacker1(g: Game, l: (Double, Double), val startingHealth: Int) extends Attacker("attacker1", g, l, startingHealth) {

  override val name: String = "attacker1"
  override val bounty = 10

  val damageToPlayer = 1
  var speed = 2

  override def update(): Unit = {
    healthPercentage = health.toInt*100/startingHealth
    this.attack(damageToPlayer,speed)
    this.speed = 2-healthPercentage.toDouble*1.5/100
  }

}

class Attacker2(g: Game, l: (Double, Double), val startingHealth: Int) extends Attacker("attacker2", g, l, startingHealth) {

  override val name: String = "attacker2"
  override val bounty = 30

  val damageToPlayer = 5
  var speed: Double = 1
  var ticker = 0.0

  override def update(): Unit = {
    healthPercentage = health.toInt * 100 / startingHealth
    this.attack(damageToPlayer, speed)
    speed = 1.5 + 0.7 * sin(ticker)
    ticker += 0.05
  }

}

class Attacker3(g: Game, l: (Double, Double), val startingHealth: Int,val contentSize:Int) extends Attacker("attacker2", g, l, startingHealth) {

  override val name: String = "boss"
  override val bounty = 150 // for some reason this attacker seems to yield the bounty twice

  val damageToPlayer = 100
  var speed: Double = 0.5

  override def onDeath() = {     // this class spawns attacker1s on death
    for (i <- 1 to contentSize) {
      game.createObject(new Attacker1(game, this.location, (40 * game.scalingMultiplier).toInt))
    }
    for (a <- game.attackers.filter(_.distance(this) < 10)) { // making sure spawned attackers dont go backwards
      while (a.pathCorners.size > this.pathCorners.size) a.pathCorners.dequeue()
      a.destination = a.pathCorners.head
    }
    this.delete()
    game.money += bounty
  }

  override def update(): Unit = {
    if (health <= 0) onDeath()
    healthPercentage = health.toInt * 100 / startingHealth
    this.attack(damageToPlayer, speed)
  }

}



