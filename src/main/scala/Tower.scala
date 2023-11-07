import scala.math._
import scala.collection.mutable.Buffer

abstract class Tower(g: Game, gridPos: GridPos) extends TileObject(g, gridPos) {
  override val game: Game = g
  var target: Option[Attacker] = None
  var range: Int
  var fireRate: Int
  var damage: Int
  var ticker = fireRate
  var cost: Int
  val upgradePath1: Buffer[TowerUpgrade] = Buffer()
  val upgradePath2: Buffer[TowerUpgrade] = Buffer()

  def upgradeRange(howMuch: Int): Unit = this.range += howMuch
  def upgradeFireRate(howMuch: Int): Unit = this.fireRate -= (this.fireRate / 10) * howMuch // how many tenths of time removed from between shots. for example 5 means twice as fast shooting
  def upgradeDamage(howMuch: Int): Unit = this.damage += howMuch
//  var projectile : Projectile  // not sure how to add projectileType here. It would be better since all of the towers will have a projectile
}

class Tower1(g: Game, gridPos: GridPos) extends Tower(g, gridPos) {

  override val name = "tower1"
  var cost = 200
  var range = 350
  var fireRate = 100
  var damage = 9

  val rangeUpgrade1 = new TowerUpgrade(this,"range ",1,200, () => upgradeRange(150) )
  val damageUpgrade = new TowerUpgrade(this,"Damage",1,300, () => upgradeDamage(this.damage) )

  val fireRateUpgrade = new TowerUpgrade(this,"FireRate",2,300, () => upgradeFireRate(3) )
  val superSpeed = new TowerUpgrade(this,"Fire Rate 2",2,2000, () => upgradeFireRate(5) )

  private def shoot() = {
    target match {
      case None => ()
      case Some(target: Attacker) =>
        this.game.createObject(new Bullet(g, damage, (this.x, this.y), target.location))
    }
  }

  override def update(): Unit = {
    val possibleTargets = game.attackers.filter(_.distance(this) <= range)
    if (possibleTargets.nonEmpty) {
      val minCornerCount = possibleTargets.minBy(_.pathCorners.size).pathCorners.size
      target = Some(possibleTargets.groupBy(_.pathCorners.size)(minCornerCount).minBy(a => a.distance(a.pathCorners.head))) // target is the attacker that is the furthest and also in range
    } else target = None
    if (ticker >= fireRate) { // the greater than fixes a bug with upgrading the fireRate
      this.shoot()
      ticker = 0
    }
    ticker += 1
  }
}

class Tower2(g: Game, gridPos: GridPos) extends Tower(g, gridPos) {
  override val name: String = "tower2"
  var range = 700
  var fireRate = 200
  var damage = 3
  var missileBaseSpeed = 0.5
  var missileRadius = 20
  var cost = 500

  val rangeUpgrade = new TowerUpgrade(this,"Range",1,400, () => upgradeRange(200 ) )
  val missileUpgrade1 = new TowerUpgrade(this,"Missile Radius",1,400, () => missileRadius += 40 )

  val damageUpgrade = new TowerUpgrade(this,"Damage",2,400, () => upgradeDamage(1) )
  val fireRateUpgrade = new TowerUpgrade(this,"Fire Rate",2,500, () => upgradeFireRate(3) )

  private def shoot() = {
    target match {
      case None => ()
      case Some(target: Attacker) =>
        this.game.createObject(new Missile(g, this.location, target, damage, missileBaseSpeed, missileRadius))
    }
  }

  override def update(): Unit = {
    if (game.attackers.nonEmpty && game.attackers.exists(_.distance(this) < range)) {
      target = Some(game.attackers.filter(_.distance(this) < range).maxBy(_.distance(this)))
    } else target = None
    if (ticker >= fireRate) {
      this.shoot()
      ticker = 0
    }
    ticker += 1
  }
}

class Tower3(g: Game, gridPos: GridPos) extends Tower(g, gridPos) {
  override val name: String = "tower3"
  var range = 225
  var fireRate = 200
  var damage = 15
  var cost = 300
  var shotAmount = 10
  var projectile = 1


  val rangeUpgrade = new TowerUpgrade(this,"Range",1,250, () => upgradeRange(100) )
  val shotIncrease = new TowerUpgrade(this,"Shot Increase",1,600, () => shotAmount += 7 )

  val fireRateUpgrade = new TowerUpgrade(this, "Fire Rate", 2, 500, () => upgradeFireRate(4))
  val spikeBallProjectile = new TowerUpgrade(this, "Spike Balls", 2, 1000, () => {
    projectile = 2
    fireRate += 100
  })

  private def shoot() = {
    if (game.attackers.isEmpty) target = None else {
      target match {
        case None => ()
        case Some(target: Attacker) =>
          for (s <- 0 until shotAmount) {
            if(projectile==1) game.createObject(new Bullet(g, damage, this.location, (this.location._1 + sin(2 * Pi * s / shotAmount)*range , this.location._2 + cos(2 * Pi * s / shotAmount) *range )))
            if(projectile==2) game.createObject(new SpikeBall(g, damage,2, this.location, (this.location._1 + sin(2 * Pi * s / shotAmount) * range, this.location._2 + cos(2 * Pi * s / shotAmount) * range)))
          }
      }
    }
  }

  override def update(): Unit = {
    if (game.attackers.nonEmpty && game.attackers.map(_.distance(this)).min <= range) {
      target = Some(game.attackers.dropWhile(_.distance(this)>range).head )
      if (ticker >= fireRate) {
        this.shoot()
        ticker = 0
      }
      ticker += 1
    }
  }
}

class Tower4(g: Game, gridPos: GridPos) extends Tower(g, gridPos) {

  override val name = "tower4"
  var cost = 350
  var range = 400
  var fireRate = 150
  var damage = 10
  var spikeBallDurability = 3

  val rangeUpgrade = new TowerUpgrade(this,"Range",1,200, () => upgradeRange(100) )
  val durabilityUpgrade = new TowerUpgrade(this,"Better Balls",1,450, ()=> spikeBallDurability += 5 )

  val fireRateUpgrade = new TowerUpgrade(this,"Fire Rate",2,500, () => upgradeFireRate(4))
  val danageUpgrade = new TowerUpgrade(this,"Damage 1",2,500, () => upgradeDamage(10) )
  val danageUpgrade2 = new TowerUpgrade(this,"Damage 2",2,5000, () => upgradeDamage(50) )

  private def shoot() = {
    target match {
      case None => ()
      case Some(target: Attacker) =>
        this.game.createObject(new SpikeBall(g, damage, spikeBallDurability, (this.x, this.y), target.location))
    }
  }

  override def update(): Unit = {
    val possibleTargets = game.attackers.filter(_.distance(this) <= range)
    if (possibleTargets.nonEmpty) {
      val minCornerCount = possibleTargets.minBy(_.pathCorners.size).pathCorners.size
      target = Some(possibleTargets.groupBy(_.pathCorners.size)(minCornerCount).minBy(a => a.distance(a.pathCorners.head))) // target is the attacker that is the furthest and also in range
    } else target = None
    if (ticker >= fireRate) {
      this.shoot()
      ticker = 0
    }
    ticker += 1
  }
}