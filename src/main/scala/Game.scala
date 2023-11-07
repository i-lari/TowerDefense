import scala.collection.mutable.Buffer
import scala.math._




/**
 * @param gridSize for now only 10 is supported
 * @param speed game speed which can be configured in a save file. Default is 1
 */

class Game(val gridSize: Int, val speed : Int) {   //game is created with gridSize and speed. For now only gridsize 10 is supported.

 val towers: Buffer[Tower] = Buffer()
 val attackers: Buffer[Attacker] = Buffer()
 val projectiles: Buffer[Projectile] = Buffer()
 val tiles :Buffer[TileObject] = Buffer()
 val explosions :Buffer[Explosion] = Buffer()
 val gameObjects = Array(towers, attackers, projectiles,tiles ,explosions)
 val rounds:Buffer[Round] = Buffer(new Round(this,Vector[(Int,String)](  )))

 rounds ++= new RoundGenerator(this).generateRounds(100)

 var round = 0
 var lives = 100
 var money = 300
 def scalingMultiplier : Double = (1+ (round-1)/10 )*pow(1.025,round)// should be about 2 at round 10 and scale deeper later // pow(1.07177,round)

 lazy val startTile :PathTile = tiles.filter(a =>a.isInstanceOf[PathTile]).asInstanceOf[Buffer[PathTile]].filter(_.isStart).head // this could be changed to support multiple starts or endings
 lazy val endTile :PathTile = tiles.filter(a =>a.isInstanceOf[PathTile]).asInstanceOf[Buffer[PathTile]].filter(_.isEnd).head

 lazy val startTileLocation = startTile.location
 lazy val endTileLocation = endTile.location

 lazy val pathCorners: Vector[(Double, Double)] = (new PathFinder(this)).findPathCorners()  // all of these are lazy so that this class can be initialized correctly

 def setRound(i: Int) = { // this is only for fileManager
  round = i
  rounds.drop(i)
 }
 def setLives(i: Int) = lives = i
 def setMoney(i: Int) = money = i

 def currentRound() = rounds.head
 def advanceRound() = {
  if(round != 0) money += (100*scalingMultiplier).toInt  // no money from starting the first round
  round += 1
  rounds -= rounds.head
 }

 def isOver = lives <= 0

 def loseHealth(amount: Int) = lives -= amount

 def spendMoney(amount: Int) = {
   money = money - amount
 }

 def createObject(gameObject: GameObject) = {
  gameObject match {
   case tower: Tower => towers += tower
   case attacker: Attacker => attackers += attacker
   case projectile: Projectile => projectiles += projectile
   case tile: TileObject => tiles += tile                  // it could be changed that towers go here also
   case explosion: Explosion => explosions += explosion
   case _ => throw new Exception("unknown gameObject type")
  }
 }

 def buyTower(tower: Tower, Pos: GridPos) = { // it is not optimal that a "phantom tower" is created when using this method but i prefer it over string etc. There is probably i better way.
  if (tiles.filter(_.gridPos == Pos).forall(_.canBeBuiltOn) && towers.filter(_.gridPos == Pos).forall(_.canBeBuiltOn) && tower.cost <= money) {
   tower match {
    case tower1: Tower1 => {
     spendMoney(tower1.cost)
     createObject(new Tower1(this, new GridPos(Pos.a, Pos.b)))
    }
    case tower2: Tower2 => {
     spendMoney(tower2.cost)
     createObject(new Tower2(this, new GridPos(Pos.a, Pos.b)))
    }
    case tower3: Tower3 => {
     spendMoney(tower3.cost)
     createObject(new Tower3(this, new GridPos(Pos.a, Pos.b)))
    }
    case tower4: Tower4 => {
     spendMoney(tower4.cost)
     createObject(new Tower4(this, new GridPos(Pos.a, Pos.b)))
    }
   }
  }
 }

 def sellTower(tower: Tower) = {
  money += tower.cost/2
  tower.delete()
 }

 def impact(projectile: Projectile, attacker: Attacker) = { // projectile damages an attacker
  projectile match {
   case b: Bullet => {
    attacker.takeDamage(b.damage)
    b.delete()
   }
   case s: SpikeBall => {
    attacker.takeDamage(s.damage)
    s.hitsLeft -=1
   }
   case _ =>
  }
 }

 def checkCollisions() = {
  for (a <- attackers) {
   for (e <- explosions) {
    if (a.distance(e) < e.radius) {
     a.takeDamage(e.damage)
    }
   }
  }
 }

 def update() = {
  if(!isOver) {
  gameObjects.flatten.foreach(_.update())
  rounds.head.spawnAttackers()
  checkCollisions()
  }
 }

 def updateTilesOnly() = {  //  for LeveEditor
  tiles.foreach(_.update())
 }

 def coordsToGridPos(x:Int,y:Int):GridPos = {
  val tileSize = 1000/gridSize
  new GridPos(x/tileSize,y/tileSize)
 }

}