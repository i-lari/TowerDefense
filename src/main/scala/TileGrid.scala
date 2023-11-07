import GameApp.game

import scala.collection.mutable.Buffer

case class GridPos(val a: Int, val b: Int) {
  def posToDir(direction: Int): GridPos = {
    direction match {
      case 1 => new GridPos(a, b - 1) // 1 is north 2 is east etc
      case 2 => new GridPos(a + 1, b)
      case 3 => new GridPos(a, b + 1)
      case 4 => new GridPos(a - 1, b)
      case _ => this
    }
  }

  def neighbors: Vector[GridPos] = {
    var vector = Vector[GridPos]()
    for(a <- 1 to 4) {
      vector = vector :+ posToDir(a)
    }
    vector
  }
}


class PathFinder(game: Game) {
  def findPathCorners(): Vector[(Double, Double)] = {
    val path = game.tiles.filter(a => a.isInstanceOf[PathTile]).asInstanceOf[Buffer[PathTile]]
    var currentTile: PathTile = game.startTile
    var previousTile: PathTile = game.startTile
    var currentDir: Int = 1
    var pathCorners: Buffer[(Double, Double)] = Buffer()
    var x = 1
    do {
      if (path.exists(a => a.gridPos != previousTile.gridPos && a.gridPos == currentTile.gridPos.posToDir(currentDir))) { //check if path continues in the current direction but not if the path came from there
        previousTile = currentTile
        currentTile = path.filter(_.gridPos == currentTile.gridPos.posToDir(currentDir)).head
        x = 1
      } else { // if not check another direction and mark that as pathcorner
        pathCorners += currentTile.location
        currentDir += 1
        x += 1
      }
      if (currentDir == 5) currentDir = 1
      if (x == 5) throw new Exception("Invalid Path placement")
      //pathCorners += currentTile.location
    } while (currentTile.gridPos != game.endTile.gridPos)
    pathCorners = pathCorners.distinct.drop(1) // attackers trying to move to the position they are spawned in is very buggy
    pathCorners += game.endTileLocation // also algorithm above creates duplicate corners so distinct is needed
    pathCorners.toVector
  }
}
