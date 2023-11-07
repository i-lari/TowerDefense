

abstract class TileObject(g:Game,val gridPos: GridPos) extends GameObject(g,(gridPos.a*100,gridPos.b*100)) { //pos needs to be changed if tilegrid is other than 10
  require(gridPos.a >= 0 && gridPos.b >= 0, "tile position needs to be positive")
  var canBeBuiltOn = false
  var isSelected = false // this is for the level editor
}

class PathTile(g: Game, gridPos: GridPos) extends TileObject(g,gridPos) {
  override val name: String = "Path"
  override val game: Game = g
  var isStart = false
  var isEnd = false
  var isCorner = false

  def update() = ()
}

class WaterTile(g: Game, gridPos: GridPos) extends TileObject(g,gridPos) {
  override val name: String = "Water"
  override val game: Game = g

  def update() = ()
}