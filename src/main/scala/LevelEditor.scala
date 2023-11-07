
import scalafx.application.JFXApp
import scalafx.scene._
import scalafx.scene.shape._
import scalafx.scene.paint.Color._
import scalafx.scene.Node
import scalafx.scene.control._
import scalafx._
import scalafx.scene.input._

import scala.collection.mutable.Buffer
import scalafx.Includes._
import scalafx.scene.image._
import scalafx.scene.paint.Color
import scalafx.scene.text.Font

import java.io._



object LevelEditor extends JFXApp {

  var game = new Game(10,1)
  val gameWindowHeight = 1000
  val gameWindowWidth = 1000
  val tileSize = gameWindowHeight / game.gridSize

  var tileSelectedToPlace: Option[TileObject] = None
  var pathTileSelected: Option[PathTile] = None
  val root = new Scene
  val pathTileColor = new Color(200,120,50)   // brown

  stage = new JFXApp.PrimaryStage {
    title.value = "Level Editor"
    width = 1400
    height = 1000
    scene = root
  }

  def drawThings() = {
    val contents = Buffer[Node]()
    val sideMenuBackGround = new Rectangle {
      x = 1000
      y = 0
      width = 400
      height = 1000
      fill = Grey
    }
    contents += sideMenuBackGround

    for (t <- game.tiles) {
      t match {
        case p: PathTile => {
          var color = pathTileColor
          if (p.isStart) color = Green
          if (p.isEnd) color = Red
          val tilePic = new Rectangle {
            width = tileSize
            height = tileSize
            x = p.x
            y = p.y
            fill = color
          }
          contents += tilePic

          if (p.isSelected) {
            val makeStart = new Button("Make this tile the starting point for attackers.") {
              translateX = 1050
              translateY = 330
            }
            val makeEnd = new Button("Make this tile the goal point for attackers.") {
              translateX = 1050
              translateY = 380
            }
            val clear = new Button("Clear start and end properties.") {
              translateX = 1050
              translateY = 430
            }
            clear.onMousePressed = (event) => {
              p.isEnd = false
              p.isStart = false
            }
            makeEnd.onMousePressed = (event) => {
              p.isEnd = true
            }
            makeStart.onMousePressed = (event) => {
              p.isStart = true
            }
            contents += clear
            contents += makeEnd
            contents += makeStart
          }
        }
        case w: WaterTile => {
          val tilePic = new Rectangle {
            width = tileSize
            height = tileSize
            x = w.x
            y = w.y
            fill = LightBlue
          }
          contents += tilePic
        }
      }
      if (t.isSelected) {
        val selectedIndicator = new Rectangle {
          x = t.x
          y = t.y
          width = tileSize
          height = tileSize
          fill = Black
          opacity = 0.5
        }
        contents += selectedIndicator
        val tileOptionsLabel = new Label("TILE OPTIONS") {
          translateX = 1150
          translateY = 280
          scaleX = 4
          scaleY = 4
        }
        contents += tileOptionsLabel
        val removeTile = new Button("Remove Tile") {
          translateX = 1050
          translateY = 480
        }
        removeTile.onMousePressed = (event) => {
          t.delete()
        }
        contents += removeTile
      }
    }
    val tilesLabel = new Label("TILES") {
      translateX = 1160
      translateY = 50
      textFill = Black
      scaleX = 7
      scaleY = 7
    }
    contents += tilesLabel

    val pathTilePic = new Rectangle {
      width = tileSize
      height = tileSize
      x = 1050
      y = 100
      fill = pathTileColor
    }
    contents += pathTilePic

    val pathTile = new Button("Path Tile") {
      translateX = 1070
      translateY = 210
    }

    val waterTilePic = new Rectangle {
      width = tileSize
      height = tileSize
      x = 1200
      y = 100
      fill = new Color(100, 100, 250)
    }
    contents += waterTilePic

    pathTile.onMousePressed = (event) => {
      val tile = new PathTile(game, (GridPos(1, 1)))
      tileSelectedToPlace = Some(tile)
    }
    val waterTile = new Button("Water Tile") {
      translateX = 1220
      translateY = 210
    }
    waterTile.onMousePressed = (event) => {
      val tile = new WaterTile(game, (GridPos(1, 1)))
      tileSelectedToPlace = Some(tile)
    }
    contents += waterTile
    contents += pathTile

    val saveSlot1 = new Button("Save level to slot 1") {
      translateX = 1050
      translateY = 650
    }
    saveSlot1.onMousePressed = (event) => {
      FileManager.saveGame(1, game)
    }
    contents += saveSlot1

    val saveSlot2 = new Button("Save level to slot 2") {
      translateX = 1050
      translateY = 700
    }
    saveSlot2.onMousePressed = (event) => {
      FileManager.saveGame(2, game)
    }
    contents += saveSlot2

    val saveSlot3 = new Button("Save level to slot 3") {
      translateX = 1050
      translateY = 750
    }
    saveSlot3.onMousePressed = (event) => {
      FileManager.saveGame(3, game)
    }
    contents += saveSlot3

    val saveSlot4 = new Button("Save level to slot 4") {
      translateX = 1050
      translateY = 800
    }
    saveSlot4.onMousePressed = (event) => {
      FileManager.saveGame(4, game)
    }
    contents += saveSlot4

    val saveSlot5 = new Button("Save level to slot 5") {
      translateX = 1050
      translateY = 850
    }
    saveSlot5.onMousePressed = (event) => {
      FileManager.saveGame(5, game)
    }
    contents += saveSlot5

    val loadSlot1 = new Button("Load level from slot 1") {
      translateX = 1200
      translateY = 650
    }
    loadSlot1.onMousePressed = (event) => {
      game = FileManager.loadGame(new FileReader("src/main/saveData/SaveSlot_1"))
    }
    contents += loadSlot1

    val loadSlot2 = new Button("Load level from slot 2") {
      translateX = 1200
      translateY = 700
    }
    loadSlot2.onMousePressed = (event) => {
      game = FileManager.loadGame(new FileReader("src/main/saveData/SaveSlot_2"))
    }
    contents += loadSlot2

    val loadSlot3 = new Button("Load level from slot 3") {
      translateX = 1200
      translateY = 750
    }
    loadSlot3.onMousePressed = (event) => {
      game = FileManager.loadGame(new FileReader("src/main/saveData/SaveSlot_3"))
    }
    contents += loadSlot3

    val loadSlot4 = new Button("Load level from slot 4") {
      translateX = 1200
      translateY = 800
    }
    loadSlot4.onMousePressed = (event) => {
      game = FileManager.loadGame(new FileReader("src/main/saveData/SaveSlot_4"))
    }
    contents += loadSlot4

    val loadSlot5 = new Button("Load level from slot 5") {
      translateX = 1200
      translateY = 850
    }
    loadSlot5.onMousePressed = (event) => {
      game = FileManager.loadGame(new FileReader("src/main/saveData/SaveSlot_5"))
    }
    contents += loadSlot5

    val selectionHelper = new Rectangle {
      width = 1000
      height = 1000
      opacity = 0
    }

    selectionHelper.onMousePressed = (event) => {
      val tilePos = (game.coordsToGridPos(event.getX.toInt, event.getY.toInt))
      game.tiles.filter(_.gridPos == tilePos).foreach(_.isSelected = true) // select tile when clicked
      game.tiles.filter(_.gridPos != tilePos).foreach(_.isSelected = false) //deselect other tiles

      if (tileSelectedToPlace.nonEmpty) {
        tileSelectedToPlace match {
          case Some(pathTile: PathTile) => {
            if (!game.tiles.exists(tilePos == _.gridPos)) {
              game.createObject(new PathTile(game, tilePos))
            }
          }
          case Some(waterTile: WaterTile) => {
            if (!game.tiles.exists(tilePos == _.gridPos)) {
              game.createObject(new WaterTile(game, tilePos))
            }
          }
          case None =>
        }
      }
    }


    contents += selectionHelper
    root.onKeyPressed = (event) => {
      tileSelectedToPlace = None
    }

    root.content = contents
    root.setFill(LightGreen)

  }

  val ticker = new Ticker(() => {
    drawThings()
    game.updateTilesOnly()
  }, 1
  )
  ticker.start()

}
