
import scalafx.animation.AnimationTimer
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
import javax.sound.sampled._
import javax.imageio.ImageIO

object GameApp extends JFXApp {

 // val attacker1Pic = ImageIO.read(new File("src/main/Pictures/kuva2.png"))

val attacker1Image = new Image(new FileInputStream("src/main/Pictures/kuva2.png"))
val upgradeIndicator = new Image(new FileInputStream("src/main/Pictures/upgradeIndicator.png") )
val spikeBallImage = new Image(new FileInputStream("src/main/Pictures/spikeBall.png") )


  var game = FileManager.loadGame(new FileReader("src/main/saveData/SaveSlot_1"))
  val gameWindowHeight = 1000
  val gameWindowWidth = 1000
  val tileSize = gameWindowHeight / game.gridSize
  val pathTileColor = new Color(200,120,50)

  var towerSelectedForBuy: Option[Tower] = None
  var towerSelected: Option[Tower] = None
  var gameSpeed = 1

  val upgrade1X = 1100
  val upgrade1Y = 330

  val upgrade2X = 1100
  val upgrade2Y = 420

   val root = new Scene

    stage = new JFXApp.PrimaryStage {
      title.value = "TD"
      width = 1400
      height = 1000
      scene = root
    }

   def drawThings() = {       // this is the main function that draws everything
     val contents = Buffer[Node]()
     val sideMenuBackGround = new Rectangle { // this is here so that upgradebuttons dont get lost behind it
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
           val tilePic = new Rectangle {
             width = tileSize
             height = tileSize
             x = p.x
             y = p.y
             fill = pathTileColor
           }
           contents += tilePic
         }
        case w: WaterTile => {
          val tilePic = new Rectangle {
            width = tileSize
            height = tileSize
            x = w.x
            y = w.y
            fill = new Color(100,100,250)
          }
          contents += tilePic
        }
      }
    }
     for (t <- game.towers) {
       t match {
         case tower1: Tower1 => {
           val towerPic = new Rectangle {
             width = tileSize
             height = tileSize
             x = t.x
             y = t.y
             fill = Yellow
           }
           towerPic.onMouseClicked = (event) => {
             tower1.isSelected = true
           }
           contents += towerPic
         }
         case tower2: Tower2 => {
           val towerPic = new Rectangle {
             width = tileSize
             height = tileSize
             x = t.x
             y = t.y
             fill = Red
           }
           towerPic.onMouseClicked = (event) => {
             tower2.isSelected = true
           }
           contents += towerPic
         }
         case tower3: Tower3 => {
           val towerPic = new Rectangle {
             width = tileSize
             height = tileSize
             x = t.x
             y = t.y
             fill = Cyan
           }
           towerPic.onMouseClicked = (event) => {
             tower3.isSelected = true
           }
           contents += towerPic
         }
         case tower4: Tower4 => {
           val towerPic = new Rectangle {
             width = tileSize
             height = tileSize
             x = t.x
             y = t.y
             fill = Pink
           }
           towerPic.onMouseClicked = (event) => {
             tower4.isSelected = true
           }
           contents += towerPic
         }
       }
       var increment = -15
       for(i <- 0 until 4-t.upgradePath2.size-t.upgradePath1.size) {   // draw upgrade indicators
         val upgrade = new ImageView(upgradeIndicator) {
           x = t.x-130
           y = t.y+30 + increment
           scaleX = 0.1
           scaleY = 0.1
           increment -= 15
         }
         contents += upgrade
       }
     }

     for (t <- game.towers) { // this is in a separate for loop so that rangecircles are always on top of towers
       if (t.isSelected) {
         val rangeCircle = new Circle {
           radius = t.range
           centerX = t.x + (tileSize / 2)
           centerY = t.y + (tileSize / 2)
           opacity = 0.5
           fill = DarkGrey
         }
         contents += rangeCircle
         val sellButton = new Button("Sell Tower  $ "+t.cost/2) {
           translateX = upgrade1X
           translateY = upgrade2Y+75
           scaleX = 2
           scaleY = 2
         }
         sellButton.onMousePressed = (event) => {
           game.sellTower(t)
         }
         contents += sellButton
       val towersLabel = new Label("UPGRADES") {  //draw tower upgrade buttons
       translateX = 1150
       translateY = 260
       textFill = Black
       scaleX = 5
       scaleY = 5
     }
     contents += towersLabel
         t.upgradePath1.headOption match {
           case Some(towerUpgrade: TowerUpgrade) => {
             val upgradeButton1 = new Button(s"${t.upgradePath1.head.name}"+"  $ "+s"${t.upgradePath1.head.cost} ") {
               translateX = upgrade1X
               translateY = upgrade1Y
               scaleX = 2
               scaleY = 2
             }
             contents += upgradeButton1
             upgradeButton1.onMousePressed = (event) => {
               towerUpgrade.useUpgrade()
             }
           }
          case None => {
             val message = new Label("Fully Upgraded!") {
               translateX = upgrade1X
               translateY = upgrade1Y
               scaleX = 2
               scaleY = 2
             }
             contents += message
           }
         }
          t.upgradePath2.headOption match {
           case Some(towerUpgrade: TowerUpgrade) => {
             val upgradeButton2 = new Button(s"${t.upgradePath2.head.name}"+"  $ "+s"${t.upgradePath2.head.cost} ") {
               translateX = upgrade2X
               translateY = upgrade2Y
               scaleX = 2
               scaleY = 2
             }
             contents += upgradeButton2
             upgradeButton2.onMousePressed = (event) => {
               towerUpgrade.useUpgrade()
             }
           }
           case None => {
             val message = new Label("Fully Upgraded!") {
               translateX = upgrade2X
               translateY = upgrade2Y
               scaleX = 2
               scaleY = 2
             }
             contents += message
           }
         }
       }
     }
     for (o <- game.attackers) {
      o match {
        case attacker1: Attacker1 => {
          val attackerPic = new ImageView(attacker1Image) {
            x = attacker1.x
            y = attacker1.y
          }
          contents += attackerPic
        }
        case attacker2: Attacker2 => {
          val attacker2Pic = new Rectangle {
            x = attacker2.x
            y = attacker2.y
            width = tileSize
            height = tileSize
            fill = Black
          }
          contents +=attacker2Pic
        }
        case attacker3: Attacker3 => {
           val attacker3Pic = new Rectangle {
            x = attacker3.x
            y = attacker3.y
            width = tileSize
            height = tileSize
            fill = new Color (0,0,200)
          }
          contents += attacker3Pic
        }
      }
        val healthBarRed = new Rectangle {
          fill = Red
          x = o.x+5
          y = o.y-20
          width = tileSize/1.2
          height = 10
        }
         contents += healthBarRed
        val healthBarGreen = new Rectangle {
          fill = Green
          x = o.x+5
          y = o.y-20
          width = o.healthPercentage*tileSize/120
          height = 10
        }
        contents += healthBarGreen

    }
    for (o <- game.projectiles) {
      o match {
        case b: Bullet => {
          val bulletPic = new Circle {
            centerX = b.x+(tileSize/2)
            centerY = b.y+(tileSize/2)
            radius = 10
            fill = Black
          }
          contents += bulletPic
        }
        case m: Missile => {
          val missilePic = new Circle {
            centerX = m.x+(tileSize/2)
            centerY = m.y+(tileSize/2)
            radius = 10
            fill = Red
          }
          contents += missilePic
        }

          case s : SpikeBall => {
          val spikeBallPic = new ImageView(spikeBallImage) {
            x = s.x+25
            y = s.y+25
          }
          contents += spikeBallPic
        }
      }
    }

    for (e <- game.explosions) {
      val explosionPic = new Circle {
        centerX = e.x+(tileSize/2)
        centerY = e.y+(tileSize/2)
        radius = e.radius
        fill = FireBrick
      }
      contents += explosionPic
    }


     val roundString = new Label("Round " + game.round) {
       translateX = 40
       translateY = 10
       textFill = Black
       scaleX = 2.5
       scaleY = 2.5
     }
     contents += roundString
     val moneyString = new Label("$ " + game.money) {
       translateX = 40
       translateY = 40
       textFill = Black
       scaleX = 2.5
       scaleY = 2.5
     }
     contents += moneyString
     val livesString = new Label("Lives: " + game.lives) {
       translateX = 40
       translateY = 70
       textFill = Black
       scaleX = 2.5
       scaleY = 2.5
     }
     contents += livesString

       val towersLabel = new Label("TOWERS") {
       translateX = 1160
       translateY = 50
       textFill = Black
       scaleX = 7
       scaleY = 7
     }
     contents += towersLabel

     if(game.isOver) {
     val gameOver = new Label("GAME OVER") {
       translateX = 500
       translateY = 400
       textFill = Black
       scaleX = 11
       scaleY = 11
     }
     contents += gameOver
     }
       val buyTower1 = new Button("Tower1  $200"){
       translateX = 1050
       translateY = 150
       scaleX = 1.5
       scaleY = 1.5
       }
     buyTower1.onMousePressed = (event) => {
      towerSelectedForBuy = Some(new Tower1(game,new GridPos(1,1) ))
     }
       val buyTower2 = new Button("Tower2  $500"){
       translateX = 1250
       translateY = 150
       scaleX = 1.5
       scaleY = 1.5
       }
     buyTower2.onMousePressed = (event) => {
      towerSelectedForBuy = Some(new Tower2(game,new GridPos(1,1)) )
     }
      val buyTower3 = new Button("Tower3  $300"){
       translateX = 1050
       translateY = 200
       scaleX = 1.5
       scaleY = 1.5

       }
     buyTower3.onMousePressed = (event) => {
      towerSelectedForBuy = Some(new Tower3(game,new GridPos(1,1) ))
     }
     val buyTower4 = new Button("Tower4  $350"){
       translateX = 1250
       translateY = 200
       scaleX = 1.5
       scaleY = 1.5
       }
     buyTower4.onMousePressed = (event) => {
      towerSelectedForBuy = Some(new Tower4(game,new GridPos(1,1) ))
     }
     val nextRoundButton = new Button("Next Round"){
       translateX = 1150
       translateY = 630
       scaleY = 4
       scaleX = 4
       resize(300,300)
     }
     nextRoundButton.onMousePressed = (event) => {
       if (game.currentRound().isDone) game.advanceRound()
     }

  contents += nextRoundButton
  contents += buyTower1
  contents += buyTower2
  contents += buyTower3
  contents += buyTower4

     val saveSlot1 = new Button("Save game to slot 1") {
       translateX = 1050
       translateY = 700
     }
     saveSlot1.onMousePressed = (event) => {
       FileManager.saveGame(1, game)
     }
     contents += saveSlot1

     val saveSlot2 = new Button("Save game to slot 2") {
       translateX = 1050
       translateY = 750
     }
     saveSlot2.onMousePressed = (event) => {
       FileManager.saveGame(2, game)
     }
     contents += saveSlot2

    val saveSlot3 = new Button("Save game to slot 3") {
      translateX = 1050
      translateY = 800
    }
    saveSlot3.onMousePressed = (event) => {
      FileManager.saveGame(3, game)
    }
    contents += saveSlot3

    val saveSlot4 = new Button("Save game to slot 4") {
      translateX = 1050
      translateY = 850
    }
    saveSlot4.onMousePressed = (event) => {
      FileManager.saveGame(4, game)
    }
    contents += saveSlot4

    val saveSlot5 = new Button("Save game to slot 5") {
      translateX = 1050
      translateY = 900
    }
    saveSlot5.onMousePressed = (event) => {
      FileManager.saveGame(5, game)
    }
    contents += saveSlot5

    val loadSlot1 = new Button("Load game from slot 1") {
      translateX = 1200
      translateY = 700
    }
    loadSlot1.onMousePressed = (event) => {
      game = FileManager.loadGame(new FileReader("src/main/saveData/SaveSlot_1"))
    }
    contents += loadSlot1

    val loadSlot2 = new Button("Load game from slot 2") {
      translateX = 1200
      translateY = 750
    }
    loadSlot2.onMousePressed = (event) => {
      game = FileManager.loadGame(new FileReader("src/main/saveData/SaveSlot_2"))
    }
    contents += loadSlot2

    val loadSlot3 = new Button("Load game from slot 3") {
      translateX = 1200
      translateY = 800
    }
    loadSlot3.onMousePressed = (event) => {
      game = FileManager.loadGame(new FileReader("src/main/saveData/SaveSlot_3"))
    }
    contents += loadSlot3

    val loadSlot4 = new Button("Load game from slot 4") {
      translateX = 1200
      translateY = 850
    }
    loadSlot4.onMousePressed = (event) => {
      game = FileManager.loadGame(new FileReader("src/main/saveData/SaveSlot_4"))
    }
    contents += loadSlot4

    val loadSlot5 = new Button("Load game from slot 5") {
      translateX = 1200
      translateY = 900
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
       game.towers.filter(_.gridPos == tilePos).foreach(_.isSelected = true) // select tower when clicked
       game.towers.filter(_.gridPos != tilePos).foreach(_.isSelected = false) //deselect other towers

       if (towerSelectedForBuy.nonEmpty) {
         towerSelectedForBuy match {
           case Some(tower1: Tower1) => {
             game.buyTower(new Tower1(game, new GridPos(1,1)), tilePos)
             towerSelectedForBuy = None
           }
           case Some(tower2: Tower2) => {
             game.buyTower(new Tower2(game, new GridPos(1,1)), tilePos)
             towerSelectedForBuy = None
           }
             case Some(tower3: Tower3) => {
             game.buyTower(new Tower3(game, new GridPos(1,1)), tilePos)
             towerSelectedForBuy = None
           }
           case Some(tower4: Tower4) => {
             game.buyTower(new Tower4(game, new GridPos(1,1)), tilePos)
             towerSelectedForBuy = None
           }
           case None =>
         }
       }
     }
     contents += selectionHelper


     root.onKeyPressed = (event) => { // deselect any selections
       towerSelectedForBuy = None
       game.towers.foreach(_.isSelected = false)
     }
     root.content = contents
     root.setFill(LightGreen)
   }

  val ticker = new Ticker( () => {
    game.update()
    drawThings()
  }, game.speed
  )

  ticker.start()
}

