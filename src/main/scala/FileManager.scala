import java.io.{BufferedReader, FileNotFoundException, FileWriter, IOException, Reader}
import scala.collection.mutable.Buffer




object FileManager {

  def removeSpaces(s: String): String = s.filter(_ != ' ')

  def loadGame(input: Reader): Game = {

    val lineReader = new BufferedReader(input)

    try {
      var currentLine = lineReader.readLine().trim.toLowerCase

      if (!((currentLine startsWith "#td") && (currentLine endsWith "save file"))) {
        throw new Exception("Unknown file type")
      }

      var stateData = Buffer[String]()
      var tileData = Buffer[String]()
      var lineHeader = " "
      while (currentLine != null) {
        currentLine = lineReader.readLine()
        if (currentLine != null) currentLine = currentLine.trim.toLowerCase
        if (currentLine != null && currentLine.nonEmpty) {
          if (currentLine.head == '#') lineHeader = currentLine
          lineHeader match {
            case "#state" => stateData += currentLine
            case "#tiles" => tileData += currentLine
            case _ => throw new Exception("Corrupted save file")
          }
        }
      }
      stateData = stateData.drop(1).map(removeSpaces(_))  //headers dropped here
      tileData = tileData.drop(1).map(removeSpaces(_))

      var gameGridSize = 0
      var round = 0
      var lives = 100
      var money = 300
      var speed = 1

      for (s <- stateData) {
        val endPart = s.dropWhile(_ != ':').drop(1)
        s.takeWhile(_ != ':') match {
          case "gridsize" => gameGridSize = endPart.toInt
          case "speed" => speed = endPart.toInt
          case "round" => round = endPart.toInt
          case "lives" => lives = endPart.toInt
          case "money" => money = endPart.toInt
          case _ => new Exception("Unknown File Format")
        }
      }
     if (gameGridSize<=3) throw new Exception("game size must be at least 3")

     val game = new Game(gameGridSize,speed)
     game.setRound(round)
     game.setMoney(money)
     game.setLives(lives)   // game state should be set up here
      /**
       * the tile format is written here as x,y:tileType
       */

      for (t <- tileData) {
        val a = t.takeWhile(_ != ',').toInt
        val b = t.dropWhile(_ != ',').drop(1).takeWhile(_ != ':').toInt
        val tileType = t.dropWhile(_ != ':').drop(1)
        tileType match {
          case "path" => game.createObject(new PathTile(game, new GridPos(a,b)))
          case "pathend" =>{
            val endTile = new PathTile(game, new GridPos(a,b))
            endTile.isEnd = true
            game.createObject(endTile) }
          case "pathstart" =>{
            val startTile = new PathTile(game, new GridPos(a,b))
            startTile.isStart = true
            game.createObject(startTile) }
          case "tower1" => game.createObject(new Tower1(game, new GridPos(a,b)))
          case "tower2" => game.createObject(new Tower2(game,new GridPos(a,b)))
          case "tower3" => game.createObject(new Tower3(game, new GridPos(a,b)))
          case "tower4" => game.createObject(new Tower4(game, new GridPos(a,b)))
          case "water" => game.createObject(new WaterTile(game, new GridPos(a,b)))
          case _ => throw new Exception("Unkwown tile type")
        }
      }

    game

    } catch {
      case e: IOException =>
      val TDExc = new Exception("Reading save data failed")  // not sure about this part
        TDExc.initCause(e)
        throw TDExc
    }


  }

  def saveGame(slot: Int, game: Game): Unit = {
    val lines = Buffer[String]("#TD save file")
    lines += "#STATE"
    lines += s"round : ${game.round}"
    lines += s"gridsize : ${game.gridSize}"
    lines += s"lives : ${game.lives}"
    lines += s"money : ${game.money}"

    lines += "#TILES"
    for (t <- game.tiles) {
      t match {
        case p: PathTile => {
          if (p.isEnd) lines += s" ${t.gridPos.a + "," + t.gridPos.b} : ${t.name}End"
          else if (p.isStart) lines += s" ${t.gridPos.a + "," + t.gridPos.b} : ${t.name}Start"
          else lines += s" ${t.gridPos.a + "," + t.gridPos.b} : ${t.name}"
        }
        case otherTile: TileObject => lines += s" ${t.gridPos.a + "," + t.gridPos.b} : ${t.name}"
      }

    }

    for (t <- game.towers) {
      lines += s" ${t.gridPos.a + "," + t.gridPos.b} : ${t.name}"
    }
    writeToSlot(slot, lines.toSeq)
  }

  def writeToSlot(saveslot: Int, arr: Seq[String]) = {
   val a = new FileWriter(s"src/main/saveData/SaveSlot_$saveslot")
    try { for(b<-arr) {
     a.write( b+"\n" )
    } } catch {
      case notFoundException: FileNotFoundException => println("File not found")
      case readingException:IOException => println("File not readable :DD")
    }
    a.close()
  }

}
