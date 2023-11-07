import javafx.animation.AnimationTimer

class Ticker(function: () => Unit, var speed:Int) extends AnimationTimer {
    private var lastUpdate: Long = 0
    override def handle(now: Long): Unit = {
        if (now - lastUpdate >= 10_000_000 / speed) {
            function()
            lastUpdate = now
        }
    }
}