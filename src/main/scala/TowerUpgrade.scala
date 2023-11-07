

class TowerUpgrade(tower: Tower, val name: String, path: Int, val cost: Int, upgradeFunction: () => Unit) {

  if (path == 1) tower.upgradePath1 += this
  if (path == 2) tower.upgradePath2 += this

  def useUpgrade(): Unit = {
    if (tower.game.money >= cost) {
      tower.game.spendMoney(cost)
      upgradeFunction()
      tower.cost += this.cost
      if (path == 1) tower.upgradePath1 -= this
      if (path == 2) tower.upgradePath2 -= this
    }
  }
}
