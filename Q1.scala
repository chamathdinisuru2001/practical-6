object Inventories{
    def main(args: Array[String]): Unit ={
        val inventory1: Map[Int,(String,Int,Double)] = Map(
            101 -> ("ProductA",50,10.0),
            102 -> ("ProductB",30,20.0),
            103 -> ("ProductC",20,30.0)
        )

        val inventory2: Map[Int,(String,Int,Double)] = Map(
            101 -> ("ProductA",40,12.0),
            102 -> ("ProductB",25,18.0),
            103 -> ("ProductC",15,25.0)
        )

        val productNames = inventory1.values.map(_._1).toSet
        println("Product names:"+productNames.mkString(","))

        val totalValue = inventory1.values.map{case (_,quantity,price) => quantity*price}.sum
        println("Total value of inventory1: " + totalValue)

        val isEmpty = inventory1.isEmpty
        println("Is Inventory1 is empty?: " + isEmpty)

        val mergedInventory = inventory2.foldLeft(inventory1) {
            case (acc,(id,(name,quantity,price))) => acc.get(id)match{
                case Some((_, existingQuantity, existingPrice)) => 
                    acc.updated(id,(name, existingQuantity + quantity, Math.max(price, existingPrice)))
                case None => acc + (id -> (name,quantity, price))
            }
        }

        val mergedInventoryString = mergedInventory.map {
            case (id,(name,quantity,price)) => s"$id -> ($name, $quantity, $price)"
        }.mkString(", ")
        println("Merged inventory:" + mergedInventoryString)

        val checkingId = 102
        inventory1.get(checkingId)match{
            case Some((name,quantity,price)) => println(s"Product with id $checkingId exists, name: $name, quantity: $quantity, price: $price")
            case None => println(s"Product with id $checkingId does not exist")
        }
    }
}