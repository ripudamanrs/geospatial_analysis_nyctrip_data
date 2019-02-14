package cse512

object HotzoneUtils {

    def ST_Contains(queryRectangle: String, pointString: String): Boolean = {
        val rect = queryRectangle.split(",").map(x => x.toDouble)
        val rectX1 = rect(0)
        val rectY1 = rect(1)
        val rectX2 = rect(2)
        val rectY2 = rect(3)

        val point = pointString.split(",").map(x => x.toDouble)
        val pointX = point(0)
        val pointY = point(1)

        var minRectX: Double = 0
        var minRectY: Double = 0
        var maxRectX: Double = 0
        var maxRectY: Double = 0

        if (rectX1 < rectX2) {
            minRectX = rectX1
            maxRectX = rectX2
        } else {
            minRectX = rectX2
            maxRectX = rectX1
        }
        if (rectY1 < rectY2) {
            minRectY = rectY1
            maxRectY = rectY2
        } else {
            minRectY = rectY2
            maxRectY = rectY1
        }

        if (minRectX <= pointX & pointX <= maxRectX & minRectY <= pointY & pointY <= maxRectY) return true

        return false
    }

  // YOU NEED TO CHANGE THIS PART

}
