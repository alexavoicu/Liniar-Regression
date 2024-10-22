type Mat = List[List[Double]]

class Matrix(m: Option[List[List[Double]]]) {

  def transpose: Matrix = data match
    case Some(x) =>
      def transposeHelper(m: Mat): Mat = {
        m match {
          case Nil :: _ => Nil
          case _ => m.map(_.head) :: transposeHelper(m.map(_.tail))
        }
      }
      Matrix(Option(transposeHelper(x)))

    case None => Matrix(None)

  def map(f: Double => Double): Matrix = {
    data match
      case Some(value) => Matrix(value.map(x => x.map(y => f(y))))
      case None => Matrix(None)
  }
  def *(other: Matrix): Matrix = {
    other.data match
      case Some(x) => data match
        case Some(value) => if (other.height == this.width) {
          val columns = other.transpose.data.get
          Matrix(value.map(line => columns.map(col => line.zip(col)
            .map(p => p._1.*(p._2)).foldLeft(0.0)(_+_))))
        } else Matrix(None)
        case None => Matrix(None)
      case None => Matrix(None)
  }
  def ++(x: Double): Matrix = {
    data match
      case Some(value) => Matrix(value.map(y => y :+ x))
      case None => Matrix(None)
  }
  def -(other: Matrix): Matrix = {
    other.data match
      case Some(value) => data match
        case Some(value) => if (other.height == this.height && other.width == this.width) {
          Matrix(value.zip(other.data.get)
            .map((x, y) => x.zip(y).map(z => z._1.-(z._2))))
        } else Matrix(None)
        case None => Matrix(None)
      case None => Matrix(None)
  }

  def data: Option[Mat] = m
  def height: Option[Int] = Option(data.get.size)
  def width: Option[Int] = Option(data.get.head.size)
  override def toString: String = data.foldLeft("")((acc, elem) => {
    acc + elem.foldLeft("")((innerAcc, value) => if (innerAcc.isEmpty) value.toString()
    else innerAcc + ',' + value) + '\n'
  })
}

object Matrix {
  def apply(data: Mat): Matrix = Matrix(Option(data))
  def apply(data: Option[Mat]): Matrix = new Matrix(data)
  def apply(dataset: Dataset): Matrix = Matrix(dataset.getRows.map(x => x.map(_.toDouble)))
}
