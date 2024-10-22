import scala.io.{BufferedSource, Source}

  class Dataset(m: List[List[String]]) {
  val data: List[List[String]] = m
  override def toString: String = data.foldLeft("")((acc, elem) => {
    acc + elem.foldLeft("")((innerAcc, value) => if (innerAcc.isEmpty) value
    else innerAcc + ',' + value) + '\n'
  })

  def selectColumn(col: String): Dataset =
    Dataset(data.transpose.filter(x => x.head == col).transpose)

  def selectColumns(cols: List[String]): Dataset = {
    val transposeData = data.transpose
    Dataset(cols.flatMap(x => transposeData.filter(y => y.head == x)).transpose)
  }

  def split(percentage: Double): (Dataset, Dataset) = {
    val sortedDataset = data.tail.sortBy(_.head)
    val sequenceNumber = (1 / percentage).ceil
    val (evaluationSet, trainingSet) = sortedDataset.zipWithIndex
      .partition((_, idx) => (idx + 1) % sequenceNumber == 0 && idx != 0)
    (Dataset(data.head::trainingSet.map(_._1)), Dataset(data.head::evaluationSet.map(_._1)))
  }

  def size: Int = data.tail.size
  def getRows: List[List[String]] = data.tail
  def getHeader: List[String] = data.head
}

object Dataset {
  def apply(csv_filename: String): Dataset = {
    val bufferedSource: BufferedSource = Source.fromFile(csv_filename)
    val dataset = Dataset(bufferedSource.getLines.toList.map(x => x.split(',').toList))
    bufferedSource.close()
    dataset
  }
  def apply(ds: List[List[String]]): Dataset = new Dataset(ds)

}
