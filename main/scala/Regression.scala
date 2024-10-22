import scala.math.abs

object Regression {

  def regression(dataset_file: String,
                 attribute_columns: List[String],
                 value_column: String,
                 test_percentage: Double,
                 alpha: Double,
                 gradient_descent_steps: Int): (Matrix, Double) = {

    val dataset = Dataset(dataset_file)

    val (trainingData, evaluationData) = dataset.selectColumns(value_column :: attribute_columns)
      .split(test_percentage)

    val trainingY = Matrix(trainingData.selectColumn(value_column))
    val evaluationY = Matrix(evaluationData.selectColumn(value_column))
    val X = Matrix(trainingData.selectColumns(attribute_columns)).++(1.0)
    val W = Matrix(List.fill(X.width.get)(List(0.0)))


    def gradientDescent(steps: Int, W: Matrix): Matrix = {
      if (steps == 0) W
      else {
        val estimation = X * W
        val error = (estimation - trainingY)
        error.data match {
          case Some(value) =>
            val gradient = X.transpose.*(error).map(x => x./(error.height.get.toDouble))
            gradientDescent(steps - 1, W - gradient.map(x => x.*(alpha)))
          case None => Matrix(None)
        }
      }
    }

    val gradientPredictions = gradientDescent(gradient_descent_steps, W)
    val predictionOnValidationSet = Matrix(evaluationData.selectColumns(attribute_columns)).++(1.0) * (gradientPredictions)
    val errorOnValidationSet = (evaluationY - predictionOnValidationSet).map(x => x.abs)


    errorOnValidationSet.data match {
      case Some(value) =>
        val error = value.foldLeft(0.0)((acc, z) => acc + z.foldLeft(0.0)(_ + _)) / value.size
        (gradientPredictions, error)
      case None =>
        (Matrix(None), 0.0)
    }
  }

  def main(args: Array[String]): Unit = {
    // Exemplu de utilizare
    print(regression("datasets/houseds.csv", List("GrLivArea", "YearBuilt"), "SalePrice", 0.1, 1e-7, 10000))
  }
}