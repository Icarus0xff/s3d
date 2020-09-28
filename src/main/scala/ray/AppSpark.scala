package ray

import org.apache.commons.math3.geometry.euclidean.threed.Vector3D
import org.apache.spark.sql.SparkSession
import ray.algo.PathTracing
import ray.common.Utils

object AppSpark{

  private val width = 1700
  private val height = 1500


  val xi = 0 until width toArray
  val yi = 0 until height toArray

  val eye = new Vector3D(width / 2, height / 2, -700f)
  val MAX_RANDOM_RAY = 16

  def main(args: Array[String]): Unit = {
    val pixs = for {
      x <- xi
      y <- yi
    } yield {
      (x, y)
    }

    val spark = SparkSession.builder()
      .config("spark.master", "local[*]")
      .getOrCreate()
    import org.apache.spark.sql.functions.udf
    import spark.implicits._
    val renderUDF = udf(PathTracing.renderPix(_, _, MAX_RANDOM_RAY, width, height))

    val df = spark.createDataFrame(pixs).toDF("x", "y").repartition(20)
      .withColumn("color", renderUDF($"x", $"y"))

    println(df.count)
    val colors = df.collect().map {
      x => (x.getInt(0), x.getInt(1), x.getInt(2))
    }


    Utils.outputImage(width, height, colors)
  }

}
