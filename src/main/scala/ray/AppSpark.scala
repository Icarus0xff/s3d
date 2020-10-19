package ray

import org.apache.spark.sql.SparkSession
import ray.algo.PathTracing
import ray.common.Utils

object AppSpark{

  private val width = 1500
  private val height = 1300


  val xi = 0 until width toArray
  val yi = 0 until height toArray
  val MAX_RANDOM_RAY = 128

  def main(args: Array[String]): Unit = {
    val pixs = for {
      x <- xi
      y <- yi
    } yield {
      (x, y)
    }

    val spark = SparkSession.builder()
//      .config("spark.master", "local[*]")
      .getOrCreate()

    import org.apache.spark.sql.functions.udf
    import spark.implicits._
    val renderUDF = udf(PathTracing.renderPix(_, _, MAX_RANDOM_RAY, width, height))

    val df = spark.createDataFrame(pixs).toDF("x", "y").repartition(8000)
      .withColumn("color", renderUDF($"x", $"y"))

    df.persist()
    //    println(df.first)

    val colors = df.collect().map {
      x => (x.getInt(0), x.getInt(1), x.getInt(2))
    }
    df.unpersist()


    Utils.outputImage(width, height, colors)
  }

}
