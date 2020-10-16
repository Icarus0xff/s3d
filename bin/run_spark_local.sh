#!/usr/bin/env bash

set -euxo pipefail

BIN_DIR=`dirname $0`
cd ${BIN_DIR}
cd ..

spark-submit --master "local[*]" --conf spark.driver.memory=4G --deploy-mode "client" --class ray.AppSpark target/s3d-1.0-SNAPSHOT.jar
