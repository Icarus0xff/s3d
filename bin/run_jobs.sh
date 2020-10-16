#!/usr/bin/env bash
set -exo pipefail
ARGS=${@}
##############################################################################################
# Sample Submit Command:                                                                     #
#      bash run_snapshot_dump.sh
# Required Arguments                                                                         #
##############################################################################################

check_var() {

  if [ -z ${!1} ]; then
    echo "Error: environment variable ${1} must be set"
    exit 1
  fi
  echo "${1}: "${!1}
}

check_var SPARK_HOME

BIN_DIR=$(dirname $0)
cd ${BIN_DIR}

${SPARK_HOME}/bin/spark-submit \
  --master yarn --deploy-mode client --principal hbsadmin@TEST-TPL-HADOOP-WH.COM --keytab ~/hbsadmin.keytab \
  --num-executors 5 \
  --executor-cores 30 \
  --name "ray" \
  --conf "spark.driver.memory=4g" \
  --class ray.AppSpark \
  ../target/s3d-1.0-SNAPSHOT.jar ${ARGS}
