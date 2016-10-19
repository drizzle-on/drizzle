#!/bin/sh

export HAIL_BIN=
export PATH=$HAIL_BIN:$PATH
export TMPDIR=
export SOURCE=

/spark/bin/spark-submit --master spark://IP-address:7077 /hail/build/libs/hail-all-spark.jar --tmpdir .../ \
  importvcf $SOURCE/my.vcf.bgz \