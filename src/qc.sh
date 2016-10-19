#!/bin/sh

export HAIL_BIN=
export PATH=$HAIL_BIN:$PATH
export TMPDIR=
export SOURCE=

/spark/bin/spark-submit --master spark://IP-address:7077 /hail/build/libs/hail-all-spark.jar --tmpdir .../ \
  importvcf $SOURCE/my.vcf.bgz \
  # Remove the Y and mitochondrial chromosomes
  filtervariants expr -c 'v.contig == "Y" || v.contig == "MT" || v.isBiallelic == false' --remove \
  # Split multi-allelic SNPs into biallelic
  splitmulti \
  write -o my.vds