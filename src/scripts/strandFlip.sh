#!/bin/bash

PLINK=
REF_FILE= 
FILE_TO_MERGE=
TRIAL_MERGE=
FINAL_MISSNP=

# Initial merging of study file with the reference set
$PLINK --bfile $REF_FILE --bmerge $FILE_TO_MERGE --out $TRIAL_MERGE

# Flip the strand for variants identified in the initial merge
$PLINK --bfile $FILE_TO_MERGE --flip $TRIAL_MISSNP --make-bed --out $FLIPPED
# Second merge attempt to identify variants that have other issues than
# the inconsistent strand assignment
$PLINK --bfile $REF_FILE --bmerge $FLIPPED --out $FINAL_MISSNP
# Exclude all the unmerged variants
$PLINK --bfile $FLIPPED --exclude $FINAL_MISSNP-merge.missnp --make-bed --out $OUT