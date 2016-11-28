#!/bin/bash

PLINK=
REF_FILE= 
FILE_TO_MERGE=
TRIAL_MERGE=

# Initial merging of study file with the reference set
$PLINK --bfile $REF_FILE --bmerge $FILE_TO_MERGE --out $TRIAL_MERGE

# Flip the strand for variants identified in the initial merge
$PLINK --bfile $FILE_TO_MERGE --flip $TRIAL_MISSNP --make-bed --out $FLIPPED