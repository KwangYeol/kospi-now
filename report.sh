#!/bin/bash

apt update && apt install -y libcurl4-openssl-dev libssl-dev libxml2-dev

R -e 'chooseCRANmirror(ind=1)
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
renv::restore()'

Rscript ./ticker.R

Rscript ./naver.R

Rscript ./fnguide.R

Rscript ./R/report.R
