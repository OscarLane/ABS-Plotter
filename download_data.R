library(tidyverse)
library(readabs)

cat_nos_to_download <- c(LFS = "6202.0")

read_abs(cat_no = cat_nos_to_download,
         path = "data")
