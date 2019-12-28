library(tidyverse)
library(readabs)

cat_nos_to_download <- c(LFS = "6202.0",
                         National.Accounts = "5206.0")

read_abs(cat_no = "5206.0",
         path = "data")
