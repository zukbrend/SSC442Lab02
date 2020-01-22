# load neccessary libraries
library(tidyverse)

# load the data set
ameslist <- read.table(
  "https://msudataanalytics.github.io/SSC442/assets/ames.csv",
  header = TRUE,
  sep = ","
)

