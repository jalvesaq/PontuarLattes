source("str_title_case.R")
library(readxl)

# As planilhas baixadas da Plataforma Qualis com as classificações de cada
# área devem estar na pasta "201720"
setwd("201720")

a <- dir(".", "*.xls")
l <- lapply(a, read_excel)
d <- do.call("rbind", l)
q17 <- d[!duplicated(d), ]
colnames(q17) <- c("isxn", "titulo17", "q17")
q17$titulo17 <- str_title_case(q17$titulo17)
q17 <- q17[order(q17$titulo17), ]
save(q17, file = "../qualis_2017_2020.RData")
