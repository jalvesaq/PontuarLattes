NomeComite <- "Sociologia"

PontosQualis <- c("A1"  = 100,
                  "A2"  =  85,
                  "B1"  =  70,
                  "B2"  =  60,
                  "B3"  =  40,
                  "B4"  =  30,
                  "B5"  =  10,
                  "C"   =   0,
                  "SQ"  =   0,
                  "OD"  =   0,
                  "Lvr" =  70,
                  "Org" =  70,
                  "Cap" =  15)

q15 <- read.table("qualis/classificacoes_publicadas_sociologia_2016_1481820772627.xls", header = TRUE,
                  encoding = "latin1", sep = "\t", colClasses = "character", stringsAsFactors = FALSE)
q14 <- read.table("qualis/classificacoes_publicadas_sociologia_2014.xls", header = TRUE,
                  encoding = "latin1", sep = "\t", colClasses = "character", stringsAsFactors = FALSE)
q13 <- read.table("qualis/classificacoes_publicadas_sociologia_2013.xls", header = TRUE,
                  encoding = "latin1", sep = "\t", colClasses = "character", stringsAsFactors = FALSE)
q12 <- read.table("qualis/classificacoes_publicadas_sociologia_2010_2012.xls", header = TRUE,
                  encoding = "latin1", sep = "\t", stringsAsFactors = FALSE)
q11 <- read.table("qualis/classificacoes_publicadas_sociologia_2010_2011.xls", header = TRUE,
                  encoding = "latin1", sep = "\t", stringsAsFactors = FALSE)
q10 <- read.table("qualis/classificacoes_publicadas_sociologia_2010.xls", header = TRUE,
                  encoding = "latin1", sep = "\t", stringsAsFactors = FALSE)

colnames(q15) <- c("isxn", "titulo15", "estr15")
colnames(q14) <- c("isxn", "titulo14", "estr14")
colnames(q13) <- c("isxn", "titulo13", "estr13")
colnames(q12) <- c("isxn", "titulo12", "estr12")
colnames(q11) <- c("isxn", "titulo11", "estr11")
colnames(q10) <- c("isxn", "titulo10", "estr10")

q15$ano.qualis <- 15
q14$ano.qualis <- 14
q13$ano.qualis <- 13
q12$ano.qualis <- 12
q11$ano.qualis <- 11
q10$ano.qualis <- 10

qualis <- merge(q15, q14, all = TRUE)
qualis <- merge(qualis, q13, all = TRUE)
qualis <- merge(qualis, q12, all = TRUE)
qualis <- merge(qualis, q11, all = TRUE)
qualis <- merge(qualis, q10, all = TRUE)

rm(q10, q11, q12, q13, q14, q15)

qualis$qualis <- qualis$estr15
qualis$titulo <- qualis$titulo15

qualis$titulo[is.na(qualis$qualis)] <- qualis$titulo14[is.na(qualis$qualis)]
qualis$qualis[is.na(qualis$qualis)] <- qualis$estr14[is.na(qualis$qualis)]

qualis$titulo[is.na(qualis$qualis)] <- qualis$titulo13[is.na(qualis$qualis)]
qualis$qualis[is.na(qualis$qualis)] <- qualis$estr13[is.na(qualis$qualis)]

qualis$titulo[is.na(qualis$qualis)] <- qualis$titulo12[is.na(qualis$qualis)]
qualis$qualis[is.na(qualis$qualis)] <- qualis$estr12[is.na(qualis$qualis)]

qualis$titulo[is.na(qualis$qualis)] <- qualis$titulo11[is.na(qualis$qualis)]
qualis$qualis[is.na(qualis$qualis)] <- qualis$estr11[is.na(qualis$qualis)]

qualis$titulo[is.na(qualis$qualis)] <- qualis$titulo10[is.na(qualis$qualis)]
qualis$qualis[is.na(qualis$qualis)] <- qualis$estr10[is.na(qualis$qualis)]

qualis$isxn <- sub("-", "", qualis$isxn)
qualis$qualis <- sub("C ", "C", qualis$qualis)

# Apagar colunas que não serão usadas
qualis$titulo15 <- NULL
qualis$titulo14 <- NULL
qualis$titulo13 <- NULL
qualis$titulo12 <- NULL
qualis$titulo11 <- NULL
qualis$titulo10 <- NULL
qualis$estr15 <- NULL
qualis$estr14 <- NULL
qualis$estr13 <- NULL
qualis$estr12 <- NULL
qualis$estr11 <- NULL
qualis$estr10 <- NULL

# Reter somente a classificação mais recente
qualis <- qualis[order(qualis$isxn, qualis$ano, decreasing = TRUE), ]
qualis  <- qualis[!duplicated(qualis$isxn), ]

# Considerar versão online = versão impressa quando apenas uma estiver no
# Qualis:
equivalente <- c("23284919" = "23284900", # Current Urban Studies
                 "15730921" = "03038300") # Social Indicators Research

