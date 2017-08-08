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

# Especificar período do relatório
Inicio <- 2013
Fim <- 2016

q10 <- read.table("qualis/classificacoes_publicadas_sociologia_2010_2012.xls",
                  stringsAsFactors = FALSE, header = TRUE,
                  fileEncoding = "Windows-1252", sep = "\t")
q17 <- read.table("qualis/classificacoes_publicadas_sociologia_2017_1496941696361.xls",
                  stringsAsFactors = FALSE, header = TRUE,
                  fileEncoding = "Windows-1252", sep = "\t")
names(q10) <- c("isxn", "titulo", "qualis")
names(q17) <- c("isxn", "titulo", "qualis")
qualis <- rbind(q17, q10)

# Atribuir valor qualis por conta própria
sq <- read.table("qualis/SemQualis.xls", stringsAsFactors = FALSE, header = TRUE, sep = "\t")
qualis <- rbind(qualis, sq)


# Reter somente a classificação mais recente
qualis <- qualis[!duplicated(qualis$isxn), ]

# Correções
qualis$isxn <- sub("-", "", qualis$isxn)
qualis$qualis <- sub(" *$", "", qualis$qualis)
qualis$titulo <- sub(" *$", "", qualis$titulo)

# Considerar versão online = versão impressa quando apenas uma estiver no
# Qualis:
equivalente <- c("23284919" = "23284900", # Current Urban Studies
                 "15730921" = "03038300") # Social Indicators Research
