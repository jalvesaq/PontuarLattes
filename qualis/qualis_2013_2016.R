source("str_title_case.R")

# Ler arquivo baixado da Plataforma Sucupira:
QualisXLS <- "classificacoes_publicadas_todas_as_areas_avaliacao1522078273541.xls"
qualis <- read.delim(QualisXLS, fileEncoding = "Latin1")

levels(qualis$Área.de.Avaliação) <- str_title_case(levels(qualis$Área.de.Avaliação))
levels(qualis$Título) <- str_title_case(levels(qualis$Título))
qualis$Título <- as.character(qualis$Título)
qualis$Estrato <- as.character(qualis$Estrato)
qualis$ISSN <- as.character(qualis$ISSN)
names(qualis) <- c("isxn", "titulo", "area", "qualis")

load("../SJR_SNIP.RData")
issn <- issn[issn$issn1 != "", ]
issn <- issn[issn$issn2 != "", ]
issn <- issn[issn$issn1 != issn$issn2, ]

# Correções
qualis$isxn <- sub("-", "", qualis$isxn)
qualis$qualis <- sub(" *$", "", qualis$qualis)
qualis$titulo <- sub(" *$", "", qualis$titulo)

qualis <- split(qualis, qualis$area)
limpar_area <- function(x){
    x$area <- NULL
    x
}
qualis <- lapply(qualis, limpar_area)

# Adicionar ISSNs alternativos
adicionar_issn <- function(x){
    tem1 <- issn$issn1 %in% x$isxn
    tem2 <- issn$issn2 %in% x$isxn
    adicionar1 <- issn[tem2 & !tem1, ]
    names(adicionar1) <- c("a", "b")
    adicionar2 <- issn[tem1 & !tem2, c(2, 1)]
    names(adicionar2) <- c("a", "b")
    adicionar <- rbind(adicionar1, adicionar2)

    novodf <- character()
    for(i in 1:nrow(adicionar)){
        idx <- grep(adicionar$b[i], x$isxn)[1]
        novodf <- rbind(novodf, c(adicionar$a[i],
                                  x$titulo[idx],
                                  x$qualis[idx]))
    }

    colnames(novodf) <- c("isxn", "titulo", "qualis")

    x <- rbind(x, novodf)
    x <- x[!duplicated(x$isxn), ]
    x
}

qualis <- lapply(qualis, adicionar_issn)

save(QualisXLS, qualis, file = "qualis_2013_2016.RData")
