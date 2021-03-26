source("str_title_case.R")

# Ler arquivo baixado da Plataforma Sucupira:
q13 <- read.delim("classificacoes_publicadas_todas_as_areas_avaliacao1522078273541.xls",
                  fileEncoding = "Latin1", stringsAsFactors = FALSE)

q13$Área.de.Avaliação <- str_title_case(q13$Área.de.Avaliação)
q13$Título <- str_title_case(q13$Título)
names(q13) <- c("isxn", "titulo13", "area", "q13")

load("../auxiliar/SJR_SNIP.RData")
issn <- issn[issn$issn1 != "", ]
issn <- issn[issn$issn2 != "", ]
issn <- issn[issn$issn1 != issn$issn2, ]

# Correções
q13$isxn <- sub("-", "", q13$isxn)
q13$q13 <- sub(" *$", "", q13$q13)
q13$titulo13 <- sub(" *$", "", q13$titulo13)

q13 <- split(q13, q13$area)
limpar_area <- function(x){
    x$area <- NULL
    x
}
q13 <- lapply(q13, limpar_area)

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
                                  x$titulo13[idx],
                                  x$q13[idx]))
    }

    colnames(novodf) <- c("isxn", "titulo13", "q13")

    x <- rbind(x, novodf)
    x <- x[!duplicated(x$isxn), ]
    x
}

q13 <- lapply(q13, adicionar_issn)

if(sum(duplicated(q13$isxn))){
    dup <- q13[duplicated(q13$isxn, fromLast = TRUE) | duplicated(q13$isxn), ]
    cat("ISSN duplicado no Qualis:\n", file = stderr())
    dup[order(dup$isxn), ]
}

save(q13, file = "qualis_2013_2016.RData")
