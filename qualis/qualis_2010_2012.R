source("str_title_case.R")

# Ler arquivo baixado da Plataforma Sucupira:
q10 <- read.delim("classificacoes_publicadas_todas_as_areas_avaliacao.xls",
                  fileEncoding = "Latin1", stringsAsFactors = FALSE)

q10$Área.de.Avaliação <- str_title_case(q10$Área.de.Avaliação)
q10$Título <- str_title_case(q10$Título)
names(q10) <- c("isxn", "titulo10", "area", "q10")

load("../auxiliar/SJR_SNIP.RData")
issn <- issn[issn$issn1 != "", ]
issn <- issn[issn$issn2 != "", ]
issn <- issn[issn$issn1 != issn$issn2, ]

# Correções
q10$isxn <- sub("-", "", q10$isxn)
q10$q10 <- sub(" *$", "", q10$q10)
q10$titulo10 <- sub(" *$", "", q10$titulo10)

q10 <- split(q10, q10$area)

# Modificar nomes para coincidirem com os do quadriênio seguinte
names(q10) <- sub("Administração, Ciências Contábeis e Turismo", "Administração Pública e de Empresas, Ciências Contábeis e Turismo", names(q10))
names(q10) <- sub("Arquitetura e Urbanismo", "Arquitetura, Urbanismo e Design", names(q10))
names(q10) <- sub("Artes / Música", "Artes", names(q10))
names(q10) <- sub("Ciências Sociais Aplicadas I", "Comunicação e Informação", names(q10))
names(q10) <- sub("Filosofia/teologia:subcomissão Filosofia", "Filosofia", names(q10))
names(q10) <- sub("Filosofia/teologia:subcomissão Teologia", "Ciências da Religião e Teologia", names(q10))
names(q10) <- sub("Letras / Linguística", "Linguística e Literatura", names(q10))

limpar_area <- function(x){
    x$area <- NULL
    x
}
q10 <- lapply(q10, limpar_area)

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
                                  x$titulo10[idx],
                                  x$q10[idx]))
    }

    colnames(novodf) <- c("isxn", "titulo10", "q10")

    x <- rbind(x, novodf)
    x <- x[!duplicated(x$isxn), ]
    x
}

q10 <- lapply(q10, adicionar_issn)

if(sum(duplicated(q10$isxn))){
    dup <- q10[duplicated(q10$isxn, fromLast = TRUE) | duplicated(q10$isxn), ]
    cat("ISSN duplicado no Qualis:\n", file = stderr())
    dup[order(dup$isxn), ]
}

save(q10, file = "qualis_2010_2012.RData")
