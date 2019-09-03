
QualisXLS <- "Qualis_2017-2018.xlsx"
names(QualisXLS) <- "Todas as áreas"

qualis <- read.delim("Qualis_2017-2018.tsv", stringsAsFactors = FALSE)

# Usar issn1 e issn2 do Scielo, do SJR e do SNIP para fazer equivalência na
# tabela Qualis
load("../SJR_SNIP.RData")
issn <- issn[issn$issn1 != "", ]
issn <- issn[issn$issn2 != "", ]
issn <- issn[issn$issn1 != issn$issn2, ]


tem1 <- issn$issn1 %in% qualis$isxn
tem2 <- issn$issn2 %in% qualis$isxn
adicionar1 <- issn[tem2 & !tem1, ]
names(adicionar1) <- c("a", "b")
adicionar2 <- issn[tem1 & !tem2, c(2, 1)]
names(adicionar2) <- c("a", "b")
adicionar <- rbind(adicionar1, adicionar2)

novodf <- character()
for(i in 1:nrow(adicionar)){
    idx <- grep(adicionar$b[i], qualis$isxn)
    novodf <- rbind(novodf, c(adicionar$a[i],
                                  qualis$titulo[idx],
                                  qualis$qualis[idx]))
}

colnames(novodf) <- c("isxn", "titulo", "qualis")

qualis <- rbind(qualis, novodf)
qualis <- qualis[!duplicated(qualis$isxn), ]

library("lettercase")
qualis$titulo <- str_to_title(qualis$titulo)
qualis <- qualis[order(qualis$titulo), ]

save(qualis, QualisXLS, file = "qualis2019.RData")
