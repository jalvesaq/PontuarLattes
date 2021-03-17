source("str_title_case.R")

# Novo Qualis (arquivo não oficial)
QualisXLS <- "Qualis_2017-2018.xlsx"
names(QualisXLS) <- "Todas as áreas"

q17 <- read.delim("Qualis_2017-2018.tsv", stringsAsFactors = FALSE)

# Usar issn1 e issn2 do Scielo, do SJR e do SNIP para fazer equivalência na
# tabela Qualis
load("../SJR_SNIP.RData")
issn <- issn[issn$issn1 != "", ]
issn <- issn[issn$issn2 != "", ]
issn <- issn[issn$issn1 != issn$issn2, ]


tem1 <- issn$issn1 %in% q17$isxn
tem2 <- issn$issn2 %in% q17$isxn
adicionar1 <- issn[tem2 & !tem1, ]
names(adicionar1) <- c("a", "b")
adicionar2 <- issn[tem1 & !tem2, c(2, 1)]
names(adicionar2) <- c("a", "b")
adicionar <- rbind(adicionar1, adicionar2)

novodf <- character()
for(i in 1:nrow(adicionar)){
    idx <- grep(adicionar$b[i], q17$isxn)
    novodf <- rbind(novodf, c(adicionar$a[i], q17$titulo17[idx], q17$q17[idx]))
}

colnames(novodf) <- c("isxn", "titulo17", "q17")

q17 <- rbind(q17, novodf)
q17 <- q17[!duplicated(q17$isxn), ]

q17$titulo17 <- str_title_case(q17$titulo17)
q17 <- q17[order(q17$titulo17), ]

if(sum(duplicated(q17$isxn))){
    dup <- q17[duplicated(q17$isxn, fromLast = TRUE) | duplicated(q17$isxn), ]
    cat("ISSN duplicado no Qualis:\n", file = stderr())
    dup[order(dup$isxn), ]
}

save(q17, QualisXLS, file = "qualis_2017_2020.RData")
