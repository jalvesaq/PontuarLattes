# Siglas das universidades
siglas <- read.delim("siglas_univ.txt", comment.char = "#", stringsAsFactors = FALSE)

# Gerar tabela de equivalência entre ISSN impresso e online de três fontes:
# SJR, SNIP e site do Scielo

# Lista de ISSNs impresso e eletrônico do Scielo
issn <- read.delim("scielo_issn.tsv", stringsAsFactors = FALSE)
issn <- issn[, c("issn1", "issn2")]

# Carregar o SJR
sjrl <- readLines("scimagojr 2017.csv")
sjrl <- gsub(";;", "\t\t", sjrl)
sjrl <- gsub("(\\S);(\\S)", "\\1\t\\2", sjrl)
sjrl <- gsub("(\\S);(\\S)", "\\1\t\\2", sjrl)
writeLines(sjrl, "/tmp/scimagojrTAB")
sjr <- read.delim("/tmp/scimagojrTAB", stringsAsFactors = FALSE)
sjr$SJR <- as.numeric(sub(",", ".", sjr$SJR))
sjr <- sjr[!is.na(sjr$SJR), ]
names(sjr) <- sub("Issn", "isxn", names(sjr))
sjr <- sjr[, c("Title", "isxn", "SJR", "Country", "Categories")]
sjr$Country <- factor(sjr$Country)

# Revistas com mais de um ISSN
sjr$issn2 <- sjr$isxn
sjr$issn2 <- sub(".*, ", "", sjr$isxn)
sjr$isxn <- sub(",.*", "", sjr$isxn)

# Adicionar novos pares de ISSNs ao data.frame issn
issn.sjr <- sjr[, c("isxn", "issn2")]
names(issn.sjr) <- c("issn1", "issn2")
issn <- rbind(issn, issn.sjr)
rm(issn.sjr)

# Duplicar as linhas com dois ISSNs
s2 <- sjr
s2$issn2[s2$isxn == s2$issn2] <- NA
s2 <- s2[!is.na(s2$issn2), ]
sjr$issn2 <- NULL
s2$isxn <- NULL
names(s2) <- sub("issn2", "isxn", names(s2))
sjr <- rbind(sjr, s2)
rm(s2)

# Remover ISSNs duplicados
sjr <- sjr[!duplicated(sjr$isxn), ]

# Dados do SNIP
# Obter SNIP de http://www.journalindicators.com/methodology#sthash.FN5cRgxb.dpuf%20
if(!file.exists("CWTS Journal Indicators May 2018.xlsx")){
    cat("Baixando o CWTS Journal Indicators\n")
    download.file("http://www.journalindicators.com/Content/CWTS%20Journal%20Indicators%20May%202018.xlsx",
                  destfile = "CWTS Journal Indicators May 2018.xlsx")
}

library("openxlsx")
# TODO: usar área de conhecimento.
snip1 <- read.xlsx("CWTS Journal Indicators May 2018.xlsx", 1)
snip1 <- snip1[snip1$Year == 2017,
               c("Source.title", "Source.type", "Print.ISSN",
                 "Electronic.ISSN", "ASJC.field.IDs", "Year", "SNIP")]
snip1$Print.ISSN <- sub("-", "", snip1$Print.ISSN)
snip1$Electronic.ISSN <- sub("-", "", snip1$Electronic.ISSN)
snip1$Electronic.ISSN <- sub("-", "", snip1$Electronic.ISSN)
snip1$Print.ISSN <- sub(" ", "", snip1$Print.ISSN)
snip1$Electronic.ISSN <- sub(" ", "", snip1$Electronic.ISSN)

# Adicionar novos pares de ISSNs ao data.frame issn
issn.snip <- snip1[, c("Print.ISSN", "Electronic.ISSN")]
names(issn.snip) <- c("issn1", "issn2")
issn <- rbind(issn, issn.snip)
issn <- issn[!duplicated(issn$issn1), ]
rm(issn.snip)

# Duplicar as linhas com dois ISSNs
snip2 <- snip1
names(snip1) <- sub("Print.ISSN", "isxn", names(snip1))
names(snip2) <- sub("Electronic.ISSN", "isxn", names(snip2))
snip1$Electronic.ISSN <- NULL
snip2$Print.ISSN <- NULL
snip <- rbind(snip1, snip2)
snip <- snip[snip$isxn != "", c("Source.title", "isxn", "SNIP")]

# Remover ISSNs duplicados
snip <- snip[!duplicated(snip$isxn), ]

# Juntar SJR e SNIP no mesmo data.frame
sjrsnip <- merge(sjr, snip, all = TRUE)

# Duplicar linhas com ISSN alternativo ausente
sum(issn$issn1 %in% sjrsnip$isxn) / nrow(issn)
sum(issn$issn2 %in% sjrsnip$isxn) / nrow(issn)

ss2 <- list()
for(i in 1:nrow(issn)){
    if(issn$issn1[i] %in% sjrsnip$isxn & !issn$issn2[i] %in% sjrsnip$isxn){
        ss2 <- rbind(ss2, sjrsnip[sjrsnip$isxn == issn$issn1[i], ])
        ss2$isxn[nrow(ss2)] <- issn$issn2[i]
    }
}

for(i in 1:nrow(issn)){
    if(issn$issn2[i] %in% sjrsnip$isxn & !issn$issn2[i] %in% sjrsnip$isxn){
        ss2 <- rbind(ss2, sjrsnip[sjrsnip$isxn == issn$issn2[i], ])
        ss2$isxn[nrow(ss2)] <- issn$issn1[i]
    }
}

sjrsnip <- rbind(sjrsnip, ss2)
table(nchar(sjrsnip$isxn))
sum(is.na(sjrsnip$isxn))
sjrsnip <- sjrsnip[!is.na(sjrsnip$isxn) & nchar(sjrsnip$isxn) > 1, ]

save(sjrsnip, issn, siglas, file = "../SJR_SNIP.RData")
