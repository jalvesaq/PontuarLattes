aqui <- getwd()
setwd("/tmp/scielo")
arquivos <- dir()

ISSN <- function(revista)
{
    l <- readLines(paste0("/tmp/scielo/", revista))
    l <- l[grep("ISSN", l)]
    l <- gsub("\t", "", l)
    l <- gsub("ISSN </FONT>", "\t", l)
    l <- strsplit(l, "\t")[[1]]
    l <- l[grep("^[0-9]", l)]
    l <- sub("<.*", "", l)
    if(length(l) == 0)
        stop("= 0:", revista)
    if(length(l) > 2)
        stop("> 2:", revista)
    if(length(l) == 1)
        l <- c(l, NA)
    c(revista, l)
}

scielo <- do.call(rbind, lapply(arquivos, ISSN))
colnames(scielo) <- c("nome", "issn1", "issn2")
scielo <- as.data.frame(scielo, stringsAsFactors = FALSE)
scielo$issn1 <- sub("-", "", scielo$issn1)
scielo$issn2 <- sub("-", "", scielo$issn2)
setwd(aqui)
write.table(scielo, "../scielo_issn.tsv", sep = "\t", quote = FALSE, row.names = FALSE)
