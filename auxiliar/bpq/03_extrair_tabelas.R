library(XML)

pq <- data.frame()
fls <- dir(pattern = "*.html")
for(f in fls){
    print(f)
    # Observação: tive que converter os arquivos para TTF-8 para evitar erros
    # de leitura.
    tbs <- readHTMLTable(f)
    tbs <- tbs[sapply(tbs, is.data.frame)]
    tbs <- tbs[sapply(tbs, ncol) == 6]
    tab <- do.call("rbind", tbs)
    if(length(tbs)){
        colnames(tab) <- c("nome", "nivel", "inicio", "termino", "instituicao", "situacao")
        tab <- tab[!is.na(tab$situacao), ]
        rownames(tab) <- NULL
        tab$area <- sub(".html", "", f)
        pq <- rbind(pq, tab)
    }
}
pq$nivel <- factor(pq$nivel)
pq$area <- factor(pq$area)
pq <- pq[!duplicated(pq), ]
pq <- pq[!pq$situacao == "Suspenso", ]
pq$situacao <- NULL
pq$inicio <- as.Date(pq$inicio, format("%d/%m/%Y"))
pq$termino <- as.Date(pq$termino, format("%d/%m/%Y"))

save(pq, file = "pq.RData")
