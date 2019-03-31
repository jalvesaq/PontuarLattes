library("XML")
library("descr")
library("scholar")

source("info.R")

# Ler currículos

doutorado <- list()
posdoc <- list()
premios <- list()
oriand <- list()
oriconc <- list()
ensino <- list()
nlist <- list()
obter.producao <- function(arquivo)
{
    unzip(paste0("lattes_xml/", arquivo), exdir = "/tmp/")
    xl <- xmlTreeParse("/tmp/curriculo.xml", encoding = "latin1")
    xl <- xl$doc$children$`CURRICULO-VITAE`
    prof <- xl$children$`DADOS-GERAIS`
    nomep <- prof$attributes[["NOME-COMPLETO"]]

    da <- sub("(..)(..)(....)", "\\1/\\2/\\3",
              xl$attributes[["DATA-ATUALIZACAO"]])

    if("DOUTORADO" %in% names(prof$children$`FORMACAO-ACADEMICA-TITULACAO`$children)){
        xx <- prof$children$`FORMACAO-ACADEMICA-TITULACAO`$children
        for(ii in 1:length(xx)){
            if("DOUTORADO" == names(xx)[ii]){
                yy <- xx[[ii]]$attributes[c("NOME-INSTITUICAO", "NOME-CURSO", "ANO-DE-CONCLUSAO")]
                doutorado[[length(doutorado)+1]] <<- c("Professor" = nomep, yy, "DataCV" = da)
            }
        }
    }

    if("POS-DOUTORADO" %in% names(prof$children$`FORMACAO-ACADEMICA-TITULACAO`$children)){
        xx <- prof$children$`FORMACAO-ACADEMICA-TITULACAO`$children
        for(ii in 1:length(xx)){
            if("POS-DOUTORADO" == names(xx)[ii]){
                yy <- xx[[ii]]$attributes[c("NOME-INSTITUICAO", "ANO-DE-INICIO", "ANO-DE-CONCLUSAO")]
                posdoc[[length(posdoc)+1]] <<- c("Professor" = nomep, yy)
            }
        }
    }

    if("ORIENTACOES-CONCLUIDAS" %in% names(xl$children$`OUTRA-PRODUCAO`$children)){
        xx <- xl$children$`OUTRA-PRODUCAO`$children$`ORIENTACOES-CONCLUIDAS`$children
        for(ii in 1:length(xx)){
            yy <- xx[[ii]]
            oriconc[[length(oriconc)+1]] <<- c("Professor" = nomep,
                                               yy[[1]]$attributes[c("NATUREZA", "ANO")],
                                               yy[[2]]$attributes[c("NOME-DA-INSTITUICAO",
                                                                    "NOME-DO-CURSO",
                                                                    "NOME-DO-ORIENTADO")])
        }
    }

    if(!is.null(xl$children$`DADOS-COMPLEMENTARES`$children$`ORIENTACOES-EM-ANDAMENTO`)){
        oa <- xl$children$`DADOS-COMPLEMENTARES`$children$`ORIENTACOES-EM-ANDAMENTO`$children
        for(ii in 1:length(oa))
        oriand[[length(oriand)+1]] <<- c("Professor" = nomep,
                                         oa[[ii]]$children[[1]]$attributes[c("NATUREZA", "ANO")],
                                         oa[[ii]]$children[[2]]$attributes[c("NOME-DO-ORIENTANDO",
                                                                             "NOME-INSTITUICAO")])
    }


    if("PREMIOS-TITULOS" %in% names(prof$children)){
        xx <- prof$children$`PREMIOS-TITULOS`$children
        for(ii in 1:length(xx)){
            premios[[length(premios)+1]] <<- c("Professor" = nomep, xx[[ii]]$attributes)
        }
    }

    ap <- xl$children$`DADOS-GERAIS`$children$`ATUACOES-PROFISSIONAIS`$children
    for(ii in 1:length(ap)){
        yy <- ap[[ii]]$children
        if("ATIVIDADES-DE-ENSINO" %in% names(yy))
            for(ens in yy$`ATIVIDADES-DE-ENSINO`$children){
                ensino[[length(ensino)+1]] <<- c("Professsor" = nomep,
                                                 ens$attributes[c("TIPO-ENSINO",
                                                                  "MES-INICIO",
                                                                  "ANO-INICIO",
                                                                  "MES-FIM",
                                                                  "ANO-FIM")],
                                                 ap[[ii]]$attributes["NOME-INSTITUICAO"])
            }
    }

    ap1 <- ap[[1]]

    xl <- xl$children$`PRODUCAO-BIBLIOGRAFICA`
    artigos <- xl$children$`ARTIGOS-PUBLICADOS`
    artigos <- artigos$children
    livros  <- xl$children$`LIVROS-E-CAPITULOS`
    capitulos <- livros$children$`CAPITULOS-DE-LIVROS-PUBLICADOS`
    capitulos <- capitulos$children
    livros <- livros$children$`LIVROS-PUBLICADOS-OU-ORGANIZADOS`
    livros <- livros$children
    rm(xl)

    pegar.artigo <- function(p, prof)
    {
        db <- p$children$`DADOS-BASICOS-DO-ARTIGO`$attributes
        dl <- p$children$`DETALHAMENTO-DO-ARTIGO`$attributes
        if(db[["NATUREZA"]] == "COMPLETO"){
            res <- c(prof, db[["ANO-DO-ARTIGO"]], "Artigo",
                     db[["TITULO-DO-ARTIGO"]],
                     dl[["TITULO-DO-PERIODICO-OU-REVISTA"]],
                     dl[["ISSN"]])
        } else {
            res <- c(NA, NA, NA, NA, NA, NA)
        }
        return(res)
    }

    pegar.capitulo <- function(p, prof)
    {
        db <- p$children$`DADOS-BASICOS-DO-CAPITULO`$attributes
        dl <- p$children$`DETALHAMENTO-DO-CAPITULO`$attributes
        res <- c(prof, db[["ANO"]], "Cap",
                 db[["TITULO-DO-CAPITULO-DO-LIVRO"]],
                 dl[["TITULO-DO-LIVRO"]], dl[["ISBN"]])
        return(res)
    }

    pegar.livro <- function(p, prof)
    {
        db <- p$children$`DADOS-BASICOS-DO-LIVRO`
        db <- db$attributes
        dl <- p$children$`DETALHAMENTO-DO-LIVRO`
        dl <- dl$attributes
        if(db[["TIPO"]] == "LIVRO_ORGANIZADO_OU_EDICAO")
            res <- c(prof, db[["ANO"]], "Org", db[["TITULO-DO-LIVRO"]], NA, dl[["ISBN"]])
        else
            res <- c(prof, db[["ANO"]], "Lvr", db[["TITULO-DO-LIVRO"]], NA, dl[["ISBN"]])
        return(res)
    }

    b <- rbind(do.call("rbind", lapply(artigos,   pegar.artigo,   nomep)),
               do.call("rbind", lapply(capitulos, pegar.capitulo, nomep)),
               do.call("rbind", lapply(livros,    pegar.livro,    nomep)))
    rownames(b) <- NULL
    colnames(b) <- c("prof", "ano", "tipo", "producao", "livro.ou.periodico", "isxn")
    b
}

xx <- lapply(dir("lattes_xml", pattern = "*.zip"), obter.producao)
xx <- do.call("rbind", xx)
p <- as.data.frame(xx, stringsAsFactors = FALSE)
rm(xx, obter.producao)


# Adicionar Fator de Impacto SJR
if(file.exists("qualis/scimagojr 2017.csv")){
    # Não foi possível carregar o banco de dados "scimagojr 2017.csv" no R sem
    # erros porque há campos com aspas desbalanceadas. A solução encontrada
    # foi usar o awk para extrair somente as colunas necessárias.
    system("awk -F ';' '{OFS=\"\t\" ; print $3, $5, $6, $16}' 'qualis/scimagojr 2017.csv' > /tmp/scimagojr_2017_TAB.csv")
    s <- readLines("/tmp/scimagojr_2017_TAB.csv")
    s <- gsub('"', "", s)
    writeLines(s, "/tmp/scimagojrTAB")
    s <- read.delim("/tmp/scimagojrTAB", stringsAsFactors = FALSE)
    names(s) <- c("title", "isxn", "SJR", "country")
    s$SJR <- as.numeric(sub(",", ".", s$SJR))
    s$title <- sub("^ *(.*) *$", "\\1", s$title)

    # Revistas com mais de um ISSN
    s$issn2 <- s$isxn
    s$issn2 <- sub(".*, ", "", s$isxn)
    s$isxn <- sub(",.*", "", s$isxn)

    # Usar isxn e issn2 do SJR para fazer equivalência na tabela Qualis
    for(i in 1:nrow(s)){
        if(s$issn2[i] %in% qualis$isxn && ! s$isxn[i] %in% qualis$isxn)
            qualis <- rbind(qualis,
                            data.frame(isxn = s$isxn[i],
                                       titulo = qualis$titulo[qualis$isxn == s$issn2[i]],
                                       qualis = qualis$qualis[qualis$isxn == s$issn2[i]],
                                       stringsAsFactors = FALSE))
        else
            if(s$isxn[i] %in% qualis$isxn && ! s$issn2[i] %in% qualis$isxn)
                qualis <- rbind(qualis,
                                data.frame(isxn = s$issn2[i],
                                           titulo = qualis$titulo[qualis$isxn == s$isxn[i]],
                                           qualis = qualis$qualis[qualis$isxn == s$isxn[i]],
                                           stringsAsFactors = FALSE))
    }

    # Concluir adição de linhas ao banco de dados do SJR
    s2 <- s
    s2$issn2[s2$isxn == s2$issn2] <- NA
    s2 <- s2[!is.na(s2$issn2), ]
    s$issn2 <- NULL
    s2$isxn <- NULL
    names(s2) <- sub("issn2", "isxn", names(s2))
    s <- rbind(s, s2)
    rm(s2)

    p <- merge(p, s, all.x = TRUE)
}
# Adicionar Qualis

if(exists("equivalente")){
    for(i in 1:length(equivalente))
        if(sum(qualis$isxn == equivalente[i]) == 1){
            idx <- grep(equivalente[i], qualis$isxn)
            qualis <- rbind(qualis, qualis[idx, ])
            qualis[nrow(qualis), "isxn"] <- names(equivalente)[i]
        }
}

p <- merge(p, qualis, all.x = TRUE, stringsAsFactors = FALSE)

p$qualis[is.na(p$qualis) & !is.na(p$tipo) & p$tipo == "Artigo"] <- "SQ"
p$qualis[is.na(p$qualis) & !is.na(p$tipo) & p$tipo != "Artigo"] <- p$tipo[is.na(p$qualis) & !is.na(p$tipo) & p$tipo != "Artigo"]

# Organização de dossiês em periódicos:
idx <- p$tipo != "Artigo" & p$qualis %in% c("A1", "A2", "B1", "B2", "B3", "B4", "B5", "C")
if(sum(idx) > 0)
    p$qualis[idx] <- "OD"

pontos <- as.data.frame(rbind(c(extenso = "Artigo Qualis A1", qualis = "A1"),
                              c("Artigo Qualis A2", "A2"),
                              c("Artigo Qualis B1", "B1"),
                              c("Artigo Qualis B2", "B2"),
                              c("Artigo Qualis B3", "B3"),
                              c("Artigo Qualis B4", "B4"),
                              c("Artigo Qualis B5", "B5"),
                              c("Artigo Qualis C", "C"),
                              c(paste("Artigo sem Qualis na área de", NomeComite), "SQ"),
                              c("Organização de dossiê em periódico", "OD"),
                              c("Livro publicado", "Lvr"),
                              c("Livro organizado", "Org"),
                              c("Capítulo de livro", "Cap")), stringsAsFactors = FALSE)
pontos <- merge(pontos, data.frame(qualis = names(PontosQualis), pontos = PontosQualis, stringsAsFactors = FALSE))

p <- merge(p, pontos, all.x = TRUE)


# Adicionar Fator de Impacto do Google Scholar
# Guardar dados coletados em arquivo para evitar necessidade de baixar dados
# cada vez que este script for executado
if(!dir.exists("~/.cache"))
    dir.create("~/.cache")
if(file.exists("~/.cache/PontuarLattes_GoogleScholar")){
    g <- read.delim("~/.cache/PontuarLattes_GoogleScholar", stringsAsFactors = FALSE)
} else {
    g <- data.frame(tituloUP = character(),
                    tituloG = character(),
                    cites = numeric(),
                    ImpactFactor = numeric(),
                    Eigenfactor = numeric(),
                    stringsAsFactors = FALSE)
}

# Listar títulos não coletados ainda
p$tituloUP <- toupper(p$titulo)
titulos <- levels(factor(p$tituloUP[p$tipo == "Artigo"]))
coletar <- rep(FALSE, length(titulos))
for(i in 1:length(titulos)){
    if(titulos[i] %in% g$tituloUP == FALSE)
        coletar[i] <- TRUE
}
titulos <- titulos[coletar]

if(length(titulos)){
    # Coletar dados dos títulos cujos dados ainda não foram coletados
    gif <- lapply(titulos, get_impactfactor)
    for(i in 1:length(titulos)){
        if(titulos[i] %in% g$titulo == FALSE)
            g <- rbind(g,
                       data.frame(tituloUP = titulos[i],
                                  tituloG = gif[[i]][[1]],
                                  cites = gif[[i]][[2]],
                                  ImpactFactor = gif[[i]][[3]],
                                  Eigenfactor = gif[[i]][[4]],
                                  stringsAsFactors = FALSE))
    }
    write.table(g, "~/.cache/PontuarLattes_GoogleScholar",
                quote = FALSE, sep = "\t", row.names = FALSE)
    rm(gif)
}
rm(titulos, coletar)

p <- merge(p, g, all.x = TRUE)
p$tituloUP <- NULL

# Especificar o período do relatório
pcompleto <- p
p <- p[!is.na(p$ano) & p$ano >= Inicio & p$ano <= Fim, ]

p$ano <- factor(as.numeric(p$ano), levels = Inicio:Fim, labels = as.character(Inicio:Fim))

if(sum(is.na(p$pontos)) > 0){
    cat("\n\n\\textcolor{red}{Pontuação não definida}:\n\n\\begin{verbatim}\n")
    print(p[is.na(p$pontos), c("prof", "livro.ou.periodico", "qualis", "isxn")])
    cat("\\end{verbatim}\n\n")
}

# Informações sobre professores
doutor <- do.call("rbind", doutorado)
colnames(doutor) <- c("Professor", "Instituição doutorado", "Nome do curso", "Ano", "DataCV")
# Reter somente último doutorado concluído:
doutor <- doutor[order(doutor[, "Ano"], decreasing = TRUE), ]
notdup <- !duplicated(doutor[, "Professor"])
doutor <- doutor[notdup, ]

quando <- as.Date(doutor[, "DataCV"], format = "%d/%m/%Y")
doutor <- doutor[order(quando), ]
rm(quando)

p$prof <- factor(p$prof)

plvr <- p[p$tipo %in% c("Lvr", "Cap", "Org"), ]
part <- p[p$tipo == "Artigo", ]

pontuacaoLvr <- as.data.frame(tapply(plvr$pontos, list(plvr$prof, plvr$ano),
                                     sum, na.rm = TRUE))

# Adicionar professores que não produziram no período
falta <- !(doutor[, "Professor"] %in% rownames(pontuacaoLvr))
if(sum(falta) > 0){
    falta <- doutor[falta, "Professor"]
    ftab <- matrix(nrow = length(falta), ncol = ncol(pontuacaoLvr))
    rownames(ftab) <- falta
    colnames(ftab) <- colnames(pontuacaoLvr)
    pontuacaoLvr <- rbind(pontuacaoLvr, ftab)
}

pontuacaoLvr$Professor <- rownames(pontuacaoLvr)
pontuacaoLvr <- pontuacaoLvr[, c(ncol(pontuacaoLvr), 1:(ncol(pontuacaoLvr)-1))]
if(ncol(pontuacaoLvr) > 2){
    pontuacaoLvr$Total <- apply(pontuacaoLvr[, 2:ncol(pontuacaoLvr)], 1, sum, na.rm = TRUE)
    pontuacaoLvr$Total[is.na(pontuacaoLvr$Total)] <- 0
    pmediaL <- round(mean(pontuacaoLvr$Total, na.rm = TRUE))
    pmedianaL <- round(median(pontuacaoLvr$Total, na.rm = TRUE))
    pontuacaoLvr <- pontuacaoLvr[order(pontuacaoLvr$Total, decreasing = TRUE), ]
} else {
    pmediaL <- round(mean(pontuacaoLvr[, 2], na.rm = TRUE))
    pmedianaL <- round(median(pontuacaoLvr[, 2], na.rm = TRUE))
    pontuacaoLvr <- pontuacaoLvr[order(pontuacaoLvr[, 2], decreasing = TRUE), ]
}

pontuacaoArt <- as.data.frame(tapply(part$pontos, list(part$prof, part$ano),
                                     sum, na.rm = TRUE))

# Adicionar professores que não produziram no período
falta <- !(doutor[, "Professor"] %in% rownames(pontuacaoArt))
if(sum(falta) > 0){
    falta <- doutor[falta, "Professor"]
    ftab <- matrix(nrow = length(falta), ncol = ncol(pontuacaoArt))
    rownames(ftab) <- falta
    colnames(ftab) <- colnames(pontuacaoArt)
    pontuacaoArt <- rbind(pontuacaoArt, ftab)
}

pontuacaoArt$Professor <- rownames(pontuacaoArt)
pontuacaoArt <- pontuacaoArt[, c(ncol(pontuacaoArt), 1:(ncol(pontuacaoArt)-1))]
if(ncol(pontuacaoArt) > 2){
    pontuacaoArt$Total <- apply(pontuacaoArt[, 2:ncol(pontuacaoArt)], 1, sum, na.rm = TRUE)
    pontuacaoArt$Total[is.na(pontuacaoArt$Total)] <- 0
    pmediaA <- round(mean(pontuacaoArt$Total, na.rm = TRUE))
    pmedianaA <- round(median(pontuacaoArt$Total, na.rm = TRUE))
    pontuacaoArt <- pontuacaoArt[order(pontuacaoArt$Total, decreasing = TRUE), ]
} else {
    pmediaA <- round(mean(pontuacaoArt[, 2], na.rm = TRUE))
    pmedianaA <- round(median(pontuacaoArt[, 2], na.rm = TRUE))
    pontuacaoArt <- pontuacaoArt[order(pontuacaoArt[, 2], decreasing = TRUE), ]
}

if(is.null(p$SJR)){
    pontuacaoSJR <- matrix("Arquivo `qualis/scimagojr_2017_TAB.csv' não encontrado", 1, 1)
    p$SJR <- NA
} else {
    pontuacaoSJR <- as.data.frame(tapply(part$SJR, list(part$prof, part$ano),
                                     sum, na.rm = TRUE))
    pontuacaoSJR <- cbind(rownames(pontuacaoSJR), pontuacaoSJR,
                          apply(pontuacaoSJR, 1, sum, na.rm = TRUE))
    names(pontuacaoSJR)[1] <- "Professores"
    names(pontuacaoSJR)[ncol(pontuacaoSJR)] <- "Total"
    pontuacaoSJR <- pontuacaoSJR[order(pontuacaoSJR[, "Total"], decreasing = TRUE), ]
}

pontuacaoGgl <- as.data.frame(tapply(part$ImpactFactor, list(part$prof, part$ano),
                                     sum, na.rm = TRUE))
pontuacaoGgl <- cbind(rownames(pontuacaoGgl), pontuacaoGgl,
                      apply(pontuacaoGgl, 1, sum, na.rm = TRUE))
names(pontuacaoGgl)[1] <- "Professores"
names(pontuacaoGgl)[ncol(pontuacaoGgl)] <- "Total"
pontuacaoGgl <- pontuacaoGgl[order(pontuacaoGgl[, "Total"], decreasing = TRUE), ]

# Lista de Pós-doutorados realizados
posdoc <- do.call("rbind", posdoc)
colnames(posdoc) <- c("Professor", "Instituição", "Início", "Fim")
posdoc[, 1] <- sub(" .* ", " ", posdoc[, 1])
posdoc <- posdoc[order(posdoc[, "Fim"]), ]

# Orientações concluídas
oc <- do.call("rbind", oriconc)
colnames(oc) <- c("Professor", "Natureza", "Ano", "Instituição", "Curso", "Orientado")
oc <- as.data.frame(oc)
oc$Ano <- as.numeric(as.character(oc$Ano))
oc <- oc[oc$Ano >= Inicio & oc$Ano <= Fim, ]
oc <- droplevels(oc)

levels(oc$Natureza) <- sub("Dissertação de mestrado", "M", levels(oc$Natureza))
levels(oc$Natureza) <- sub("INICIACAO_CIENTIFICA", "IC", levels(oc$Natureza))
levels(oc$Natureza) <-
    sub("MONOGRAFIA_DE_CONCLUSAO_DE_CURSO_APERFEICOAMENTO_E_ESPECIALIZACAO", "E",
        levels(oc$Natureza))
levels(oc$Natureza) <- sub("ORIENTACAO-DE-OUTRA-NATUREZA", "O", levels(oc$Natureza))
levels(oc$Natureza) <- sub("Supervisão de pós-doutorado", "PD", levels(oc$Natureza))
levels(oc$Natureza) <- sub("Tese de doutorado", "D", levels(oc$Natureza))
levels(oc$Natureza) <- sub("TRABALHO_DE_CONCLUSAO_DE_CURSO_GRADUACAO", "G",
                                levels(oc$Natureza))
oc$um <- 1

oriconcTab <- tapply(oc$um, list(oc$Professor, oc$Natureza), sum)
oriconcTab[is.na(oriconcTab)] <- 0

# Ordenar colunas (nem todos os programas têm todos os tipos de orientações)
ordem <- c("O", "IC", "G", "E", "M", "D", "PD")
ordem <- ordem[ordem %in% levels(oc$Natureza)]

ro <- 1:nrow(oriconcTab)
if("PD" %in% ordem & "D" %in% ordem & "M" %in% ordem){
    ro <- order(oriconcTab[, "PD"], oriconcTab[, "D"], oriconcTab[, "M"],
                decreasing = TRUE)
} else {
    if("D" %in% ordem & "M" %in% ordem){
        ro <- order(oriconcTab[, "D"], oriconcTab[, "M"], decreasing = TRUE)
    } else {
        if("M" %in% ordem)
            ro <- order(oriconcTab[, "M"], decreasing = TRUE)
    }
}

if(nrow(oriconcTab) > 1)
    oriconcTab <- oriconcTab[ro, ordem]

# Detalhamento das orientações concluídas
oc$Professor <- sub(" .* ", " ", oc$Professor)
oc$Orientado <- sub(" .* ", " ", oc$Orientado)
oc$Instituição <- sub("Universidade", "U.", oc$Instituição)
oc$Instituição <- sub("Federal", "F.", oc$Instituição)
oc$Instituição <- sub("Estadual", "E.", oc$Instituição)
oc$Curso <- sub("Programa de Pós-Graduação", "PPG", oc$Curso)
oc <- oc[order(paste(oc$Natureza, oc$Instituição, oc$Curso, oc$Professor, oc$Orientado, oc$Ano)),
         c("Natureza", "Instituição", "Curso", "Professor", "Orientado", "Ano")]

# Orientações em andamento
oa <- do.call("rbind", oriand)
oa <- as.data.frame(oa)
colnames(oa) <- c("Professor", "Natureza", "Ano", "Orientando", "Instituição")
oa <- oa[order(oa$Ano), ]
levels(oa$Natureza) <- sub("Dissertação de mestrado", "M", levels(oa$Natureza))
levels(oa$Natureza) <- sub("Iniciação Científica", "IC", levels(oa$Natureza))
levels(oa$Natureza) <-
    sub("Monografia de conclusão de curso de aperfeiçoamento/especialização", "E",
        levels(oa$Natureza))
levels(oa$Natureza) <- sub("Orientação de outra natureza", "O", levels(oa$Natureza))
levels(oa$Natureza) <- sub("Supervisão de pós-doutorado", "PD", levels(oa$Natureza))
levels(oa$Natureza) <- sub("Tese de doutorado", "D", levels(oa$Natureza))
levels(oa$Natureza) <- sub("Trabalho de conclusão de curso de graduação", "G",
                               oa$Natureza)
oa$um <- 1
oriandTab <- tapply(oa$um, list(oa$Professor, oa$Natureza), sum)
oriandTab[is.na(oriandTab)] <- 0

ordem <- c("O", "IC", "G", "E", "M", "D", "PD")
ordem <- ordem[ordem %in% levels(oa$Natureza)]

ro <- 1:nrow(oriandTab)
if("PD" %in% ordem & "D" %in% ordem & "M" %in% ordem){
    ro <- order(oriandTab[, "PD"], oriandTab[, "D"], oriandTab[, "M"],
                decreasing = TRUE)
} else {
    if("D" %in% ordem & "M" %in% ordem){
        ro <- order(oriandTab[, "D"], oriandTab[, "M"], decreasing = TRUE)
    } else {
        if("M" %in% ordem)
            ro <- order(oriandTab[, "M"], decreasing = TRUE)
    }
}

oriandTab <- oriandTab[ro, ordem]

# Detalhamento das orientações em andamento
oa$um <- NULL
oa <- oa[order(paste(oa$Ano, oa$Professor, oa$Natureza, oa$Instituição)),
         c("Ano", "Professor", "Natureza", "Instituição", "Orientando")]
oa$Professor <- sub(" .* ", " ", oa$Professor)

## Registro do item “Ensino” no período
ens <- do.call("rbind", ensino)
ens <- as.data.frame(ens, stringsAsFactors = FALSE)
names(ens) <- c("Professor", "Tipo", "MI", "AnoI", "MF", "AnoF")
ens <- ens[ens$AnoI >= as.character(Inicio) &
                 (ens$AnoF <= as.character(Fim) | ens$AnoF == ""), ]
ens$um <- 1
ensinoTab <- tapply(ens$um, list(ens$Professor, ens$Tipo), sum)
ensinoTab[is.na(ensinoTab)] <- 0


# Produção bibliográfica (Livros e Artigos)
colnames(pontos) <- c("Classe", "Abreviatura", "Pontos")
if(ncol(pontuacaoLvr) > 2){
    pLvr <- pontuacaoLvr[, c("Professor", "Total")]
    pArt <- pontuacaoArt[, c("Professor", "Total")]
} else {
    # Relatório de somente 1 ano
    pLvr <- pontuacaoLvr
    pArt <- pontuacaoArt
}
colnames(pLvr) <- c("Professor", "Livros")
colnames(pArt) <- c("Professor", "Artigos")

# Professores classificados por pontuação ponderada
pond <- merge(pLvr, pArt, all = TRUE)
pond$Livros[is.na(pond$Livros)] <- 0
pond$Artigos[is.na(pond$Artigos)] <- 0
pond$Livros <- pond$Livros / max(pond$Livros, na.rm = TRUE)
pond$Artigos <- pond$Artigos / max(pond$Artigos, na.rm = TRUE)
pond$Média <- PesoArtigos * pond$Artigos + PesoLivros * pond$Livros
pond <- pond[order(pond$Média, decreasing = TRUE), ]
rownames(pond) <- as.character(1:nrow(pond))



# http://stackoverflow.com/questions/5060076/convert-html-character-entity-encoding-in-r
# Convenience function to convert html codes
html2txt <- function(str) {
      xpathApply(htmlParse(str, asText=TRUE, encoding = "UTF-8"),
                 "//body//text()", xmlValue)[[1]]
}

# Produção segundo classificação Qualis
p$um <- 1
producao <- tapply(p$um, list(p$prof, p$qualis), sum)
producao <- producao[, !grepl("Nada", colnames(producao))]
rownames(producao) <- c(rownames(producao)[1], paste("\\hline", rownames(producao)[2:nrow(producao)]))

# Produção detalhada
b <- p[, c("prof", "producao", "ano", "qualis", "SJR", "livro.ou.periodico", "isxn")]
b <- b[order(p$prof, p$ano, p$producao), ]

b$prof <- sub(" .* ", " ", b$prof)
b$prof <- sub("^(...................).*", "\\1", b$prof)
b$producao <- sapply(b$producao, html2txt)
# b$livro.ou.periodico <- html2txt(b$livro.ou.periodico)
# b$producao <- gsub("_", "\\_", b$producao)
b$livro.ou.periodico <- gsub("_", "\\\\_", b$livro.ou.periodico)

erros <- NULL

bp <- split(b, b$prof)
ObterCapDup <- function(x)
{
    isbnls <- x$isxn[x$qualis == "Lvr"]
    x$capdup <- x$qualis == "Cap" & x$isxn %in% isbnls
    x
}
bp <- lapply(bp, ObterCapDup)
b <- do.call("rbind", bp)
if(sum(b$capdup) > 0){
    b$prof[b$capdup] <- paste("\\rowcolor{capdup}", b$prof[b$capdup])
    erros <- c(erros, "\\rowcolor{capdup}Capítulo indevidamente registrado porque pertence a livro do próprio professor.")
}
b$capdup <- NULL

idx <- ((b$tipo == "Artigo" | b$qualis == "OD") & nchar(b$isxn) != 8) | ((b$tipo != "Artigo" & b$qualis != "OD") & nchar(b$isxn) != 13)
if(sum(idx) > 0){
    b$prof[idx] <- paste("\\rowcolor{ncarac}", b$prof[idx])
    erros <- c(erros, "\\rowcolor{ncarac}ISSN ou ISBN com número inválido de caracteres. O ISSN deve ter 8 caracteres e o ISBN deve ter 13.")
}

# ISBN check digit
checkISBN <- function(x){
    x <- strsplit(x, "")[[1]]
    if(length(x) != 13)
        return(FALSE) # Não é livro
    x <- as.numeric(x)
    if(sum(is.na(x)) > 0)
        return(TRUE)
    correto <- 10 - (sum(x[1:12] * c(1, 3)) %% 10)
    if(correto == 10)
        correto <- 0
    if(x[13] == correto){
        return(FALSE) # Nenhum problema
    } else {
        return(TRUE)  # Erro na soma
    }
}

idx <- sapply(b$isxn, checkISBN)
if(sum(idx) > 0){
    b$prof[idx] <- paste("\\rowcolor{ninval}", b$prof[idx])
    erros <- c(erros, "\\rowcolor{ninval}O ISBN é inválido. Confira se todos os algarismos estão corretos.")
}


b$producao <- sub("^(....................................).*", "\\1", b$producao)
b$livro.ou.periodico <- sub("^(....................................).*", "\\1", b$livro.ou.periodico)
idx <- grep("[A-Z][A-Z][A-Z][A-Z][A-Z]", b$producao)
b$producao[idx] <- sub("^(...........................).*", "\\1", b$producao[idx])
idx <- grep("[A-Z][A-Z][A-Z][A-Z][A-Z]", b$livro.ou.periodico)
b$livro.ou.periodico[idx] <- sub("^(...........................).*", "\\1", b$livro.ou.periodico[idx])
dup <- b[, c("prof", "qualis", "producao")]
dup$producao <- tolower(dup$producao)
dup$producao <- gsub("[[:punct:]]", "", dup$producao)
dup$producao <- gsub("[[:space:]]", "", dup$producao)
dup$producao <- sub("^(........................).*", "\\1", dup$producao)
idx <- duplicated(dup)
if(sum(idx) > 0){
    b$prof[idx] <- paste("\\rowcolor{duplic}", b$prof[idx])
    erros <- c(erros, "\\rowcolor{duplic}Produção registrada mais de uma vez.")
}
rm(idx, dup, checkISBN)
b$livro.ou.periodico <- gsub("&", "\\\\&", b$livro.ou.periodico)
b$producao <- gsub("&", "\\\\&", b$producao)
b$producao <- gsub("#", "\\\\#", b$producao)
b$livro.ou.periodico <- gsub("#", "\\\\#", b$livro.ou.periodico)
levels(b$qualis) <- sub("Nada", " ", levels(b$qualis))
names(b) <- c("Professor", "Produção (títulos truncados)", "Ano", "Qualis", "SJR",
              "Periódico ou Livro (títulos truncados)", "ISSN/ISBN")
proddet <- b
rm(b)

# Títulos de periódicos registrados nos currículos com alguma diferença dos
# títulos na planilha Qualis
ttldif <- p[p$tipo == "Artigo", ]
ttldif <- ttldif[!is.na(ttldif$titulo), ]
ttldif <- ttldif[tolower(ttldif$titulo) != tolower(ttldif$livro.ou.periodico),
       c("titulo", "livro.ou.periodico")]
ttldif <- ttldif[!duplicated(ttldif), ]
colnames(ttldif) <- c("Título Qualis", "Título Lattes")

# Lista de periódicos sem qualis
semqualis <- p[p$qualis == "SQ", c("isxn", "livro.ou.periodico")]
semqualis <- semqualis[!duplicated(semqualis), ]
semqualis <- semqualis[order(semqualis$livro.ou.periodico), ]
semqualis$livro.ou.periodico <- sub(" \x26 ", " \x5c\x5c\x26 ", semqualis$livro.ou.periodico)
colnames(semqualis) <- c("ISSN", "Título do periódico")
