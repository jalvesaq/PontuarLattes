library("XML")
library("ineq")

# Variáveis cujos valores devem ser substituídos no info.R:
NomeProg <- "Ciência Política e Relações Internacionais"
TituloDoc <- "Produção dos Professores do PPG em Ciência Política e Relações Internacionais"
NomeComite <- "Ciência Política e Relações Internacionais"
Autor <- "PPG"
PontosQualis <- c("A1"  = 100,
                  "A2"  =  85,
                  "B1"  =  70,
                  "B2"  =  30,
                  "B3"  =  20,
                  "B4"  =  15,
                  "B5"  =  10,
                  "C"   =   0,
                  "SQ"  =   0,
                  "OD"  =   0,
                  "Lvr" =  60,
                  "Org" =  30,
                  "Cap" =  15)
# Pesos para cálculo da média ponderada
PesoArtigos <- 0.7
PesoLivros <- 0.3
# Período do relatório
Inicio <- 2017
Fim <- 2019
QualisPorTitulo <- FALSE

# Carregar dados do SJR e SNIP
load("SJR_SNIP.RData")

# A leitura do info.R fica aqui para possibilitar a alteração dos pesos das
# diferentes categorias do SJR e SNIP.
if(file.exists("info.R"))
    source("info.R")

# Novo Qualis?
if(sum(grepl("A3", names(PontosQualis)))){
    load("qualis/qualis_2017_2020.RData")
    QNovo <- TRUE
} else {
    load("qualis/qualis_2013_2016.RData")
    if(NomeComite %in% names(qualis)){
        qualis <- qualis[[NomeComite]]
    } else {
        writeLines(names(qualis), "nomes_validos.txt")
        stop(paste0("Variável 'NomeComite' inválida: '", NomeComite,
                    "'.\nVeja o arquivo 'nomes_validos.txt'."))
    }
    QNovo <- FALSE
}

sum(duplicated(qualis$isxn))

dup <- qualis[duplicated(qualis$isxn, fromLast = TRUE) | duplicated(qualis$isxn), ]
dup[order(dup$isxn), ]

sum(duplicated(sjrsnip$isxn))
sum(duplicated(sjrsnip$isxn))

qualis <- merge(qualis, sjrsnip, all = TRUE)
qualis[duplicated(qualis$isxn), ]

if(sum(duplicated(qualis$isxn)))
    stop("ISSN duplicado")


# http://stackoverflow.com/questions/5060076/convert-html-character-entity-encoding-in-r
# Convenience function to convert html codes
html2txt <- function(x) {
    if(is.na(x))
        return(NA)
    xpathApply(htmlParse(x, asText=TRUE, encoding = "UTF-8"),
               "//body//text()", xmlValue)[[1]]
}


NomeSigla <- function(x)
{
    for(i in 1:nrow(siglas))
        x <- sub(paste0("^", siglas$nome[i], "$"), siglas$sigla[i], x)
    x
}

AbreviarInstituicao <- function(x)
{
    x <- sub("Universidade", "U.", x, ignore.case = TRUE)
    x <- sub("Federal", "F.", x, ignore.case = TRUE)
    x <- sub("Estadual", "E.", x, ignore.case = TRUE)
    x <- sub("Programa de Pós.Graduação", "PPG", x, ignore.case = TRUE)
    x <- sub("Programa de Pós.Graduação", "PPG", x, ignore.case = TRUE)
    x <- sub("Programa Pós.Graduação", "PPG", x, ignore.case = TRUE)
    x
}


# Ler currículos

datacv <- list()
doutorado <- list()
posdoc <- list()
premios <- list()
oriand <- list()
oriconc <- list()
ensino <- list()
extensao <- list()
projext <- list()
nlist <- list()
cnpqId <- matrix(character(), ncol = 2, nrow = 0)
colnames(cnpqId) <- c("Professor", "id")

obter.producao <- function(arquivo)
{
    if(grepl("zip", arquivo)){
        unzip(paste0("lattes_xml/", arquivo), exdir = "/tmp/")
        xl <- xmlTreeParse("/tmp/curriculo.xml", encoding = "latin1")
    } else {
        xl <- xmlTreeParse(paste0("lattes_xml/", arquivo), encoding = "latin1")
    }
    if("ERRO" %in% names(xl$doc$children))
        stop(paste0("O currículo ", arquivo, " contém ERRO. Verifique se usou o link correto para baixar o arquivo."))
    xl <- xl$doc$children$`CURRICULO-VITAE`
    prof <- xl$children$`DADOS-GERAIS`
    nomep <- prof$attributes[["NOME-COMPLETO"]]
    cnpqId <<- rbind(cnpqId, c(nomep, xl$attributes[["NUMERO-IDENTIFICADOR"]]))

    da <- sub("(..)(..)(....)", "\\1/\\2/\\3",
              xl$attributes[["DATA-ATUALIZACAO"]])
    datacv[[length(datacv)+1]] <<- c("Professor" = nomep, "DataCV" = da)

    if("DOUTORADO" %in% names(prof$children$`FORMACAO-ACADEMICA-TITULACAO`$children)){
        xx <- prof$children$`FORMACAO-ACADEMICA-TITULACAO`$children
        for(ii in 1:length(xx)){
            if("DOUTORADO" == names(xx)[ii]){
                yy <- xx[[ii]]$attributes[c("NOME-INSTITUICAO", "NOME-CURSO", "ANO-DE-CONCLUSAO")]
                doutorado[[length(doutorado)+1]] <<- c("Professor" = nomep, yy)
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
        for(ii in 1:length(oa)){
            if(length(oa[[ii]]$children) > 1)
                oriand[[length(oriand)+1]] <<- c("Professor" = nomep,
                                                 oa[[ii]]$children[[1]]$attributes[c("NATUREZA", "ANO")],
                                                 oa[[ii]]$children[[2]]$attributes[c("NOME-DO-ORIENTANDO", "NOME-INSTITUICAO")])
        }
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

    ap <- xl$children$`DADOS-GERAIS`$children$`ATUACOES-PROFISSIONAIS`$children
    for(ii in 1:length(ap)){
        yy <- ap[[ii]]$children
        if("ATIVIDADES-DE-EXTENSAO-UNIVERSITARIA" %in% names(yy))
            for(ens in yy$`ATIVIDADES-DE-EXTENSAO-UNIVERSITARIA`$children){
                extensao[[length(extensao)+1]] <<- c("Professsor" = nomep,
                                                 ens$attributes[c("ATIVIDADE-DE-EXTENSAO-REALIZADA",
                                                                  "MES-INICIO",
                                                                  "ANO-INICIO",
                                                                  "MES-FIM",
                                                                  "ANO-FIM")])
            }
    }

    ap <- xl$children$`DADOS-GERAIS`$children$`ATUACOES-PROFISSIONAIS`$children
    for(ii in 1:length(ap)){
        yy <- ap[[ii]]$children
        if("ATIVIDADES-DE-PARTICIPACAO-EM-PROJETO" %in% names(yy)){
            for(ens in yy$`ATIVIDADES-DE-PARTICIPACAO-EM-PROJETO`$children){
                for(prj in ens$children){
                    if("NATUREZA" %in% names(prj$attributes) &&
                       prj$attributes["NATUREZA"] == "EXTENSAO"){
                        projext[[length(projext)+1]] <<- c("Professsor" = nomep,
                                                           prj$attributes[c("NOME-DO-PROJETO",
                                                                            "MES-INICIO",
                                                                            "ANO-INICIO",
                                                                            "MES-FIM",
                                                                            "ANO-FIM")])
                    }
                }
            }
        }
    }

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
        c(prof, db[["ANO-DO-ARTIGO"]],
          ifelse(db[["NATUREZA"]] == "COMPLETO", "Artigo", "NãoArt"),
          db[["TITULO-DO-ARTIGO"]],
          dl[["TITULO-DO-PERIODICO-OU-REVISTA"]],
          dl[["VOLUME"]],
          dl[["SERIE"]],
          dl[["PAGINA-INICIAL"]],
          dl[["PAGINA-FINAL"]],
          dl[["ISSN"]])
    }

    pegar.capitulo <- function(p, prof)
    {
        db <- p$children$`DADOS-BASICOS-DO-CAPITULO`$attributes
        dl <- p$children$`DETALHAMENTO-DO-CAPITULO`$attributes
        c(prof, db[["ANO"]], "Cap",
          db[["TITULO-DO-CAPITULO-DO-LIVRO"]],
          dl[["TITULO-DO-LIVRO"]], NA, NA,
          dl[["PAGINA-INICIAL"]],
          dl[["PAGINA-FINAL"]],
          dl[["ISBN"]])
    }

    pegar.livro <- function(p, prof)
    {
        db <- p$children$`DADOS-BASICOS-DO-LIVRO`
        db <- db$attributes
        dl <- p$children$`DETALHAMENTO-DO-LIVRO`
        dl <- dl$attributes
        c(prof, db[["ANO"]],
          ifelse(db[["TIPO"]] == "LIVRO_ORGANIZADO_OU_EDICAO", "Org", "Lvr"),
          db[["TITULO-DO-LIVRO"]], NA, NA, NA, NA, NA, dl[["ISBN"]])
    }

    b <- rbind(do.call("rbind", lapply(artigos,   pegar.artigo,   nomep)),
               do.call("rbind", lapply(capitulos, pegar.capitulo, nomep)),
               do.call("rbind", lapply(livros,    pegar.livro,    nomep)))
    rownames(b) <- NULL

    # b = NULL se o autor do currículo nunca tiver publicado nada:
    if(!is.null(b))
        colnames(b) <- c("prof", "ano", "tipo", "producao", "livro.ou.periodico",
                         "vol", "num", "pini", "pfim", "isxn")
    b
}

lsxml <- c(dir("lattes_xml", pattern = "*.zip"), dir("lattes_xml", pattern = "*.xml"))
if(length(lsxml) == 0)
    stop("Nenhum currículo encontrado na pasta 'lattes_xml'")
xx <- lapply(lsxml, obter.producao)
xx <- do.call("rbind", xx)
p <- as.data.frame(xx, stringsAsFactors = FALSE)
rm(xx, obter.producao)

if(exists("equivalente")){
    for(i in 1:length(equivalente))
        if(sum(qualis$isxn == equivalente[i]) == 1){
            idx <- grep(equivalente[i], qualis$isxn)
            qualis <- rbind(qualis, qualis[idx, ])
            qualis[nrow(qualis), "isxn"] <- names(equivalente)[i]
        }
}


p <- merge(p, qualis, all.x = TRUE, stringsAsFactors = FALSE)

# Adicionar Qualis por título
if(QualisPorTitulo){
    p2 <- p[p$tipo == "Artigo" & is.na(p$qualis), c("isxn", "livro.ou.periodico")]
    names(p2) <- c("issn", "titulo")
    p2 <- p2[!duplicated(p2$titulo) & !is.na(p2$issn) & p2$issn != "", ]
    p2 <- p2[!duplicated(p2$issn), ]
    q2 <- qualis
    q2$titulo <- tolower(q2$titulo)
    p2$titulo <- tolower(p2$titulo)
    p2 <- merge(p2, q2, stringsAsFactors = FALSE)
    if(nrow(p2) > 0){
        for(i in 1:nrow(p2)){
            p$qualis[p$isxn == p2$issn[i]] <- p2$qualis[i]
            p$title.sjr[p$isxn == p2$issn[i]] <- p2$title.sjr[i]
            p$SJR[p$isxn == p2$issn[i]] <- p2$SJR[i]
            p$Country[p$isxn == p2$issn[i]] <- p2$Country[i]
            p$cat.sjr[p$isxn == p2$issn[i]] <- p2$cat.sjr[i]
            p$Source.title[p$isxn == p2$issn[i]] <- p2$Source.title[i]
            p$SNIP[p$isxn == p2$issn[i]] <- p2$SNIP[i]
            p$ASJC.field.IDs[p$isxn == p2$issn[i]] <- p2$ASJC.field.IDs[i]
        }
    }
}


# Organização de dossiês em periódicos:
if(QNovo){
    p$qualis[is.na(p$qualis) & p$tipo == "Artigo"] <- "NP"
    p$qualis[is.na(p$qualis) & p$tipo != "Artigo"] <- p$tipo[is.na(p$qualis) & p$tipo != "Artigo"]

    idx <- p$tipo != "Artigo" & p$tipo != "NãoArt" &
        p$qualis %in% c("A1", "A2", "A3", "A4", "B1", "B2", "B3", "B4", "C", "NP")
    if(sum(idx) > 0)
        p$qualis[idx] <- "OD"

    pontos <- as.data.frame(rbind(c(extenso = "Artigo Qualis A1", qualis = "A1"),
                                  c("Artigo Qualis A2", "A2"),
                                  c("Artigo Qualis A3", "A3"),
                                  c("Artigo Qualis A4", "A4"),
                                  c("Artigo Qualis B1", "B1"),
                                  c("Artigo Qualis B2", "B2"),
                                  c("Artigo Qualis B3", "B3"),
                                  c("Artigo Qualis B4", "B4"),
                                  c("Artigo Qualis C", "C"),
                                  c(paste("Artigo sem Qualis na área de", NomeComite), "NP"),
                                  c("Organização de dossiê em periódico", "OD"),
                                  c("Livro publicado", "Lvr"),
                                  c("Livro organizado", "Org"),
                                  c("Capítulo de livro", "Cap")), stringsAsFactors = FALSE)
    pontos <- merge(pontos, data.frame(qualis = names(PontosQualis), pontos = PontosQualis,
                                       stringsAsFactors = FALSE))
} else {
    p$qualis[is.na(p$qualis) & p$tipo == "Artigo"] <- "SQ"
    p$qualis[is.na(p$qualis) & p$tipo != "Artigo"] <- p$tipo[is.na(p$qualis) & p$tipo != "Artigo"]

    idx <- p$tipo != "Artigo" & p$tipo != "NãoArt" &
        p$qualis %in% c("A1", "A2", "B1", "B2", "B3", "B4", "B5", "C")
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
    pontos <- merge(pontos, data.frame(qualis = names(PontosQualis), pontos = PontosQualis,
                                       stringsAsFactors = FALSE))
}

p <- merge(p, pontos, all.x = TRUE, stringsAsFactors = FALSE)
p$pontos[p$tipo == "NãoArt"] <- 0

# # Adicionar Fator de Impacto do Google Scholar
# # Guardar dados coletados em arquivo para evitar necessidade de baixar dados
# # cada vez que este script for executado
# if(!dir.exists("~/.cache"))
#     dir.create("~/.cache")
# if(file.exists("~/.cache/PontuarLattes_GoogleScholar")){
#     g <- read.delim("~/.cache/PontuarLattes_GoogleScholar", stringsAsFactors = FALSE)
# } else {
#     g <- data.frame(tituloUP = character(),
#                     tituloG = character(),
#                     cites = numeric(),
#                     ImpactFactor = numeric(),
#                     Eigenfactor = numeric(),
#                     stringsAsFactors = FALSE)
# }
#
# # Listar títulos não coletados ainda
# p$tituloUP <- toupper(p$titulo)
# titulos <- levels(factor(p$tituloUP[p$tipo == "Artigo"]))
# coletar <- rep(FALSE, length(titulos))
# for(i in 1:length(titulos)){
#     if(titulos[i] %in% g$tituloUP == FALSE)
#         coletar[i] <- TRUE
# }
# titulos <- titulos[coletar]
#
# if(length(titulos)){
#     # Coletar dados dos títulos cujos dados ainda não foram coletados
#     gif <- lapply(titulos, get_impactfactor)
#     for(i in 1:length(titulos)){
#         if(titulos[i] %in% g$titulo == FALSE)
#             g <- rbind(g,
#                        data.frame(tituloUP = titulos[i],
#                                   tituloG = gif[[i]][[1]],
#                                   cites = gif[[i]][[2]],
#                                   ImpactFactor = gif[[i]][[3]],
#                                   Eigenfactor = gif[[i]][[4]],
#                                   stringsAsFactors = FALSE))
#     }
#     write.table(g, "~/.cache/PontuarLattes_GoogleScholar",
#                 quote = FALSE, sep = "\t", row.names = FALSE)
#     rm(gif)
# }
# rm(titulos, coletar)
#
# p <- merge(p, g, all.x = TRUE, stringsAsFactors = FALSE)
# p$tituloUP <- NULL


# Especificar o período do relatório
p <- p[p$ano > "1900" & p$ano < "2100", ] # O ano pode não estar especificado

# Detectar coautorias
p$chave <- tolower(paste(p$isxn, p$ano, p$tipo, p$vol, p$num, p$pini))
coaut <- table(p$chave)
coaut <- data.frame(chave = names(coaut), ncoaut = as.numeric(coaut), stringsAsFactors = FALSE)
p <- merge(p, coaut)

pcompleto <- p
p <- p[!is.na(p$ano) & p$ano >= Inicio & p$ano <= Fim, ]

p$ano <- factor(as.numeric(p$ano), levels = Inicio:Fim, labels = as.character(Inicio:Fim))

if(sum(is.na(p$pontos)) > 0){
    cat("\n\n\\textcolor{red}{Pontuação não definida}:\n\n\\begin{verbatim}\n")
    print(p[is.na(p$pontos), c("prof", "livro.ou.periodico", "qualis", "isxn")])
    cat("\\end{verbatim}\n\n")
}

# Data de atualização do currículo
quando <- as.data.frame(do.call("rbind", datacv))
quando$DataCV <- as.Date(quando$DataCV, format = "%d/%m/%Y")
quando <- quando[order(quando$DataCV), ]
quando$Dias <- as.integer(as.Date(Sys.time()) - quando$DataCV)

# Informações sobre doutorado
if(length(doutorado) > 1){
    doutor <- do.call("rbind", doutorado)
    doutor[, 2] <- NomeSigla(sapply(doutor[, 2], html2txt))
    colnames(doutor) <- c("Professor", "Instituição doutorado", "Nome do curso", "Ano")
    # Reter somente último doutorado concluído:
    doutor <- doutor[order(doutor[, "Ano"], decreasing = TRUE), ]
    notdup <- !duplicated(doutor[, "Professor"])
    doutor <- doutor[notdup, ]
} else {
    doutor <- NULL
}

p$prof <- factor(p$prof)

TabProd <- function(d, v, g = FALSE)
{
    pontos <- d[[v]] / d$ncoaut
    tab <- tapply(pontos, list(d$prof, d$ano), sum, na.rm = TRUE)

    # Adicionar professores que não produziram no período
    falta <- !(quando[, "Professor"] %in% rownames(tab))
    if(sum(falta) > 0){
        falta <- quando[falta, "Professor"]
        ftab <- matrix(nrow = length(falta), ncol = ncol(tab))
        rownames(ftab) <- falta
        colnames(ftab) <- colnames(tab)
        tab <- rbind(tab, ftab)
    }

    if(ncol(tab) > 2)
        tab <- cbind(tab, "Total" = apply(tab, 1, sum, na.rm = TRUE))
    tab[is.na(tab)] <- 0
    cn <- colnames(tab)
    tab <- tab[order(tab[, ncol(tab)], decreasing = TRUE), ]
    # Matrix com uma coluna se transforma em vetor
    if(is.null(dim(tab)[1])){
        rn <- names(tab)
        tab <- matrix(tab, nrow = length(tab), ncol = 1)
        rownames(tab) <- rn
        colnames(tab) <- cn
    }
    media <- apply(tab, 2, mean)
    mrow <- max(media)
    if(mrow > 10){
        rr <- 1
    } else {
        if(mrow > 1){
            rr <- 2
        } else {
            rr <- 3
        }
    }
    tab <- rbind(tab, "Média" = round(media, rr))
    if(g){
        tab <- rbind(tab, "Gini" = apply(tab, 2, Gini))
    }

    tab
}

p$SJR.pond <- p$SJR
if(min(sjr.cat$Peso) != max(sjr.cat$Peso)){
    p$SJR.pond <- p$SJR * min(sjr.cat$Peso)
    SJRPond <- sjr.cat[sjr.cat$Peso > min(sjr.cat$Peso), ]
    SJRPond <- SJRPond[order(SJRPond$Peso), ]
    SJRPond$Categoria <- as.character(SJRPond$Categoria)
    for(i in 1:nrow(SJRPond)){
        idx <- grep(paste0("^", SJRPond$Categoria[i], " \\(Q"), p$cat.sjr)
        p$SJR.pond[idx] <- SJRPond$Peso[i] * p$SJR[idx]
        idx <- grep(paste0("; ", SJRPond$Categoria[i], " \\(Q"), p$cat.sjr)
        p$SJR.pond[idx] <- SJRPond$Peso[i] * p$SJR[idx]
    }
    SJRPond <- SJRPond[order(SJRPond$Peso, decreasing = TRUE), ]
    SJRPond <- rbind(SJRPond,
                     data.frame("Categoria" = "Outras categorias",
                                "Peso" = min(sjr.cat$Peso),
                                stringsAsFactors = FALSE))
} else {
    SJRPond <- data.frame("Categoria" = "Todas as categorias", "Peso" = max(sjr.cat$Peso))
}

p$SNIP.pond <- p$SNIP
if(min(snip.cat$Peso) != max(snip.cat$Peso)){
    p$SNIP.pond <- p$SNIP * min(snip.cat$Peso)
    SNIPPond <- snip.cat[snip.cat$Peso > min(snip.cat$Peso), ]
    SNIPPond <- SNIPPond[order(SNIPPond$Peso), ]
    for(i in 1:nrow(SNIPPond)){
        idx <- grep(paste0("^", SNIPPond$id[i]), p$ASJC.field.IDs)
        p$SNIP.pond[idx] <- SNIPPond$Peso[i] * p$SNIP[idx]
        idx <- grep(paste0("; ", SNIPPond$id[i]),  p$ASJC.field.IDs)
        p$SNIP.pond[idx] <- SNIPPond$Peso[i] * p$SNIP[idx]
    }
    SNIPPond <- SNIPPond[order(SNIPPond$Peso, decreasing = TRUE), ]
    SNIPPond <- rbind(SNIPPond,
                     data.frame("id" = "",
                                "Categoria" = "Outras categorias",
                                "Peso" = min(snip.cat$Peso),
                                stringsAsFactors = FALSE))
} else {
    SNIPPond <- data.frame("id" = "",
                           "Categoria" = "Todas as categorias",
                           "Peso" = max(snip.cat$Peso))
}

pontuacaoLvr  <- TabProd(p[p$tipo %in% c("Lvr", "Cap", "Org"), ], "pontos")
pontuacaoArt  <- TabProd(p[p$tipo == "Artigo", ], "pontos")
pontuacaoSNIP <- TabProd(p[p$tipo == "Artigo", ], "SNIP", TRUE)
pontuacaoSJR  <- TabProd(p[p$tipo == "Artigo", ], "SJR", TRUE)
pontuacaoSJRPond  <- TabProd(p[p$tipo == "Artigo", ], "SJR.pond", TRUE)
pontuacaoSNIPPond  <- TabProd(p[p$tipo == "Artigo", ], "SNIP.pond", TRUE)

p4 <- p[p$tipo == "Artigo", c("prof", "ano", "pontos")]
p4s <- split(p4, p4$prof)
QuatroMaiores <- function(x)
{
    x <- x[order(x$pontos, decreasing = TRUE), ]
    if(nrow(x) > 4)
        x <- x[1:4, ]
    x
}
p4s <- lapply(p4s, QuatroMaiores)
p4 <- do.call("rbind", p4s)
tab <- tapply(p4$pontos, p4$prof, sum)
falta <- !(quando[, "Professor"] %in% names(tab))
if(sum(falta) > 0){
    zf <- rep(0, sum(falta))
    names(zf) <- quando[falta, "Professor"]
    tab <- c(tab, zf)
}
tab <- sort(tab, decreasing = TRUE)
pontuacaoArt4 <- data.frame(Professor = names(tab), Pontos = unname(tab))

# Lista de Pós-doutorados realizados
posdoc <- do.call("rbind", posdoc)
if(is.null(posdoc)){
    posdoc <- matrix(NA, ncol = 4)
} else {
    posdoc[, 2] <- NomeSigla(sapply(posdoc[, 2], html2txt))
    if(nrow(posdoc) > 1)
        posdoc <- posdoc[order(posdoc[, 4]), ]
}
colnames(posdoc) <- c("Professor", "Instituição", "Início", "Fim")

# Orientações concluídas
if(length(oriconc)){
    oc <- do.call("rbind", oriconc)
    colnames(oc) <- c("Professor", "Natureza", "Ano", "Instituição", "Curso", "Orientado")
    oc[, 4] <- NomeSigla(sapply(oc[, 4], html2txt))
    oc <- as.data.frame(oc, stringsAsFactors = FALSE)
    oc$Instituição <- sapply(oc$Instituição, html2txt)
    oc$Ano <- as.numeric(as.character(oc$Ano))
    oc <- oc[oc$Ano >= Inicio & oc$Ano <= Fim, ]

    oc$Natureza <- sapply(oc$Natureza, html2txt)
    oc$Natureza <- sub("Dissertação de mestrado", "M", oc$Natureza)
    oc$Natureza <- sub("INICIACAO_CIENTIFICA", "IC", oc$Natureza)
    oc$Natureza <-
        sub("MONOGRAFIA_DE_CONCLUSAO_DE_CURSO_APERFEICOAMENTO_E_ESPECIALIZACAO", "E",
            oc$Natureza)
    oc$Natureza <- sub("ORIENTACAO-DE-OUTRA-NATUREZA", "O", oc$Natureza)
    oc$Natureza <- sub("Supervisão de pós-doutorado", "PD", oc$Natureza)
    oc$Natureza <- sub("Tese de doutorado", "D", oc$Natureza)
    oc$Natureza <- sub("TRABALHO_DE_CONCLUSAO_DE_CURSO_GRADUACAO", "G",
                       oc$Natureza)
    oc$Natureza <- factor(oc$Natureza)
    oc$um <- 1

    oriconcTab <- tapply(oc$um, list(oc$Professor, oc$Natureza), sum)
    oriconcTab[is.na(oriconcTab)] <- 0

    # Ordenar colunas (nem todos os programas têm todos os tipos de orientações)
    ordem <- c("O", "IC", "G", "E", "M", "D", "PD")
    faltaCol <- !(levels(oc$Natureza) %in% ordem)
    if(sum(faltaCol)){
        print(levels(oc$Natureza))
        stop(paste0("Tipo de orientação não reconhecida: ", levels(oc$Natureza)[faltaCol]))
    }

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
    oc$Instituição <- AbreviarInstituicao(oc$Instituição)
    oc$Curso <- AbreviarInstituicao(oc$Curso)
    oc <- oc[order(paste(oc$Natureza, oc$Instituição, oc$Curso, oc$Professor, oc$Orientado, oc$Ano)),
             c("Natureza", "Instituição", "Curso", "Professor", "Orientado", "Ano")]
} else {
    oriconcTab <- data.frame()
}



# Prêmios
if(length(premios)){
    if(length(premios) > 1)
        premios <- do.call("rbind", premios)
    else
        premios <- matrix(premios[[1]], nrow = 1)
    colnames(premios) <- c("Professor", "Prêmio", "Entidade promotora", "Ano", "En")
    if(nrow(premios) > 1)
        premios <- premios[order(premios[, "Ano"]), 1:4]
    premios[, "Professor"] <- sub(" .* ", " ", premios[, "Professor"])
    premios <- as.data.frame(premios, stringsAsFactors = FALSE)
    premios <- premios[premios[, "Ano"] >= Inicio & premios[, "Ano"] <= Fim, ]
}

# Orientações em andamento
if(length(oriand)){
    oa <- do.call("rbind", oriand)
    oa <- as.data.frame(oa, stringsAsFactors = FALSE)
    colnames(oa) <- c("Professor", "Natureza", "Ano", "Orientando", "Instituição")
    oa$Instituição <- sapply(oa$Instituição, html2txt)
    oa$Instituição <- NomeSigla(sapply(oa$Instituição, html2txt))
    oa$Instituição <- AbreviarInstituicao(oa$Instituição)
    oa <- oa[order(oa$Ano), ]
    oa$Natureza <- sapply(oa$Natureza, html2txt)
    oa$Natureza <- factor(oa$Natureza)
    levels(oa$Natureza) <- sub("Dissertação de mestrado", "M", levels(oa$Natureza))
    levels(oa$Natureza) <- sub("Iniciação Científica", "IC", levels(oa$Natureza))
    levels(oa$Natureza) <-
        sub("Monografia de conclusão de curso de aperfeiçoamento/especialização", "E",
            levels(oa$Natureza))
    levels(oa$Natureza) <- sub("Orientação de outra natureza", "O", levels(oa$Natureza))
    levels(oa$Natureza) <- sub("Supervisão de pós-doutorado", "PD", levels(oa$Natureza))
    levels(oa$Natureza) <- sub("Tese de doutorado", "D", levels(oa$Natureza))
    levels(oa$Natureza) <- sub("Trabalho de conclusão de curso de graduação", "G",
                               levels(oa$Natureza))

    oa$um <- 1
    oriandTab <- tapply(oa$um, list(oa$Professor, oa$Natureza), sum)
    oriandTab[is.na(oriandTab)] <- 0

    ordem <- c("O", "IC", "G", "E", "M", "D", "PD")
    faltaCol <- !(levels(oa$Natureza) %in% ordem)
    if(sum(faltaCol)){
        print(levels(oa$Natureza))
        stop(paste0("Tipo de orientação desconhecida: ",
                    levels(oa$Natureza)[faltaCol]))
    }
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

    if(nrow(oriandTab) > 1)
        oriandTab <- oriandTab[ro, ordem]

    # Detalhamento das orientações em andamento
    oa$um <- NULL
    oa <- oa[order(paste(oa$Professor, oa$Instituição, oa$Natureza, oa$Ano)),
             c("Ano", "Professor", "Natureza", "Instituição", "Orientando")]
    oa$Professor <- sub(" .* ", " ", oa$Professor)
    oa$Ano <- as.numeric(as.character(oa$Ano))
} else {
    oriandTab <- data.frame()
}


## Registro do item “Ensino” no período
if(length(ensino)){
    ens <- do.call("rbind", ensino)
    ens <- as.data.frame(ens, stringsAsFactors = FALSE)
    names(ens) <- c("Professor", "Tipo", "MI", "AnoI", "MF", "AnoF")
    ens <- ens[ens$AnoI >= as.character(Inicio) &
               (ens$AnoF <= as.character(Fim) | ens$AnoF == ""), ]
    if(nrow(ens)){
        ens$um <- 1
        ensinoTab <- tapply(ens$um, list(ens$Professor, ens$Tipo), sum)
        ensinoTab[is.na(ensinoTab)] <- 0
        colnames(ensinoTab) <- sub("GRADUACAO", "Graduação", colnames(ensinoTab))
        colnames(ensinoTab) <- sub("POS-", "Pós-", colnames(ensinoTab))
    }
}
if(!exists("ensinoTab"))
        ensinoTab <- data.frame("Atividade de ensino" = "Nenhuma atividade de ensino registrada com início e fim no período")

## Registro do item “Extensão” no período
if(length(extensao)){
    ext <- do.call("rbind", extensao)
    ext <- as.data.frame(ext, stringsAsFactors = FALSE)
    names(ext) <- c("Professor", "Atividade", "MI", "AnoI", "MF", "AnoF")
    ext$Atividade <- sapply(ext$Atividade, html2txt)
    ext <- ext[(ext$AnoI >= as.character(Inicio) & ext$AnoF <= as.character(Fim)) |
               ext$AnoF == "", ]
           if(nrow(ext)){
               extensaoTab <- ext
           } else {
               extensaoTab <- data.frame("Atividade de extensao" = "Nenhuma atividade de extensao registrada com início e fim no período")
           }

           ## Registro do item “Projeto de Extensão” no período
           ext <- do.call("rbind", projext)
           ext <- as.data.frame(ext, stringsAsFactors = FALSE)
           names(ext) <- c("Professor", "Projeto", "MI", "AnoI", "MF", "AnoF")
           ext$Projeto <- sapply(ext$Projeto, html2txt)
           ext <- ext[(ext$AnoI >= as.character(Inicio) & ext$AnoF <= as.character(Fim)) |
                      ext$AnoF == "", ]
                  if(nrow(ext)){
                      projextTab <- ext
                  } else {
                      projextTab <- data.frame("Projeto de extensao" = "Nenhum projeto de extensao registrado com início e fim no período")
                  }
} else {
    extensaoTab <- data.frame("Atividade de extensao" = "Nenhuma atividade de extensao registrada com início e fim no período")
    projextTab <- data.frame("Projeto de extensao" = "Nenhum projeto de extensao registrado com início e fim no período")
}

# Produção bibliográfica (Livros e Artigos)
colnames(pontos) <- c("Classe", "Abreviatura", "Pontos")
pLvr <- data.frame(Professor = rownames(pontuacaoLvr)[1:(nrow(pontuacaoLvr)-1)],
                   Livros = pontuacaoLvr[1:(nrow(pontuacaoLvr)-1), ncol(pontuacaoLvr)],
                   stringsAsFactors = FALSE)
pArt <- data.frame(Professor = rownames(pontuacaoArt)[1:(nrow(pontuacaoArt)-1)],
                   Artigos = pontuacaoArt[1:(nrow(pontuacaoArt)-1), ncol(pontuacaoArt)],
                   stringsAsFactors = FALSE)

# Professores classificados por pontuação ponderada
pond <- merge(pLvr, pArt, all = TRUE, stringsAsFactors = FALSE)
pond$Livros[is.na(pond$Livros)] <- 0
pond$Artigos[is.na(pond$Artigos)] <- 0
pond$Livros <- pond$Livros / max(pond$Livros, na.rm = TRUE)
pond$Artigos <- pond$Artigos / max(pond$Artigos, na.rm = TRUE)
if(PesoLivros > 0){
    pond$Média <- PesoArtigos * pond$Artigos + PesoLivros * pond$Livros
} else {
    pond$Média <- pond$Artigos
}
pond <- pond[order(pond$Média, decreasing = TRUE), ]
rownames(pond) <- as.character(1:nrow(pond))

# Média móvel
# Calcular média móvel geral

# É preciso especificar o ano porque há casos de não registro do ano
pm <- pcompleto[, c("tipo", "pontos", "ano")]

pm$pontos[pm$tipo == "Artigo"] <- 0.7 * pm$pontos[pm$tipo == "Artigo"]
pm$pontos[pm$tipo != "Artigo"] <- 0.3 * pm$pontos[pm$tipo != "Artigo"]
media <- tapply(pm$pontos, pm$ano, function(x) sum(x) / nrow(quando))
mediamovel <- rep(0, max(as.numeric(names(media))) - min(as.numeric(names(media))) + 1)
names(mediamovel) <- as.character(as.numeric(min(names(media))):as.numeric(max(names(media))))
for(n in names(media))
    mediamovel[[n]] <- media[[n]]


mmmsg <- character()
# Calcular média móvel de cada professor
MediaMovel <- function(x)
{
    if(nrow(x) < 3){
        msg <- paste0(x$prof[1], ": não é possível calcular a média móvel porque ")
        if(nrow(x) == 0)
            msg <- paste0(msg, "não há nenhuma publicação registrada.")
        else if(nrow(x) == 1)
            msg <- paste0(msg, "apenas 1 publicação está registrada.\n\n")
        else
            msg <- paste0(msg, "apenas 2 publicações estão registradas.\n\n")
        mmmsg <<- c(mmmsg, msg)
        return(NA)
    }

    sa <- tapply(x$pontos[x$tipo == "Artigo"], x$ano[x$tipo == "Artigo"], sum) * PesoArtigos
    sl <- tapply(x$pontos[x$tipo != "Artigo"], x$ano[x$tipo != "Artigo"], sum) * PesoLivros
    anos <- c(names(sa), names(sl))
    if(max(as.numeric(anos)) - min(as.numeric(anos)) < 3){
        mmmsg <- paste0(x$prof[1], ": não é possível calcular a média móvel porque a produção registrada não se estende por um mínimo de três anos.")
        return(NA)
    }
    m <- numeric()
    for(a in min(as.numeric(anos)):max(as.numeric(anos))){
        ano <- as.character(a)
        m[ano] <- 0
        if(length(grep(ano, names(sa))))
            m[ano] <- m[ano] + PesoArtigos * sa[ano]
        if(length(grep(ano, names(sl))))
            m[ano] <- m[ano] + PesoLivros * sl[ano]
    }

    mm <- rep(0, max(as.numeric(names(m))) - min(as.numeric(names(m))) + 1)
    names(mm) <- as.character(as.numeric(min(names(m))):as.numeric(max(names(m))))
    for(n in names(m))
        mm[[n]] <- m[[n]]
    mml <- length(mm)
    mm1 <- mm[3:mml]
    mm2 <- mm[2:(mml - 1)]
    mm3 <- mm[1:(mml - 2)]
    mm <- (mm1 + mm2 + mm3) / 3
    mm
}
pcl <- split(pcompleto, pcompleto$prof)
mm <- lapply(pcl, MediaMovel)
mm <- mm[!is.na(mm)]


# Produção segundo classificação Qualis
p$um <- 1
producao <- tapply(p$um, list(p$prof, p$qualis), sum)
producao <- producao[, !grepl("Nada", colnames(producao))]

p$producao <- sapply(p$producao, html2txt)
p$livro.ou.periodico <- sapply(p$livro.ou.periodico, html2txt)

# Produção detalhada
b <- p[, c("prof", "producao", "ano", "qualis", "SJR", "SNIP", "livro.ou.periodico", "isxn", "ncoaut")]
b <- b[order(p$prof, p$ano, p$producao), ]

b$prof <- sub(" .* ", " ", b$prof)
b$prof <- sub("^(...................).*", "\\1", b$prof)
b$producao <- sapply(b$producao, html2txt)
b$livro.ou.periodico <- gsub("_", "\\\\_", b$livro.ou.periodico)

bp <- split(b, b$prof)
ObterCapDup <- function(x)
{
    isbnls <- x$isxn[x$qualis == "Lvr"]
    x$capdup <- x$qualis == "Cap" & x$isxn %in% isbnls
    x
}
bp <- lapply(bp, ObterCapDup)
b <- do.call("rbind", bp)
b$erro <- ""
if(sum(b$capdup) > 0){
    b$erro[b$capdup] <- "capdup"
}
b$capdup <- NULL

idx <- ((b$tipo == "Artigo" | b$qualis == "OD") & nchar(b$isxn) != 8) | ((b$tipo != "Artigo" & b$qualis != "OD") & nchar(b$isxn) != 13)
if(sum(idx) > 0){
    b$erro[idx] <- "ncarac"
}

idx <- b$ncoaut > 1
if(sum(idx) > 0){
    b$erro[idx] <- "coautr"
}
b$ncoaut <- NULL

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
    b$erro[idx] <- "ninval"
}


b$producao <- sub("^(................................).*", "\\1", b$producao)
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
    b$erro[idx] <- "duplic"
}
rm(idx, dup, checkISBN)

# b$livro.ou.periodico <- xtable::sanitize(b$livro.ou.periodico)
# b$producao <- xtable::sanitize(b$producao)

levels(b$qualis) <- sub("Nada", " ", levels(b$qualis))
names(b) <- c("Professor", "Produção (títulos truncados)", "Ano", "Qualis", "SJR",
              "SNIP", "Periódico ou Livro (títulos truncados)", "ISSN/ISBN", "erro")
proddet <- b
proddet$SNIP <- round(proddet$SNIP, 3)
rm(b)

# TODO: Produzir tabela com periódicos que mais contribuíram para a pontuação
# do programa

# Títulos de periódicos registrados nos currículos com alguma diferença dos
# títulos na planilha Qualis
ttldif <- p[p$tipo == "Artigo", ]
ttldif <- ttldif[!is.na(ttldif$titulo), ]
ttldif <- ttldif[tolower(ttldif$titulo) != tolower(ttldif$livro.ou.periodico),
       c("titulo", "livro.ou.periodico")]
ttldif <- ttldif[!duplicated(ttldif), ]
colnames(ttldif) <- c("Título Qualis", "Título Lattes")

# Lista de periódicos sem qualis
semqualis <- p[p$qualis == "SQ" | p$qualis == "NP", c("isxn", "livro.ou.periodico")]
semqualis <- semqualis[!duplicated(semqualis), ]
semqualis <- semqualis[order(semqualis$livro.ou.periodico), ]
semqualis$livro.ou.periodico <- sub(" \x26 ", " \x5c\x5c\x26 ", semqualis$livro.ou.periodico)
colnames(semqualis) <- c("ISSN", "Título do periódico")

p$Country <- factor(p$Country)

save(cnpqId, quando, doutor, posdoc, premios, pontuacaoLvr, pontuacaoArt,
     pontuacaoSJR, pontuacaoSNIP, file = "tabs.RData")

cnpqId <- cnpqId[order(cnpqId[, 1]), ]
sink("lattes_xml/ultima_lista.html")
cat('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">\n')
cat('<html xmlns="http://www.w3.org/1999/xhtml">\n')
cat('<head>\n')
cat('   <meta http-equiv="content-type" content="text/html; charset=utf-8" />\n')
cat('   <title>Lista de professores</title>\n')
cat('</head>\n')
cat('<body>\n')
cat('<p><strong>Lista dos currículos incluídos no relatório gerado mais recentemente (links para Lattes no formato XML):</strong></p>\n')
cat('<ol>\n')
for(i in 1:nrow(cnpqId)){
    cat('  <li><a href="http://buscatextual.cnpq.br/buscatextual/download.do?metodo=apresentar&idcnpq=',
        cnpqId[i, 2], '">', cnpqId[i, 1], '</a></li>\n', sep = "")
}
cat('</ol>\n')
cat('</body>\n')
cat('</html>\n')
sink()

# Fonte a ser usada no PDF:
if(!exists("MainFont")){
    if(grepl("linux", version$os)){
        MainFont = "Liberation Serif"
    } else {
        MainFont = "Times New Roman"
    }
}
