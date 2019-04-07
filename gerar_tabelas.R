library("XML")
# library("scholar")

source("info.R")

# Usar issn1 e issn2 do scielo para fazer equivalência na tabela Qualis
sci <- read.delim("scielo_issn.tsv", stringsAsFactors = FALSE)
for(i in 1:nrow(sci)){
    if(!is.na(sci$issn2[i]) && sci$issn2[i] %in% qualis$issxn && ! sci$issn1[i] %in% qualis$issxn){
        qualis <- rbind(qualis,
                        data.frame(isxn = sci$issn1[i],
                                   titulo = qualis$titulo[qualis$isxn == sci$issn2[i]],
                                   qualis = qualis$qualis[qualis$isxn == sci$issn2[i]],
                                   stringsAsFactors = FALSE))
    } else {
        if(!is.na(sci$issn2[i]) && sci$issn1[i] %in% qualis$isxn && ! sci$issn2[i] %in% qualis$isxn){
            qualis <- rbind(qualis,
                            data.frame(isxn = sci$issn2[i],
                                       titulo = qualis$titulo[qualis$isxn == sci$issn1[i]],
                                       qualis = qualis$qualis[qualis$isxn == sci$issn1[i]],
                                       stringsAsFactors = FALSE))
        }
    }
}


siglas <- read.delim("siglas_univ.txt", comment.char = "#", stringsAsFactors = FALSE)

NomeSigla <- function(x)
{
    for(i in 1:nrow(siglas))
        x <- sub(paste0("^", siglas$nome[i], "$"), siglas$sigla[i], x)
    x
}


# Ler currículos

doutorado <- list()
posdoc <- list()
premios <- list()
oriand <- list()
oriconc <- list()
ensino <- list()
nlist <- list()
cnpqId <- matrix(character(), ncol = 2, nrow = 0)
colnames(cnpqId) <- c("Professor", "id")

obter.producao <- function(arquivo)
{
    unzip(paste0("lattes_xml/", arquivo), exdir = "/tmp/")
    xl <- xmlTreeParse("/tmp/curriculo.xml", encoding = "latin1")
    xl <- xl$doc$children$`CURRICULO-VITAE`
    prof <- xl$children$`DADOS-GERAIS`
    nomep <- prof$attributes[["NOME-COMPLETO"]]
    cnpqId <<- rbind(cnpqId, c(nomep, xl$attributes[["NUMERO-IDENTIFICADOR"]]))

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
            res <- c(prof, db[["ANO-DO-ARTIGO"]], "NãoArt",
                     db[["TITULO-DO-ARTIGO"]],
                     dl[["TITULO-DO-PERIODICO-OU-REVISTA"]],
                     dl[["ISSN"]])
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

zipls <- dir("lattes_xml", pattern = "*.zip")
if(length(zipls) == 0)
    stop("Nenhum currículo encontrado na pasta 'lattes_xml'")
xx <- lapply(zipls, obter.producao)
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

    # Usar issn1 e issn2 do scielo para fazer equivalência na tabela do SJR
    for(i in 1:nrow(sci)){
        if(!is.na(sci$issn2[i]) && sci$issn2[i] %in% s$issxn && ! sci$issn1[i] %in% s$issxn){
            s <- rbind(s,
                       data.frame(isxn = sci$issn1[i],
                                  title = s$titulo[s$isxn == sci$issn2[i]],
                                  SJR = s$SJR[s$isxn == sci$issn2[i]],
                                  country = s$country[s$isxn == sci$issn2[i]],
                                  stringsAsFactors = FALSE))
        } else {
            if(!is.na(sci$issn2[i]) && sci$issn1[i] %in% s$isxn && ! sci$issn2[i] %in% s$isxn){
                s <- rbind(s,
                           data.frame(isxn = sci$issn2[i],
                                      title = s$title[s$isxn == sci$issn1[i]],
                                      SJR = s$SJR[s$isxn == sci$issn1[i]],
                                      country = s$country[s$isxn == sci$issn1[i]],
                                      stringsAsFactors = FALSE))
            }
        }
    }

    p <- merge(p, s, all.x = TRUE, stringsAsFactors = FALSE)
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

if(file.exists("SNIP.RData")){
    load("SNIP.RData")
} else {
    # Obter SNIP de http://www.journalindicators.com/methodology#sthash.FN5cRgxb.dpuf%20
    if(!file.exists("qualis/CWTS Journal Indicators May 2018.xlsx")){
        cat("Baixando o CWTS Journal Indicators\n")
        download.file("http://www.journalindicators.com/Content/CWTS%20Journal%20Indicators%20May%202018.xlsx",
                      destfile = "qualis/CWTS Journal Indicators May 2018.xlsx")
    }
    if(!file.exists("qualis/CWTS Journal Indicators May 2018.xlsx"))
        stop("Arquivo 'qualis/CWTS Journal Indicators May 2018.xlsx' não encontrado.")

    library("openxlsx")
    # TODO: usar área de conhecimento.
    afield <- read.xlsx("qualis/CWTS Journal Indicators May 2018.xlsx", 2)
    snip1 <- read.xlsx("qualis/CWTS Journal Indicators May 2018.xlsx", 1)
    snip2 <- snip1
    names(snip1) <- sub("Print.ISSN", "isxn", names(snip1))
    names(snip2) <- sub("Electronic.ISSN", "isxn", names(snip2))
    snip1$Electronic.ISSN <- NULL
    snip2$Print.ISSN <- NULL
    snip <- rbind(snip1, snip2)
    snip <- snip[snip$Year == 2017 & snip$isxn != "-", c("Source.title", "isxn", "SNIP")]

    # Existem ISSNs duplicados (atribuídos a mais de um periódico):
    # sum(duplicated(snip$isxn))
    # # Exemplo:
    # snip[snip$isxn == "1432-2218", 1:4]

    snip <- snip[!duplicated(snip$isxn), ]

    snip$isxn <- sub("-", "", snip$isxn)
    save(snip, file = "SNIP.RData")
}

p <- merge(p, snip, all.x = TRUE, stringsAsFactors = FALSE)
p <- merge(p, qualis, all.x = TRUE, stringsAsFactors = FALSE)

p$qualis[is.na(p$qualis) & p$tipo == "Artigo"] <- "SQ"
p$qualis[is.na(p$qualis) & p$tipo != "Artigo"] <- p$tipo[is.na(p$qualis) & p$tipo != "Artigo"]

# Organização de dossiês em periódicos:
idx <- p$tipo != "Artigo" & p$tipo != "NãoArt" & p$qualis %in% c("A1", "A2", "B1", "B2", "B3", "B4", "B5", "C")
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
doutor[, 2] <- NomeSigla(doutor[, 2])
colnames(doutor) <- c("Professor", "Instituição doutorado", "Nome do curso", "Ano", "DataCV")
# Reter somente último doutorado concluído:
doutor <- doutor[order(doutor[, "Ano"], decreasing = TRUE), ]
notdup <- !duplicated(doutor[, "Professor"])
doutor <- doutor[notdup, ]

quando <- as.Date(doutor[, "DataCV"], format = "%d/%m/%Y")
doutor <- doutor[order(quando), ]
rm(quando)

p$prof <- factor(p$prof)

TabProd <- function(d, v)
{
    tab <- tapply(d[[v]], list(d$prof, d$ano), sum, na.rm = TRUE)

    # Adicionar professores que não produziram no período
    falta <- !(doutor[, "Professor"] %in% rownames(tab))
    if(sum(falta) > 0){
        falta <- doutor[falta, "Professor"]
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
    tab <- rbind(tab, "Média" = apply(tab, 2, function(x)
                                       round(sum(x, na.rm = TRUE) / length(x), 1)))

    tab
}

pontuacaoLvr <- TabProd(p[p$tipo %in% c("Lvr", "Cap", "Org"), ], "pontos")
pontuacaoArt <- TabProd(p[p$tipo == "Artigo", ], "pontos")

pontuacaoSNIP <- TabProd(p[p$tipo == "Artigo", ], "SNIP")

if(is.null(p$SJR)){
    pontuacaoSJR <- matrix("Arquivo `qualis/scimagojr_2017_TAB.csv' não encontrado", 1, 1)
    p$SJR <- NA
} else {
    pontuacaoSJR <- TabProd(p[p$tipo == "Artigo", ], "SJR")
}

# pontuacaoGgl <- TabProd(p[p$tipo == "Artigo", ], "ImpactFactor")


# Lista de Pós-doutorados realizados
posdoc <- do.call("rbind", posdoc)
if(is.null(posdoc)){
    posdoc <- matrix(NA, ncol = 4)
} else {
    posdoc[, 2] <- NomeSigla(posdoc[, 2])
    posdoc <- posdoc[order(posdoc[, 4]), ]
}
colnames(posdoc) <- c("Professor", "Instituição", "Início", "Fim")

# Orientações concluídas
oc <- do.call("rbind", oriconc)
colnames(oc) <- c("Professor", "Natureza", "Ano", "Instituição", "Curso", "Orientado")
oc[, 4] <- NomeSigla(oc[, 4])
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
oa[, 5] <- NomeSigla(oa[, 5])
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
levels(oa$Natureza) <- sub("Trabalho de conclusão de curso de graduação", "G", oa$Natureza)

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
media <- tapply(pm$pontos, pm$ano, function(x) sum(x) / nrow(doutor))
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
