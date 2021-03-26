NomeProg <- "Ciência Política e Relações Internacionais"
TituloDoc <- "Produção dos Professores do PPG em XXXX da Universidade YYYY"
NomeComite <- "Ciência Política e Relações Internacionais"
Autor <- "Seu Nome"

# Pontuação para Qualis do Triênio 2010-2012
PontosQualis10 <- data.frame(qualis = c("A1", "A2", "B1", "B2", "B3", "B4", "B5", "C", "SQ", "OD", "Lvr", "Org", "Cap"),
                             pontos = c(100,   90,   70,    0,    0,    0,    0,   0,    0,    0,    70,    70,    15))

# Pontuação para Qualis do Quadriênio 2013-2016
PontosQualis13 <- data.frame(qualis = c("A1", "A2", "B1", "B2", "B3", "B4", "B5", "C", "SQ", "OD", "Lvr", "Org", "Cap"),
                             pontos = c(100,   90,   70,    0,    0,    0,    0,   0,    0,    0,    70,    70,    15))

# Pontuação para Qualis do Quadriênio 2017-2020
PontosQualis17 <- data.frame(qualis = c("A1", "A2", "A3", "A4", "B1", "B2", "B3", "B4", "C", "SQ", "OD", "Lvr", "Org", "Cap"),
                             pontos = c(100,   90,   70,   60,    0,    0,    0,    0,    0,   0,    0,    70,    70,    15))

# Período de aplicação de cada Qualis
QualQualis <- rbind("Q10" = c("ini" = 1900, "fim" = 2012), "Q13" = c(2013, 2016), "Q17" = c(2017, 2099))

# Procurar Qualis por título exato se não encontrar por ISSN?
QualisPorTitulo <- FALSE

# Ordenar tabela com número de orientandos por nome?
OrdenarOrientacaoPorNome <- FALSE

# Pesos para cálculo da média ponderada
PesoArtigos <- 0.65
PesoLivros <- 0.35

# Especificar período do relatório
Inicio <- 2017
Fim <- 2020
