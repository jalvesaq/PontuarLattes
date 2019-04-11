NomeComite <- "Sociologia"
NomeProg <- "Sociologia"
TituloDoc <- "Produção dos Professores do Programa de Pós-Graduação em Sociologia"
Autor <- "PPGS"

PontosQualis <- c("A1"  = 100,
                  "A2"  =  85,
                  "B1"  =  70,
                  "B2"  =   0,
                  "B3"  =   0,
                  "B4"  =   0,
                  "B5"  =   0,
                  "C"   =   0,
                  "SQ"  =   0,
                  "OD"  =   0,
                  "Lvr" =  70,
                  "Org" =  70,
                  "Cap" =  15)

# Pesos para cálculo da média ponderada
PesoArtigos <- 0.7
PesoLivros <- 0.3

# Especificar período do relatório
Inicio <- 2017
Fim <- 2019

QualisXLS <- "qualis/classificacoes_publicadas_sociologia_2017_1496941696361.xls"
