NomeProg <- "Ciência Política e Relações Internacionais"
TituloDoc <- "Produção dos Professores do PPG em XXXX da Universidade YYYY"
NomeComite <- "Ciência Política e Relações Internacionais"
Autor <- "Seu Nome"

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

PontosQualis <- c("A1"  = 100,
                  "A2"  =  85,
                  "A3"  =  70,
                  "A4"  =  55,
                  "B1"  =   0,
                  "B2"  =   0,
                  "B3"  =   0,
                  "B4"  =   0,
                  "C"   =   0,
                  "NP"  =   0,
                  "OD"  =   0,
                  "Lvr" =  70,
                  "Org" =  70,
                  "Cap" =  15)

# Procurar Qualis por título exato se não encontrar por ISSN?
QualisPorTitulo <- FALSE

# Pesos para cálculo da média ponderada
PesoArtigos <- 0.7
PesoLivros <- 0.3

# Especificar período do relatório
Inicio <- 2017
Fim <- 2020

#QualisXLS <- "qualis/classificacoes_publicadas_sociologia_2017_1496941696361.xls"

# Modificar peso do SJR conforme escopo do periódico
pesos <- list("0.65" = c("Management, Monitoring, Policy and Law",
                         "Marketing",
                         "Modeling and Simulation"),
              "0.70" = c("Archeology",
                         "Archeology (arts and humanities)",
                         "Arts and Humanities (miscellaneous)",
                         "Arts and Humanities (all)",
                         "Cognitive Neuroscience",
                         "Behavioral Neuroscience",
                         "Decision Sciences (miscellaneous)",
                         "Decision Sciences (all)",
                         "Demography",
                         "Ecology, Evolution, Behavior and Systematics",
                         "Health Policy",
                         "Issues, Ethics and Legal Aspects",
                         "Law",
                         "Leadership and Management",
                         "Organizational Behavior and Human Resource Management",
                         "Public Health, Environmental and Occupational Health",
                         "Statistics and Probability",
                         "Statistics, Probability and Uncertainty"),
              "0.75" = c("Communication",
                         "Cultural Studies",
                         "Education",
                         "Gender Studies",
                         "Public Administration",
                         "Urban Studies"),
              "0.80" = c("Applied Psychology",
                         "Economics and Econometrics",
                         "Economics, Econometrics and Finance (all)",
                         "Economics, Econometrics and Finance (miscellaneous)",
                         "Experimental and Cognitive Psychology",
                         "Psychology (all)",
                         "Psychology (miscellaneous)"),
              "0.85" = c("History and Philosophy of Science",
                         "Philosophy",
                         "Social Psychology",
                         "Social Work"),
              "0.90" = c("History"),
              "0.95" = c("Anthropology",
                         "Public Administration",
                         "Political Science and International Relations",
                         "Social Sciences (all)",
                         "Social Sciences (miscellaneous)"),
              "1.00" = "Sociology and Political Science")

if(exists("snip.cat")){
    snip.cat$Peso <- 0.40
    sjr.cat$Peso  <- 0.40
    for(n in names(pesos)){
        sjr.cat$Peso[sjr.cat$Categoria %in% pesos[[n]]] <- as.numeric(n)
        snip.cat$Peso[snip.cat$Categoria %in% pesos[[n]]] <- as.numeric(n)
    }
}
