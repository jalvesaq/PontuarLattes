NomeProg <- "Ciência Política e Relações Internacionais"
TituloDoc <- "Produção dos Professores do PPG em Ciência Política e Relações Internacionais --- UFPE"
NomeComite <- "Ciência Política e Relações Internacionais"
Autor <- "\\begin{tabular}{ccc}\\\\\nJakson Aquino & ~~~~ & Dalson Figueiredo Filho\\\\\n{\\footnotesize Departamento de Ciências Sociais, UFC} & & {\\footnotesize Departamento de Ciência Política, UFPE}\\\\\n{\\footnotesize jaa@ufc.br} & & {\\footnotesize dalsonbritto@yahoo.com.br}\n\\end{tabular}"

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
                  "Lvr" = 1.5,
                  "Org" = 1.0,
                  "Cap" = 0.5)

# Pesos para cálculo da média ponderada
PesoArtigos <- 0.7
PesoLivros <- 0.3

# Especificar período do relatório
Inicio <- 2017
Fim <- 2019

# Modificar peso do SJR e do SNIP conforme escopo do periódico
pesos <- list("0.65" = c("Education",
                         "Management, Monitoring, Policy and Law",
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
                         "Gender Studies",
                         "Public Administration",
                         "Urban Studies"),
              "0.80" = c("Applied Psychology",
                         "Economics and Econometrics",
                         "Economics, Econometrics and Finance (all)",
                         "Economics, Econometrics and Finance (miscellaneous)",
                         "Experimental and Cognitive Psychology",
                         "History and Philosophy of Science",
                         "Philosophy",
                         "Psychology (all)",
                         "Psychology (miscellaneous)"),
              "0.85" = c("Social Psychology",
                         "Social Work"),
              "0.90" = c("Anthropology",
                         "History",
                         "Public Administration",
                         "Social Sciences (all)",
                         "Social Sciences (miscellaneous)"),
              "0.95" = "Sociology and Political Science",
              "1.00" = "Political Science and International Relations")

snip.cat$Peso <- 0.50
sjr.cat$Peso  <- 0.50
for(n in names(pesos)){
    sjr.cat$Peso[sjr.cat$Categoria %in% pesos[[n]]] <- as.numeric(n)
    snip.cat$Peso[snip.cat$Categoria %in% pesos[[n]]] <- as.numeric(n)
}
