areas <- c("Administração Pública e de Empresas Ciências Contábeis e Turismo" = "classificacoes_publicadas_administracao_publica_e_de_empresas_ciencias_contabeis_e_turismo_2017_1496941692100.xls",
           "Antropologia / Arqueologia" = "classificacoes_publicadas_antropologia_arqueologia_2017_1496941692387.xls",
           "Arquitetura Urbanismo e Design" = "classificacoes_publicadas_arquitetura_urbanismo_e_design_2017_1503422061875.xls",
           "Artes Música" = "classificacoes_publicadas_artes_musica_2017_1496941692545.xls",
           "Astronomia / Física" = "classificacoes_publicadas_astronomia_fisica_2017_1496941692618.xls",
           "Biodiversidade" = "classificacoes_publicadas_biodiversidade_2017_1496941692693.xls",
           "Biotecnologia" = "classificacoes_publicadas_biotecnologia_2017_1496941692806.xls",
           "Ciência da Computação" = "classificacoes_publicadas_ciencia_da_computacao_2017_1496941692902.xls",
           "Ciência de Alimentos" = "classificacoes_publicadas_ciencia_de_alimentos_2017_1496941692970.xls",
           "Ciência Política e Relações Internacionais" = "classificacoes_publicadas_ciencia_politica_e_relacoes_internacionais_2017_1503422062321.xls",
           "Ciências Agrarias I" = "classificacoes_publicadas_ciencias_agrarias_i_2017-2018_1522078261448.xls",
           "Ciências Ambientais" = "classificacoes_publicadas_ciencias_ambientais_2017_1496941693222.xls",
           "Ciências Biológicas I" = "classificacoes_publicadas_ciencias_biologicas_i_2017_1496941693320.xls",
           "Ciências Biológicas II" = "classificacoes_publicadas_ciencias_biologicas_ii_2017_1496941693427.xls",
           "Ciências Biológicas III" = "classificacoes_publicadas_ciencias_biologicas_iii_2017_1496941693523.xls",
           "Ciências da Religião e Teologia" = "classificacoes_publicadas_ciencias_da_religiao_e_teologia_2017-2018_1521030263970.xls",
           "Comunicação e Informação" = "classificacoes_publicadas_comunicacao_e_informacao_2017_1496941693687.xls",
           "Direito" = "classificacoes_publicadas_direito_2017_1496941693777.xls",
           "Economia" = "classificacoes_publicadas_economia_2017_1496941693861.xls",
           "Educação" = "classificacoes_publicadas_educacao_2017_1498744438098.xls",
           "Educação Física" = "classificacoes_publicadas_educacao_fisica_2017_1496941693943.xls",
           "Enfermagem" = "classificacoes_publicadas_enfermagem_2017_1496941694042.xls",
           "Engenharias I" = "classificacoes_publicadas_engenharias_i_2017_1496941694145.xls",
           "Engenharias II" = "classificacoes_publicadas_engenharias_ii_2017_1496941694245.xls",
           "Engenharias III" = "classificacoes_publicadas_engenharias_iii_2017_1496941694387.xls",
           "Engenharias IV" = "classificacoes_publicadas_engenharias_iv_2017_1496941694543.xls",
           "Ensino" = "classificacoes_publicadas_ensino_2017_1496941694663.xls",
           "Farmácia" = "classificacoes_publicadas_farmacia_2017_1496941694770.xls",
           "Filosofia" = "classificacoes_publicadas_filosofia_2017_1496941694878.xls",
           "Geociências" = "classificacoes_publicadas_geociencias_2017-2018_1521030264117.xls",
           "Geografia" = "classificacoes_publicadas_geografia_2017_1496941695043.xls",
           "História" = "classificacoes_publicadas_historia_2017_1496941695130.xls",
           "Interdisciplinar" = "classificacoes_publicadas_interdisciplinar_2017_1503422062442.xls",
           "Linguística e Literatura" = "classificacoes_publicadas_letras_linguistica_2017_1496941695218.xls",
           "Matemática / Probabilidade e Estatística" = "classificacoes_publicadas_matematica_probabilidade_e_estatistica_2017_1496941695329.xls",
           "Materiais" = "classificacoes_publicadas_materiais_2017_1496941695405.xls",
           "Medicina I" = "classificacoes_publicadas_medicina_i_2017_1496941695476.xls",
           "Medicina II" = "classificacoes_publicadas_medicina_ii_2017_1496941695569.xls",
           "Medicina III" = "classificacoes_publicadas_medicina_iii_2017_1496941695680.xls",
           "Medicina Veterinaria" = "classificacoes_publicadas_medicina_veterinaria_2017_1496941695742.xls",
           "Nutrição" = "classificacoes_publicadas_nutricao_2017_1496941695818.xls",
           "Odontologia" = "classificacoes_publicadas_odontologia_2017_1498506499076.xls",
           "Planejamento Urbano e Regional / Demografia" = "classificacoes_publicadas_planejamento_urbano_e_regional_demografia_2017_1496941695959.xls",
           "Psicologia" = "classificacoes_publicadas_psicologia_2017_1503422062662.xls",
           "Química" = "classificacoes_publicadas_quimica_2017_1496941696117.xls",
           "Saúde Coletiva" = "classificacoes_publicadas_saude_coletiva_2017_1496941696195.xls",
           "Serviço Social" = "classificacoes_publicadas_servico_social_2017_1496941696297.xls",
           "Sociologia" = "classificacoes_publicadas_sociologia_2017_1496941696361.xls",
           "Zootecnia / Recursos Pesqueiros" = "classificacoes_publicadas_zootecnia_recursos_pesqueiros_2017_1496941696439.xls")

load("../SJR_SNIP.RData")
for(n in names(areas)){
    # Carregar Qualis
    QualisXLS = areas[n]
    qualis <- read.table(QualisXLS,
                         stringsAsFactors = FALSE, header = TRUE,
                         fileEncoding = "Windows-1252", sep = "\t")
    names(qualis) <- c("isxn", "titulo", "qualis")

    # Correções
    qualis$isxn <- sub("-", "", qualis$isxn)
    qualis$qualis <- sub(" *$", "", qualis$qualis)
    qualis$titulo <- sub(" *$", "", qualis$titulo)

    # Usar issn1 e issn2 do Scielo, do SJR e do SNIP para fazer equivalência na tabela Qualis
    for(i in 1:nrow(issn)){
        if(!is.na(issn$issn2[i]) && issn$issn2[i] %in% qualis$issxn && ! issn$issn1[i] %in% qualis$issxn){
            qualis <- rbind(qualis,
                            data.frame(isxn = issn$issn1[i],
                                       titulo = qualis$titulo[qualis$isxn == issn$issn2[i]],
                                       qualis = qualis$qualis[qualis$isxn == issn$issn2[i]],
                                       stringsAsFactors = FALSE))
        } else {
            if(!is.na(issn$issn2[i]) && issn$issn1[i] %in% qualis$isxn && ! issn$issn2[i] %in% qualis$isxn){
                qualis <- rbind(qualis,
                                data.frame(isxn = issn$issn2[i],
                                           titulo = qualis$titulo[qualis$isxn == issn$issn1[i]],
                                           qualis = qualis$qualis[qualis$isxn == issn$issn1[i]],
                                           stringsAsFactors = FALSE))
            }
        }
    }
    qualis <- qualis[!duplicated(qualis$isxn), ]
    save(qualis, QualisXLS, file = paste0(gsub(" ", "_", gsub(" / ", " ", n)), ".RData"))
}
