# PontuarLattes

O processamento do arquivo `QualisLattes.Rnw` gera um PDF com tabelas com
informações sobre os professores de um programa de pós-graduação, incluindo a
produção bibliográfica extraída de currículos Lattes selecionados. Artigos
publicados em periódicos são pontuados de três formas diferentes: pela
classificação Qualis dos periódicos e pelos fatores de impacto SJR e SNIP.

O código de `QualisLattes.Rnw` é genérico e pode ser usado para calcular a
pontuação de qualquer programa de pós-graduação de qualquer área, sendo,
entretanto, necessário baixar os currículos Lattes da Plataforma Lattes,
baixar a pontuação Qualis da Plataforma Sucupira e ajustar o conteúdo do
arquivo `info.R`, conforme instruções detalhadas a seguir.

O código deste repositório foi testado somente no Linux.

## Instruções de uso

  1. Instale o R e, no R, instale os pacotes `XML`, `openxlsx`, e `ineq`.

  2. Acesse o [Scimago Journal Ranking](https://www.scimagojr.com/journalrank.php)
     e faça o download da base de dados para a pasta `auxiliar`. O nome do
     arquivo deve ser `scimagojr 2018.csv`

  3. Baixe o arquivo [CWTS Journal Indicators May 2019.xlsx](http://www.journalindicators.com/Content/CWTS%20Journal%20Indicators%20May%202019.xlsx)
     e salve-o na pasta `auxiliar`.

  4. Copie o arquivo `exemplo/info.R` para a pasta inicial do PontuarLattes,
     ou seja, a mesma pasta onde encontra-se o arquivo `QualisLattes.Rnw`.

  5. Edite o código do `info.R`.

  6. Na pasta `auxiliar` execute os scripts: `scielo_01.sh`,
     `auxiliar/scielo_02.R` e `SJR_SNIP.R`.

  7. Na pasta `qualis` execute os scripts: `qualis_2013_2016.R` e
     `qualis_2017_2020.R`.

  8. Na pasta principal do PontuarLattes, crie uma pasta chamada `lattes_xml`.

  9. Baixe os currículos no formato xml (zipados e, portanto, com extensão
      `.zip`) e salve-os na pasta `lattes_xml`. Dica: o link para o currículo
      no formato xml fica no canto superior direito do currículo Lattes.

  11. Gere o relatório no formato HTML a partir do `QualisLattes.Rmd`
      ou, se preferir, use o Makefile.

  12. Quando quiser atualizar o relatório, basta repetir os passos 10, 11 e 12.

Em caso de dúvida, peça ajuda a alguém que saiba programar em R. Se você tem
familiaridade com R e encontrou algum erro nestas instruções, por favor, me
avise.

Se você tiver familiaridade com LaTeX, poderá também gerar o relatório em
versão PDF. Para isso, siga as mesmas instruções acima e, então:

  1. Instale o texlive ou outro sistema para LaTeX. Certifique-se de ser capaz
     de compilar um documento usando o XeLaTeX.

  2. Gere o PDF a partir do `QualisLattes.Rnw` (use XeLaTeX e não pdfLaTeX)
      ou, se preferir, use o Makefile.

