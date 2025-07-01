# PontuarLattes

> [!Note]
> A Plataforma Lattes não disponibiliza mais o link para download da versão
> XML do currículo e o PontuarLattes não foi programado para extrair
> informações da página HTML.

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

  1. Instale o R e, no R, instale os pacotes `XML`, `openxlsx`, `cld2` e `ineq`.

  2. Instale o texlive ou outro sistema para LaTeX. Certifique-se de ser capaz
     de compilar um documento usando o XeLaTeX.

  3. Acesse o [Scimago Journal Ranking](https://www.scimagojr.com/journalrank.php)
     e faça o download da base de dados para a pasta `auxiliar`. O nome do
     arquivo deve ser `scimagojr 2019.csv`

  4. Baixe o arquivo [CWTS Journal Indicators May 2020.xlsx](https://www.journalindicators.com/Content/CWTS%20Journal%20Indicators%20April%202020.xlsx)
     e salve-o na pasta `auxiliar`.

  5. Copie o arquivo `exemplo/info.R` para a pasta inicial do PontuarLattes,
     ou seja, a mesma pasta onde encontra-se o arquivo `QualisLattes.Rnw`.

  6. Edite o código do `info.R`.

  7. Na pasta `auxiliar` execute os scripts: `scielo_01.sh`,
     `auxiliar/scielo_02.R` e `SJR_SNIP.R`.

  8. Na pasta `qualis` execute os scripts: `qualis_2010_2012.R`,
     `qualis_2013_2016.R` e `qualis_2017_2020.R`.

  9. Na pasta principal do PontuarLattes, crie uma pasta chamada `curriculos`.

  10. Baixe os currículos no formato xml (zipados e, portanto, com extensão
      `.zip`) e salve-os na pasta `lattes_xml`. Dica: o link para o currículo
      no formato xml fica no canto superior direito do currículo Lattes.

  11. Gere o PDF a partir do `QualisLattes.Rnw` (use XeLaTeX e não pdfLaTeX)
      ou, se preferir, use o Makefile.

  12. Quando quiser atualizar o relatório, basta repetir os passos 10 e 11.

Em caso de dúvida, peça ajuda a alguém que saiba programar em R. Se você tem
familiaridade com R e encontrou algum erro nestas instruções, por favor, me
avise.
