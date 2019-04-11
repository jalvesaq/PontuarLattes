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

  1. Instale o R e, no R, instale os pacotes `XML`, `openxlsx`, `xtable` e
     `ineq`.

  2. Instale o texlive ou outro sistema para LaTeX. Certifique-se de ser capaz
     de compilar um documento usando o XeLaTeX.

  3. Baixe os currículos no formato xml (zipados e, portanto, com extensão
     `.zip`) e salve-os na pasta qualis. Dica: o link para o currículo no
     formato xml fica no canto superior direito do currículo Lattes.

  4. Acesse a página com a pontuação Qualis da Plataforma Sucupira, selecione
     apenas um *Evento de Classificação* e uma *Área de Avaliação*, deixando
     todos os demais campos em branco. Clique no botão *Consultar*. Deverá
     aparecer um link para um arquivo com extensão `.xls`. Salve o arquivo
     `.xls` do último *Evento de Classificação* na pasta `qualis`.

  6. Acesse o [Scimago Journal Ranking](https://www.scimagojr.com/journalrank.php)
     e faça o download da base de dados para a pasta `auxiliar`. O nome do
     arquivo deve ser `scimagojr 2017.csv`

  7. Baixe o arquivo [CWTS Journal Indicators May 2018.xlsx](http://www.journalindicators.com/Content/CWTS%20Journal%20Indicators%20May%202018.xlsx)
     e salve-o na pasta `auxiliar`.

  8. Copie o arquivo `exemplo/info.R` para a pasta inicial do PontuarLattes,
     ou seja, a mesma pasta onde encontra-se o arquivo `QualisLattes.Rnw`.

  9. Edite o código do `info.R`.

  10. Se não tiver conseguido instalar o XeLaTeX ou se quiser apenas ver as
     tabelas no próprio R, rode o script `gerar_tabelas.R` e execute o comando
     `print(obj)` para cada uma das tabelas, substituindo `obj` pelo nome do
     objeto correspondente à tabela.

  11. Gere o PDF a partir do `QualisLattes.Rnw` (use XeLaTeX e não pdfLaTeX)
      ou, se preferir, use o Makefile.

  12. Em caso de dúvida, peça ajuda a alguém que saiba programar em R e em
      LaTeX, e que esteja acostumado a gerar relatórios Rnoweb.

  13. Se você tem familiaridade com R e LaTeX e encontrou algum erro nestas
      instruções, por favor, me avise.
