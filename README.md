# PontuarLattes

O processamento do arquivo `QualisLattes.Rnw` gera um PDF com tabelas da
produção bibliográfica extraída de currículos Lattes selecionados. Artigos
publicados em periódicos são pontuados conforme a classificação Qualis dos
periódicos.

O código de `QualisLattes.Rnw` é genérico e pode ser usado para calcular a
pontuação de qualquer programa de pós-graduação de qualquer área, sendo,
entretanto, necessário baixar os currículos Lattes da Plataforma Lattes,
baixar a pontuação Qualis da Plataforma Sucupira e ajustar o conteúdo dos
arquivos `info.tex` e `info.R`, conforme instruções detalhadas a seguir.

## Instruções de uso para Linux e OS X

  1. Crie um diretório fora do diretório `PontuarLattes`.

  2. Nesse novo diretório, crie um link simbólico para o arquivo
     `QualisLattes.Rnw`. Se não puder criar um link simbólico, pode copiar o
     arquivo, mas, neste caso, você terá que copiá-lo novamente sempre que ele
     for atualizado no repositório do github.

  3. Copie os arquivos `exemplo/info.R` e `exemplo/info.tex` para o novo
     diretório. Não faça links simbólicos desses arquivos porque eles precisam
     ser editados com informações específicas de cada programa.

  4. No novo diretório, crie os diretórios `qualis` e `lattes_xml`.

  5. Copie o arquivo `exemplo/lattes_xml/lista_de_professores.html` para o seu
     novo diretório `lattes_xml`.

  6. Edite o seu arquivo `lattes_xml/lista_de_professores.html`. O arquivo
     possui uma única linha com link para um currículo Lattes na versão XML.
     Faça cópias dessa linha e substitua os dados pelos nomes e links para os
     Lattes dos professores do programa de pós-graduação para o qual será
     produzido o relatório. Depois, abra o arquivo em um navegador e baixe os
     currículos. Mova todos arquivos `.zip` baixados para o seu diretório
     `lattes_xml`.

  7. Acesse a página com a pontuação Qualis da Plataforma Sucupira, selecione
     apenas um *Evento de Classificação* e uma *Área de Avaliação*, deixando
     todos os demais campos em branco. Clique no botão *Consultar*. Deverá
     aparecer um link para um arquivo com extensão `.xls`. Salve os arquivos
     `.xls` de cada *Evento de Classificação* no seu diretório `qualis`. 

  8. Edite as informações do arquivo `info.tex` e o código do `info.R`.

  9. Gere o PDF a partir do link simbólico `QualisLattes.Rnw` (use XeLaTeX e não
     pdfLaTeX).

  10. Em caso de dúvida, peça ajuda a alguém que saiba programar em R e em
      LaTeX, e que esteja acostumado a gerar relatórios Rnoweb.

  11. Se encontrar algum erro nestas instruções, me avise.

## Instruções de uso para Windows

  1. Tente seguir as intruções para Linux e OS X.

  2. Me avise se precisar fazer algo diferente para que eu melhore estas
     instruções. Por exemplo, não sei se existe link simbólico no Windows.
