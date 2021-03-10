
QualisLattes.pdf: QualisLattes.Rnw gerar_tabelas.R info.R lattes_xml/*.*
	echo "library(knitr) ; knit('QualisLattes.Rnw')" | R --no-save
	latexmk -pdf -pdflatex="xelatex %O %S" QualisLattes

