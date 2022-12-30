
QualisLattes.pdf: QualisLattes.Rnw gerar_tabelas.R curriculos/*.*
	echo "library(knitr) ; knit('QualisLattes.Rnw')" | R --no-save
	latexmk -pdf -pdflatex="xelatex %O %S" QualisLattes

