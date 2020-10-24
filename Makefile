
QualisLattes.pdf: QualisLattes.Rnw gerar_tabelas.R lattes_xml/*.zip
	echo "library(knitr) ; knit('QualisLattes.Rnw')" | R --no-save
	latexmk -pdf -pdflatex="xelatex %O %S" QualisLattes

