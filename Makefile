
QualisLattes.html: QualisLattes.md template.html gerar_tabelas.R info.R lattes_xml/*.zip
	echo "rmarkdown::render('QualisLattes.md')" | R --no-save

QualisLattes.pdf: QualisLattes.Rnw gerar_tabelas.R info.R lattes_xml/*.zip
	echo "library(knitr) ; knit('QualisLattes.Rnw')" | R --no-save
	latexmk -pdf -pdflatex="xelatex %O %S" QualisLattes

QualisLattes.md: QualisLattes.Rmd gerar_tabelas.R info.R lattes_xml/*.zip
	echo "knitr::knit('QualisLattes.Rmd')" | R --no-save

