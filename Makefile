
QualisLattes.pdf: QualisLattes.Rnw gerar_tabelas.R info.R lattes_xml/*.*
	echo "library(knitr) ; knit('QualisLattes.Rnw')" | R --no-save
	latexmk -pdf -pdflatex="xelatex %O %S" QualisLattes

QualisLattes.html: QualisLattes.md template.html
	echo "rmarkdown::render('QualisLattes.md')" | R --no-save

QualisLattes.md: QualisLattes.Rmd gerar_tabelas.R info.R lattes_xml/*.*
	echo "knitr::knit('QualisLattes.Rmd')" | R --no-save

