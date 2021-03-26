
mv "Ciênciada Computação.html" "Ciência da Computação.html"
mv "Engenharia de Materiais e Metalurgica.html" "Engenharia de Materiais e Metalúrgica.html"
mv "Fontes Renováveisde Energia.html" "Fontes Renováveis de Energia.html"
mv "Geociências:Geofísica, Metereologia e Geodésia.html" "Geociências:Geofísica, Meteorologia e Geodésia.html"
mv "Lingüística.html" "Linguística.html"
mv "Engenharia  Aeroespacial.html" "Engenharia Aeroespacial.html"
mv "Engenharia  Nuclear.html" "Engenharia Nuclear.html"

for a in *.html
do
    recode l1..utf8 "$a"
    sed -i -e 's/charset=iso-8859-1/charset=UTF-8/' "$a"
done
