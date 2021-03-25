import csv

with open("scimagojr 2019.csv", mode="r") as infile:
    reader = csv.reader(infile, delimiter=";")    
    with open("/tmp/scimagojrTAB", mode="w") as outfile:
        writer = csv.writer(outfile, delimiter='\t')
        writer.writerows(reader)

