#!/bin/bash

#wget https://www.nps.gov/webcams-romo/campsite_availability_list.pdf

java -jar ~/Downloads/tabula-0.9.0-SNAPSHOT-jar-with-dependencies.jar -p 1 -a 118,24.33,749.82,562.13 -c 48,210,245,280,325,360.95,393,425,460,492.44,526.56 campsite_availability_list.pdf -o _rmnp_1.csv

java -jar ~/Downloads/tabula-0.9.0-SNAPSHOT-jar-with-dependencies.jar -p 2-48 -a 53.82,24.33,749.82,562.13 -c 48,210,245,280,325,360.95,393,425,460,492.44,526.56 campsite_availability_list.pdf -o _rmnp_2.csv

cat _rmnp_1.csv _rmnp_2.csv > rmnp_raw.csv
rm _rmnp_*.csv
