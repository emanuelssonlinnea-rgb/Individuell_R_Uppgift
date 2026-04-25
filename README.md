# Individuell_R_Uppgift
Ett försäkringsbolag har beställt en analys av deras historiska kunddata i syfte att undersöka vilka variabler som påverkar försäkringskostnaderna mest. Målet är att utreda datan och genomföra en regressionsanalys som förhoppningsvis kan användas som beslutsstöd för framtida kunder.

## Så kör du projektet
1. Klona eller ladda ner projektet från GitHub
2. 	Öppna projektet i RStudio och kör Individuell_Uppgift_R.R
3. 	RStudio öppnar projektet med rätt arbetskatalog och struktur.

## Paket som använts
- glmnet
- here
- Metrics
- patchwork
- tidyverse

# Projektstruktur
- Individuell_Uppgift_R.R
- data:
  - insurance_costs.csv
- script:
  - R_Uppgift_Linnea_Emanuelsson.R
- output:
  -  figurer
- report:
  - Rapport.qmd

## Hur analysen körs
Du kan köra analysen på två sätt:
1. Kör hela rapporten
  I RStudio:
  - Öppna report/Rapport.qmd
  - Klicka Render
  - Det genererar en HTML‑rapport med en analytisk rapport

2. Kör analysen direkt i konsolen
  - Öppna filen script/R_Uppgift_Linnea_Emanuelsson.R
  - Kör hela skriptet eller valda delar.
