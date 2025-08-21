# Analyse des flux cyclistes à Bruxelles (CB02411)
**ACP + HCPC + Régression multiple en R** — Connexion directe **MySQL** via `{RMariaDB}` — Dashboard **Power BI**.

## Objectif
Comprendre l’évolution des passages vélo à Bruxelles (2022–2024), identifier des profils de stations (jour/nuit, semaine/saisons) et modéliser les facteurs explicatifs (calendrier, météo).

## Méthodologie
1) **Données** : connexion MySQL avec `{RMariaDB}` (stations/horaires) + données calendaires (vacances/jours fériés).  
2) **R (tidyverse)** : nettoyage, features temporelles.  
3) **ACP (FactoMineR) + HCPC** : typologie de stations.  
4) **Régression multiple** (avec imputation `mice`, retrait outliers/Cook).  
5) **Power BI** : tableau de bord interactif (observé vs ajusté, filtres).

## Fichiers
- Script : [`bicycle-project/code.R`](bicycle-project/code.R)  
- Données calendaires : [`data/Vacances FR-NL.csv`](data/Vacances%20FR-NL.csv)  
- Rapport PDF : [`results/Analyse-borne-CB02411.pdf`](results/Analyse-borne-CB02411.pdf)  
- Dashboard : [`results/Dashboard-CB02411.pbix`](results/Dashboard-CB02411.pbix)

## Reproduire
```r
# Connexion MySQL (adapter les secrets)
con <- DBI::dbConnect(RMariaDB::MariaDB(),
                      user="USER", password="***",
                      dbname="DB", host="HOST", port=3306)

# Exécuter l'analyse
source("bicycle-project/code.R")
