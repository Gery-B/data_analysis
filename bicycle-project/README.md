# Bicycle Project — Code R

Ce dossier contient le script principal d’analyse en **R**.

## Contenu
- **code.R** : script unique qui gère l’ensemble du workflow :
  1. Connexion à la base **MySQL** via `{RMariaDB}`
  2. Prétraitement et enrichissement temporel
  3. Analyses multivariées (ACP + HCPC)
  4. Régressions multiples avec imputation (`mice`) et retrait des outliers
  5. Export des résultats (fichiers CSV, graphiques, outputs pour Power BI)

## Utilisation
```r
# Charger les librairies
library(DBI); library(RMariaDB); library(tidyverse)

# Exécuter l’analyse complète
source("code.R")
