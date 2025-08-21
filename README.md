#  Analyse des flux de mobilité douce à Bruxelles : classification temporelle et modélisation des passages de la station de comptage CB02411

## Objectif
Étudier l’évolution des flux cyclistes à Bruxelles et identifier les facteurs explicatifs des variations observées, en distinguant notamment les profils de stations selon les périodes (jour/nuit, jours ouvrables, saisons).

---

##  Méthodologie
- Nettoyage et préparation des données (base SQL exportée en CSV).
- Analyses statistiques avec **R** :
  - **ACP (Analyse en Composantes Principales)** pour réduire la dimensionnalité.
  - **Clustering hiérarchique (HCPC)** pour regrouper les stations selon leur profil d’utilisation.
  - **Régression multiple** pour modéliser les passages sur la borne de référence (CB02411).
- Visualisations interactives via **Power BI**.

---

## Contenu du dossier
- `code.R` → Script R complet (préparation, ACP, clustering, régressions, visualisations).
- `Vacances FR-NL.csv` → Base de données des jours fériés/vacances scolaires utilisée dans l’analyse.
- `Dashboard - Analyse de la borne CB02411 - Bochenski Géry.pbix` → Tableau de bord Power BI interactif.
- `resume.pdf` → Résumé écrit du projet (méthodologie, résultats principaux).

---

## Résultats principaux
- **Profils de stations** identifiés par clustering (jours ouvrables, jour/nuit).
- **Facteurs explicatifs** des flux mis en évidence par régression.
- **Dashboard Power BI** permettant :
  - Analyse temporelle (jour/semaine/mois/saison).
  - Comparaison entre valeurs observées et ajustées.
  - Exploration interactive par filtres.

---

## Utilisation
1. **Exécuter le code R** :  
   Ouvrir `code.R` dans RStudio → installer les packages nécessaires (`tidyverse`, `FactoMineR`, `factoextra`, `mice`, `performance`, etc.) → lancer l’analyse.
   
2. **Explorer le dashboard** :  
   Ouvrir `Dashboard - Analyse de la borne CB02411 - Bochenski Géry.pbix` dans **Power BI Desktop**.

3. **Lire la synthèse** :  
   Consulter `resume.pdf` pour une vue d’ensemble.

---

## Auteurs
Projet réalisé par **Géry Bochenski**  
Certificat universitaire Junior Data Analyst – UCLouvain (2024–2025)

---
