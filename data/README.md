# Données

Ce dossier contient les fichiers de données externes nécessaires à l’analyse.

## Contenu
- **Vacances FR-NL.csv** : calendrier des vacances scolaires et jours fériés en Belgique (utilisé pour enrichir les données temporelles).

## Données de comptage vélo
Les données horaires de la station **CB02411** ne sont **pas incluses ici** car elles proviennent d’une base **MySQL** privée.  
Elles sont chargées directement via le package `{RMariaDB}` dans le script R (`bicycle-project/code.R`).

Pour exécuter le script, configurez d’abord les variables d’environnement (voir README principal).
