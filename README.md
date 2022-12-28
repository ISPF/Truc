# Bienvenue dans la Tahitian R User Community !

Quelques précisions sur celle-ci ... 
L'objectif est de se réunir et échanger sur les bonnes pratiques, les nouveautés, les nouveaux packages, les réalisations de chacun sur le logiciel R.
Ces rencontres ont lieu les 1er mardis de chaque mois de 14h à 16h, il se peut qu'il y ait des exceptions donc restez connectés au groupe [LinkedIn TRUC](https://www.linkedin.com/groups/13966558/), tous les rendez vous y seront postés. Les sessions peuvent être organisées à différents endroits et l'accueil peut se faire par différents organismes sur volontariat.

Le mode de fonctionnement est collaboratif, c'est-à-dire que chacun amène ses connaissances et son expertise afin d'échanger et d'enrichir le collectif. Le programme des sessions est soumis à un vote: on retient les 5 premiers sujets afin de rythmer les prochaines séances. Les sujets sont conservés si au moins un intervenant se propose pour présenter du contenu. Un récapitulatif des sujets abordés se trouve sur le [Blog de l'ISPF](https://blog.ispf.pf/).
Après chaque réunion, le contenu est poussé sur le [GitHub](https://github.com/ISPF/Truc) du TRUC et accessible à tous.

A bientôt pour de nouvelles aventuRes,



Le lieu de rencontre est modulable : ISPF - CPS - CHPF

Horaires : dépendants des lieux de rencontre

# Saison 03

Agenda 2023

- Décembre 2022 - Advent of code ( Advent of Code 2022 )
- 17 Janvier - Les meilleurs graphiques avec ggplot : chacun un graphique différent - (Nathalie)
- 7 Février - Comment scraper du web + présentation Json (Hubert)
- 7 Mars - R et intelligence artificielle (tensorflow) (Laurent P et Benjamin)
- 4 Avril - Analyse des réseaux sociaux avec R (Florence)
- 2 Mai - Écrire un jeu sous R (Laurent R)
- Juin - dplyr, dtplyr, data.table, Substitute2, en mode TD (Serge)
- Juillet - OFF
- Août -  OFF
- 5 Septembre - Analyse textuelle (Mathieu)
- 3 Octobre -  Calage sur marge (pour redresser les poids d'une enquête) (Julie)
- 7 Novembre - algorithme génétique (Mathieu)
- Décembre - OFF




# S01E01 - Première rencontre

# Tour de table
Olivier Bessalem (Bureau Etudes CPS), Jean WENCELIUS (Anthropologue), Ariinui TERIITEHAU (Etudiant), Philippe (Contrôleur de gestion CHPF), Thierry POIRINE(Informaticien ISPF), Mathieu BOLDUC(Statisticien / Méthodologue ISPF), Florence BOULIOU (Statisticien ISPF), Léopold (attaché enseignement et recherche économie), Peter (informaticien, Freelance), Hubert LEVIEL (data scientist, Freelance), Jean-François BAILLETTE (Consultant cybersécurité), Laurent PELLET (Informaticien ISPF)

# Déroulé de la rencontre
* Présentation du datalab
* Présentation d’un projet d’ETL, Laurent P. ISPF
* Extraire des données de base SQL, calculer une distance de Jaro Winckler puis utilisé ces données sur l’API
* Débat sur data table et data frame
* Expression des besoins de chacun en participants à cette communauté: visualisation de données cartographiques (R vs Qgis), prédiction de résultat d’une élection, connecter son serveur AS 400

# Idées pour les prochaines sessions

Savoir faire présent au sein des participants:shiny, plotli, encapsulement dockeR, analyse multivariée appliquée, modèle de gravité



# S01E02 - Vote alternatif et SIG
## Vote
Documentation sur le vote alternatif

[Vote à second tour instantané](https://fr.wikipedia.org/wiki/Vote_%C3%A0_second_tour_instantan%C3%A9)

[Vote à second tour instantané (en)](https://en.wikipedia.org/wiki/Instant-runoff_voting)


[[Mail liste des utilisateurs du TRUC]{.ul}](https://docs.google.com/spreadsheets/d/1gPdIxBFNqHY299Ywfv-UrSkJrIULryD5BuiP12cHExA/edit?usp=sharing)

[[Fichier de contact]{.ul}](https://drive.google.com/file/d/1Rp7t79xnojTQmYXv89hKy-vRhSEJkjdE/view?usp=sharing) (pour importation directement dans vos contacts google)



# Déroulement de la rencontre

## Présentation du package votesys 

Le package permet de générer un processus de vote pour sélectionner les sujets à aborder lors des prochaines sessions.


### Sujets proposés au vote
- Data.table vs DPLYR
- SIG suite
- Encodage UTF-8 et les autres
- Shiny/Plotly
- Dates et lubridate
- modèles GLM
- Tests unitaires en R
- Julia
- Notebooks Jupyter/RMD

### Résultats du vote

- Shiny/Plotly                    8
- modèles GLM                     7
- SIG suite                       5
- Tests unitaires en R            5
- Notebooks Jupyter/RMD           4
- Data.table vs DPLYR             2
- Dates et lubridate              1
- Encodage UTF-8 et les autres    0
- Julia                           0


### Planification des interventions


> \- 1^er^ : Shiny / Plotly : Hubert -- Laurent.R -- Jean ⇒ S01 E03
>
> \- 2^ème^ : Modèle linéraire / Modèle de gravité : Géraldine -- Mathieu -- Ariinui ⇒ S01 E04
>
> \- 3^ème^ : SIG (suite) : *intervenant ?*
>
> \- 4^ème^ : tests unitaires : *intervenant ?*
>
> \- 5^ème^ : Notebook Rmd / Jupiter : Philippe - Laurent P. ⇒ S01 E0X
>

#### Autres sujets non soumis au vote:

-   intervention de la société ThinkR sur la création de package

-   analyse multivariée (ACP, AFM, ACM): intervenant Philippe

-   dojo

-   algorithme de détection de fraude

## Présentation des packages leaflet, rgdal, sf

Démonstration de l'utilisation de ces packages en utilisant des données de cartographie disponibles en [[open data]{.ul}](https://www.data.gouv.fr/fr/organizations/institut-de-la-statistique-de-la-polynesie-francaise/) pour la réalisation de carte :

-   leaflet situer des emplacements sur une carte type google maps

-   rgdal / sf visualiser des données sur une carte

# S01E03 - Shiny et Plotly - Hubert, Laurent et Jean

## Présentation d'une application shiny en ligne et session codage d'une nouvelle appli

### FLARES - Shiny app poour l'analyse de freelists
Démonstration de l'application FLARES à titre d'illustration pour montrer ce qu'on peut faire avec shiny. 
[Lien vers l'appli](http://anthrocogs.com/shiny/flares)
Jeu de données dispo [ici](http://anthrocogs.com) pour faire tourner l'application

### Session codage : appli type dashboard pour explorer les données du MCR-LTER (Moorea Coral Reef Long-Term Ecological Research Programme)
A partir de données écologiques sur les comptages de poissons et relevés substrats autour de Moorea mis en accès libre par le MCR-LTER création d'une application pour visualiser les données avec un line chart et une carte.
Code et données (retravaillées) dispo dans sur GitHub


# S01E04 - Modèle linéaires - Mathieu Géraldine



# Saison 2

Ce mardi 7 décembre s'est tenu à la Polynesian Factory de Pirae la dernière session de la communauté TRUC pour cette année 2021. Merci aux participants pour leur présence.

Avant cette rencontre, un vote à un tour avait été proposé pour définir les sujets à aborder lors des sessions 2022. Il en est ressorti les scores suivants :
1. Package flexdashboard ... 13 points
2. Les meilleurs graphes ggplot ... 12 points
3. Date et time series : BDD sous R ... 12points
4. Package shiny avec golem ... 9 points
5. R et intelligence artificielle (Raspberry Pi) ... 9 points
6. Travail d'équipe: Coupe du Monde de foot 2022 : quelle équipe va la gagner ?... 8 points
7. Plumber et présentation de l'API de l'ISPF ... 6 points
8. Le package Rspotify : analyser les playlists ... 6 points
9. Création de site ou blog avec le package distill ... 5 points
10. Rocker ... 5 points
11. Data table, viz gganimate, série chronologique (stl,; arima, décomposition)... 5 points
12. Package eurostat ... 1 point
13. Apply/map & co ... 1 point
14. Les modèles de prédiction sur le Covid ... 1 point
15. Les modèles de prédiction sur les élections ... 0 point

Les discussions autour de ces résultats lors de la session d'hier ont permis d'établir une ébauche de planning pour les prochains mois. Ces programmations sont à titre indicatif et peuvent être amenées à évoluer ; les événements programmés sur LinkedIn feront foi.
L'ISPF propose d'héberger ces sessions, mais reste ouvert à toute proposition extérieure (CPS, Polynesian Factory, autres). Les sessions sont pour le moment toujours prévues le premier mardi de chaque mois de 14 à 16h. 

## Janvier 2022 : Pas de session prévue

## Février 2022 : 
 - date / heure : mardi 2 février 2022 à 14h 
 - sujet : les meilleurs graphiques avec ggplot
 - intervenant(s) : mode collaboratif, chacun propose un ou plusieurs graphes original(ux)
 - thème avec le 2e plus gros score lors du vote
 - en fin de séance, Laurent P et Hubert aborderont également le package rocker (conteneurs dockers en environnement R)
 
 EDIT : **thème annulé**
 

## Avril 2022 : 
 - date / heure : mardi 5 avril 2022 à 14h    
 - sujet : flexdashboard, dont le principe est de créer des tableaux de bord interactifs facilement à partir de R markdown
 - intervenant(s) : présentation par Hubert
 - thème ayant obtenu le plus gros score lors du vote

## Mai 2022 : 
 - date / heure : mardi 3 mai 2022 à 14h    
 - sujet : time series
 - intervenant(s) : Mathieu et Benjamin (stagiaire ISPF)


## Juin 2022 : 
 - date / heure : mardi 7 juin 2022 à 14h    
 - sujet : data.table / dplyr / tibble
 - intervenant(s) : Mathieu
 - Mathieu a déjà présenté data.table lors de la session d'hier (merci à lui), le but est donc d'aller un peu plus loin sur ces packages

ET/OU

 - sujet : Coupe du Monde de foot 2022 : quelle équipe va la gagner ?
 - intervenant(s) : Florence et Laurent R
 - mode collaboratif : deux modèles vont être proposés ; deux groupes seront définis, chacun appliquant un modèle
    
## Juillet / Août / Septembre 2022 : Pas de session planifiée pour le moment. La charge de travail sera importante à l'ISPF avec le recensement de la population, mais d'autres intervenants sont les bienvenus pour reprendre la main au moins durant cette période.
