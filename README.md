# Truc
Tahiti R User Community

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

# Organisation de la communauté :

La Tahitien R User Community est née dans le cadre du datalab et coordonnée par l'ISPF. Les membres de ce groupe d'échange sont des codeurs R et le but est de partager nos connaissances, notre savoir faire et de tester des nouveautés.

Pour cela, plusieurs outils sont à disposition des participants et sont recensés sur le [[Blog ISPF]{.ul}](https://blog.ispf.pf/) :

-   les rendez vous sont annoncés sur le blog et sur le [[Meetup de Tahitidevops]{.ul}](https://www.meetup.com/fr-FR/TahitiDevOps)

-   le contenu des réunions est partagé sur le blof et sur le [[github]{.ul}](https://github.com/ISPF/Truc) de la communauté

Le choix des sujets est soumis au vote des participants.

Le lieu de rencontre est modulable : ISPF - CPS - CHPF

Horaires : dépendants des lieux de rencontre

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
