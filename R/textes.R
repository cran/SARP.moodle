## —————————————————————————————————————————————————————————————————
## Création de XML Moodle avec R
## Emmanuel Curis — juin 2015
##
## Fonctions permettant la préparation des textes
## —————————————————————————————————————————————————————————————————
## HISTORIQUE
##   24 juin 2020 : création du fichier
##
##    1 jan. 2021 : question de pur text (« description »)
## —————————————————————————————————————————————————————————————————

## —————————————————————————————————————————————————————————————————
##
## Question de type « description »
##
## —————————————————————————————————————————————————————————————————
description.moodle <- function( texte,
                                titre = "Question r&eacute;dactionnelle ouverte",
                                commentaire.global = NA, 
                                idnum = NA, 
                                fichier.xml = get( "fichier.xml", envir = SARP.Moodle.env ) )
{
    ## On crée la question
    debut_question.moodle( type = "description",
                           titre = titre, texte = texte,
                           commentaire.global = commentaire.global,
                           penalite = 0, note = 0, idnum = idnum,
                           fichier.xml = fichier.xml )

    ## Et on a fini en fait…
    fin_question.moodle( fichier.xml = fichier.xml )
}

## —————————————————————————————————————————————————————————————————
##
## Préparation des textes pour meilleur rendu etc.
## 
## —————————————————————————————————————————————————————————————————

preparer_texte <- function( textes )
{
    ## On supprime les doubles-espaces
    textes <- gsub( " {2,}", " ", textes )

    ## On supprime les espaces en début de ligne
    textes <- gsub( "^[[:space:]]+", "", textes )

    ## On supprime les espaces en fin de ligne
    textes <- gsub( "[[:space:]]+$", "", textes )

    ## On convertit les < qui trainent en &lt;
    textes <- gsub( "<([[:space:]]+)", "&lt;\\1", textes )

    ## On convertit les > qui trainent en &gt;
    textes <- gsub( "([[:space:]]+)>", "\\1&gt;", textes )
    
    ## On renvoie les textes modifiés
    textes
}
