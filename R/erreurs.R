## ——————————————————————————————————————————————————————————————————————
## Création de XML Moodle avec R
## © Emmanuel Curis — mars 2015, mai 2019
##
## Traitement des messages d'erreur harmonisé
##   (pour faciliter l'interfaçage avec Shiny)
##
## Sur une idée de Virginie LASSERRE, février 2016
## ——————————————————————————————————————————————————————————————————————
## Historique
##  15 mai 2023 : création du fichier
##
##  18 mai 2023 : mémorisation du code d'erreur ou d'avertissement
##
##  31 mai 2023 : les avertissements sont affichés dans la console lors de leur appel
## ——————————————————————————————————————————————————————————————————————

erreur <- function( numero, nom.fonction, ... ) {
    ## On mémorise le code d'erreur
    assign( "code.erreur", numero, envir = SARP.Moodle.env )

    ## On crée le message
    texte <- paste0( "[", 
                     formatC( numero, width = 4,
                              format = "d", flag = "0" ),
                     ":", nom.fonction, "] ",
                     "ERREUR : ",
                     ... )

    ## On s'arrête proprement
    stop( texte )
}

avertissement <- function( numero, nom.fonction, ... ) {
    ## On ajoute le code d'avertissement à la liste
    liste <- get( "liste.avertissements", envir = SARP.Moodle.env )
    liste <- c( liste, numero )
    assign( "liste.avertissements", numero, envir = SARP.Moodle.env )

    ## On crée le message
    texte <- paste0( "[", 
                     formatC( numero, width = 4,
                              format = "d", flag = "0" ),
                     ":", nom.fonction, "] ",
                     "Attention, ",
                     ... )

    ## On affiche le texte
    cat( texte, "\r\n" )
    
    ## On émet l'avertissement
    warning( texte )
}
