## —————————————————————————————————————————————————————————————————
## Création de XML Moodle avec R
## Emmanuel Curis — juin 2015
##
## Fonctions permettant la gestion des images
## —————————————————————————————————————————————————————————————————
## HISTORIQUE
##    1 janvier 2021 : création du fichier
##                     repris de lier_image.moodle
##
##   18 mai     2023 : conversion stop → erreur
## —————————————————————————————————————————————————————————————————

lier_fichier.moodle <- function( nom.fichier, texte.lien = NULL,
                                 interne = TRUE ) {
    if ( interne ) {
        URL.base <- "@@PLUGINFILE@@/"
        nom.fichier <- basename( nom.fichier )
    } else {
        URL.base <- get( "URL_base", envir = SARP.Moodle.env )
    }
    if ( any( is.null( URL.base ), nchar( URL.base ) < 1,
              FALSE == is.character( URL.base ) ) ) {
        erreur( 650, "lier_fichier.moodle",
                "D\u00e8finissez la partie commune de l'URL des fichiers",
                " gr\u00e2ce \u00e0 la fonction definir_dossier.image.moodle" )
    }

    url <- paste0( URL.base, "/", nom.fichier )

    code <- paste0( "<a href=\"", url, "\">" )
    if ( all( nchar( texte.lien ) > 0, is.character( texte.lien ), 
              FALSE == is.null( texte.lien ) ) ) {
        code <- paste0( code, texte.lien )
    } else {
        code <- paste0( code, "[", nom.fichier, "]" )
    }
    code <- paste0( code, "</a>" )

    # On renvoie la balise HTML créée
    return( code )
}
