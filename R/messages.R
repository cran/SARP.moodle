## —————————————————————————————————————————————————————————————————
## Création de XML Moodle avec R
## Emmanuel Curis — mars 2015, avril 2020
##
## Fonctions facilitant les affichages : messages prédéfinis
## ——————————————————————————————————————————————————————————————————————
## HISTORIQUE
##   31 mai  2020 : création du fichier
##
##    8 juil 2020 : si temps = NULL, on renvoie une chaîne vide
## ——————————————————————————————————————————————————————————————————————

## ——————————————————————————————————————————————————————————————————————
##
##              Temps conseillé pour répondre à une question
##
## temps    : le temps conseillé (en min)
## couleur  : la couleur du message
## nv.ligne : si TRUE, on commence par un retour à la ligne

temps_necessaire.moodle <- function( temps, couleur = "Blue",
                                     nv.ligne = TRUE )
{
    ## Tous les cas où aucun temps n'est demandé
    if ( missing( temps ) ) return( "" )
    if ( any( is.null( temps ), length( temps ) == 0 ) ) {
        return( "" )
    }
    if ( length( temps ) > 1 ) {
        warning( "Plusieurs temps - seul le premier sera",
                 " utilis\u00e9." )
        temps <- temps[ 1 ]
    }
    if ( !is.finite( temps ) ) return( "" )

    ## On affiche le temps conseillé pour répondre
    txt <- paste0( if ( nv.ligne ) "<br />",
                   "<span style=\"color: ", couleur, ";\">",
                   "Temps conseill\u00e9 pour ",
                   "<span style=\"white-space: nowrap;\">",
                   " r\u00e9pondre&nbsp;:</span> ",
                   "<span style=\"white-space: nowrap;\">",
                   "<b>" )

    temps.min <- as.integer( temps )
    if ( temps.min > 0 ) {
        txt <- paste0( txt, temps.min, "&nbsp;min" )
    }
    if ( temps > temps.min ) {
        temps.s <- temps - temps.min
        temps.s <- 60 * temps.s
        temps.s <- as.integer( round( temps.s, 0 ) )

        txt <- paste0( txt, if ( temps.min > 0 ) "&nbsp;",
                       temps.s, "&nbsp;s" )
    }
    
    txt <- paste0( txt, "</b></span>.</span>" )
    txt
}
