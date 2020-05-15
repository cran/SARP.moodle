## —————————————————————————————————————————————————————————————————
## Création de XML Moodle avec R
## Emmanuel Curis — mars 2015, avril 2020
##
## Fonctions facilitant les affichages
## ——————————————————————————————————————————————————————————————————————
## HISTORIQUE
##   30 avr. 2020 : début de l'historique
##                  modifier affichage d'échantillon : utilise les sorties R enjolivées
##                  nombre de *décimale* correct quand on utilise la notation scientifique
## ——————————————————————————————————————————————————————————————————————

######################################################################
# Affichage d'un nombre
#
afficher_nombre.moodle <- function( x,
                                    dec = get( "decimal", envir = SARP.Moodle.env ),
                                    n.chiffres = get( "nombre.chiffres", envir = SARP.Moodle.env ) )
{
    ## Y a-t-il bien une et une seule valeur ?
    if ( is.null( x ) || ( 0 == length( x ) ) ) {
        return( paste0( "<span style=\"", 
                        get( "styles", envir = SARP.Moodle.env )$erreur,
                        "\">&empty;</span>" ) )
    }
    if ( length( x ) > 1 ) {
        warning( "Attention, seule la premi\u00e8re valeur sera convertie, les autres seront ignor\u00e9es..." )
        x <- x[ 1 ]
    }

    ## Valeur manquante ?
    if ( is.na( x ) ) {
        return( paste0( "<span style=\"", 
                        get( "styles", envir = SARP.Moodle.env )$erreur,
                        "\">NA</span>" ) )
    }

    ## Valeur non-numérique ?
    if ( is.nan( x ) ) {
        return( "<span style=\"", 
                get( "styles", envir = SARP.Moodle.env )$erreur,
                "\">NaN</span>" )
    }

    ## Valeur infinie ?
    if ( is.infinite( x ) ) {
        if ( x > 0 )
            return( "&infin;" )
        else
            return( "&minus;&infin;" )
    }

    ## Si c'est une chaîne, on l'affiche telle qu'elle...
    if ( is.character( x ) ) {
        return( x )
    }

    ## Valeur numérique
    ##  1) conversion
    txt <- paste0( if ( log( abs( x ), 10 ) > -n.chiffres ) round( x, n.chiffres ) else signif( x, n.chiffres + 1 ) )
    
    ##  2) nettoyage : le signe -
    txt <- gsub( "-", "&minus;", fixed = TRUE, txt )
    
    ##                 le séparateur décimal
    txt <- gsub( ".", dec, fixed = TRUE, txt )

    ##                 les puissances de 10
    if ( any( grepl( "e", fixed = TRUE, txt ) ) ) {
        ## Recodage de la partie puissance de 10
        txt <- paste0( gsub( "e", "&thinsp;&times;&thinsp;10<sup>", txt ), "</sup>" )
        ## On ôte le signe de l'exposant, si c'est un plus, et les 0 initiaux éventuels
        txt <- gsub( "p>[+]0*", "p>", txt )
        txt <- gsub( ";0*", ";", txt )
        ## On corrige le 1×10^... en 10^...
        txt <- gsub( "^1&thinsp;&times;&thinsp;", "", txt )
  }

  return( txt )
}

## ——————————————————————————————————————————————————————————————————————
## Affichage d'un échantillon de valeurs
##  (dans un tableau idéalement)
afficher_echantillon.moodle <- function( x, tableau = TRUE, trier = FALSE,
                                         marge = c( 10, 10, 1, 1 ), ... )
{
    ## Veut-on trier les valeurs ?
    if ( TRUE == trier ) {
        x <- sort( x )
    }

    ## Cas simple : échantillon de valeurs entre parenthèses
    if ( FALSE == tableau ) {
        txt <- unlist( lapply( x, afficher_nombre.moodle ) )
        txt <- paste0( "(",
                       paste0( txt, collapse = "&thinsp;; " ),
                       ")" )
    } else {
        ## Ici : on veut faire un tableau
        ##   (on met des espaces dans les cellules
        ##    parce que certaines versions de moodle ne mettent aucun blanc entre les colonnes...)
        txt <- sortie_R.moodle( x, enjoliver = TRUE, cadre = FALSE,
                                marge = marge, ... )
    }
        
    ## On a fini...
    return( txt )
}
