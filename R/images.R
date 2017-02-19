# -----------------------------------------------------------------
# Création de XML Moodle avec R
# Emmanuel Curis --- juin 2015
#
# Fonctions permettant la gestion des images
# -----------------------------------------------------------------

definir_dossier.image.moodle <- function( URL ) {
    print( paste( "L'URL de base des images est fix\u00e8e \u00e0 ",
                  URL ) )
    assign( "URL_base", URL, envir = SARP.Moodle.env )
}

lier_image.moodle <- function( nom.image, largeur = -1, hauteur = -1, description = NULL ) {
    URL.base <- get( "URL_base", envir = SARP.Moodle.env )
    if ( any( is.null( URL.base ), nchar( URL.base ) < 1, FALSE == is.character( URL.base ) ) ) {
        stop( "D\u00e8finissez la partie commune de l'URL des images",
              " gr\u00e2ce \u00e0 la fonction definir_dossier.image.moodle" )
    }

    url <- paste0( URL.base, "/", nom.image )

    code <- paste0( "<img src=\"", url, "\"" )
    if ( all( nchar( description ) > 0, is.character( description ), FALSE == is.null( description ) ) ) {
        code <- paste0( code, " alt=\"", description, "\"" )
    }
    if ( all( is.finite( largeur ), largeur > 0, length( largeur ) == 1 ) ) {
        code <- paste0( code, " width=", largeur )
    }
    if ( all( is.finite( largeur ), hauteur > 0, length( largeur ) == 1 ) ) {
        code <- paste0( code, " height=", hauteur )
    }
    code <- paste0( code, ">" )

    # On renvoie la balise HTML créée
    return( code )
}
