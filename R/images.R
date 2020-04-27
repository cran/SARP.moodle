## —————————————————————————————————————————————————————————————————
## Création de XML Moodle avec R
## Emmanuel Curis — juin 2015
##
## Fonctions permettant la gestion des images
## —————————————————————————————————————————————————————————————————
## HISTORIQUE
##   29 janvier 2020 : début de l'historique
##                     codage d'une image en base 64, avec balise
##                     possibiliter de lier une image « en interne »
##
##   24 mars    2020 : liste des images internes, pour éviter les doublons
##                      ne fonctionne pas :( [ne les trouve pas en « plugin file »] )
##                      Fonctionne au sein du même texte : on se limite à cela...
##
##   19 avril   2020 : dossier local d'image paramétrable avec definir_dossier.image.moodle
## —————————————————————————————————————————————————————————————————

definir_dossier.image.moodle <- function( URL, local = FALSE ) {
    if ( local ) {
        print( paste( "Le dossier local des images est fix\u00e8e \u00e0 ",
                     URL ) )
        assign( "dossier.images", URL, envir = SARP.Moodle.env )
    } else {
        
        print( paste( "L'URL de base des images est fix\u00e8e \u00e0 ",
                      URL ) )
        assign( "URL_base", URL, envir = SARP.Moodle.env )
    }
}

lier_image.moodle <- function( nom.image, largeur = -1, hauteur = -1, description = NULL,
                               interne = FALSE ) {
    if ( interne ) {
        URL.base <- "@@PLUGINFILE@@/"
        nom.image <- basename( nom.image )
    } else {
        URL.base <- get( "URL_base", envir = SARP.Moodle.env )
    }
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

coder_image.moodle <- function( nom.image,
                                dossier.image = get( "dossier.images", envir = SARP.Moodle.env ) )
{
    nom.image.court <- nom.image
    if ( FALSE == file.exists( nom.image ) ) {
        nom.image <- paste0( dossier.image, "/", nom.image.court )
    }
    if ( FALSE == file.exists( nom.image ) ) {
        nom.image <- paste0( tempdir() , "/", nom.image.court )
    }
    if ( FALSE == file.exists( nom.image ) ) {
        stop( "Image ", nom.image, " non trouv\u00e9e..." )
    }

    ## On isole le nom de fichier
    nom.court <- basename( nom.image )

    ## On vérifie que ce n'est pas déjà dans la liste
    ##   (on utilise le nom court, car c'est celui qui sera dans le lien...)
    lst.img <- get( "liste.images", SARP.Moodle.env )
    if ( nom.court %in% lst.img ) {
        warning( "Image interne ", nom.court, " d\u00e9j\u00e0 cr\u00e9\u00e9..." )
        return( "" )
    } else {
        ## On ajoute l'image à la liste
        lst.img <- c( lst.img, nom.court )
        assign( "liste.images", lst.img, SARP.Moodle.env )
    }

    ## La conversion en Base 64
    code.image <- base64enc::base64encode( nom.image )

    ## On prépare la balise XML
    code.image <- paste0( "<file",
                          " name=\"", nom.court, "\"",
                          " path=\"/\"",
                          " encoding=\"base64\">",
                          code.image,
                          "</file>" )
    ## On renvoie cette balise
    code.image
}
