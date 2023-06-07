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
##
##   30 janvier 2021 : corrigé une coquille dans l'affichage des dossiers modifiés
##
##    9 mai     2023 : liste de dossiers de recherche possible
##                     plus de flexibilité dans la recherche des images
##                     affichage des messages par cat et plus print
##                     affichage des messages de choix de dossier désactivable
##
##   18   mai      2023 : conversion stop → erreur et warning → avertissement
## —————————————————————————————————————————————————————————————————

definir_dossier.image.moodle <- function( URL, local = FALSE,
                                          ajouter = TRUE, silencieux = FALSE ) {
    if ( local ) {
#        silencieux <- FALSE
        ## On récupère la liste des dossiers actuelle
        dossiers <- get( "dossier.images", envir = SARP.Moodle.env )

        ## On ajoute ou remplace par le dossier fourni
        if ( FALSE == ajouter ) {
            if ( !silencieux ) {
                cat( "Le dossier local des images est fix\u00e9 \u00e0 ",
                     URL, ".\n", sep = "" )
            }
            dossiers <- URL
        } else {
            if ( URL %in% dossiers ) {
                if ( length( dossiers ) > 1 ) {
                    if ( !silencieux ) {
                        cat( "Le dossier ", URL, " est replac\u00e9",
                             " en premi\u00e8re position de recherche.\n", sep = "" )
                    }
                    idx <- which( dossiers == URL )
                    dossiers <- c( dossiers[ idx ], dossiers[ -idx ] )
                }
            } else {
                if ( !silencieux ) {
                    cat( "Le dossier ", URL, " est ajout\u00e9",
                         " \u00e0 la liste des dossiers explor\u00e9s.\n", sep = "" )
                }
                dossiers <- c( URL, dossiers )
            }
        }

        ## On mémorise cette nouvelle liste
        assign( "dossier.images", dossiers, envir = SARP.Moodle.env )

        r <- dossiers
    } else {        
        if ( !silencieux ) {
            cat( "L'URL de base des images est fix\u00e9e \u00e0 ",
                 URL, ".\n", sep = "" )
        }
        assign( "URL_base", URL, envir = SARP.Moodle.env )

        r <- dossiers
    }

    ## On renvoie l'URL ou la liste des dossiers de recherche
    r
}

lier_image.moodle <- function( nom.image, largeur = -1, hauteur = -1, description = NULL,
                               interne = FALSE ) {
    if ( interne ) {
        URL.base <- "@@PLUGINFILE@@/"
        dossier.image <- dirname( nom.image )
        nom.image <- basename( nom.image )
        definir_dossier.image.moodle( dossier.image, local = TRUE, ajouter = TRUE,
                                      silencieux = TRUE )
    } else {
        URL.base <- get( "URL_base", envir = SARP.Moodle.env )
    }
    if ( any( is.null( URL.base ), nchar( URL.base ) < 1,
              FALSE == is.character( URL.base ) ) ) {
        erreur( 650, "lier_image.moodle",
                "D\u00e8finissez la partie commune de l'URL des images",
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
                                dossier.image = get( "dossier.images",
                                                     envir = SARP.Moodle.env ) )
{
    ## Le nom fourni, complet (peut être un chemin relatif)
    nom.image.court <- nom.image

    ## On cherche d'abord avec le nom fourni
    ##  1) tel quel [si un chemin absolu est fourni]
    if ( FALSE == file.exists( nom.image ) ) {
        nom.image <- paste0( dossier.image, "/", nom.image.court )
    }
    
    ##  2) dans l'un des dossiers enregistrés
    ##       (peut être un sous-dossier si nom.image contient un chemin relatif…)
    existe <- file.exists( nom.image )
    if ( any( existe ) ) {
        idx.ok <- which( existe )
        if ( length( idx.ok ) > 1 ) {
            avertissement( 650, "coder_image.moodle",
                           "Image ", nom.image, " trouv\u00e9e",
                           " dans plusieurs dossiers.",
                           " Seule la premi\u00e9re sera utilis\u00e9e",
                           " [", nom.image[ idx.ok[ 1 ] ], "]" )
        }
        nom.image <- nom.image[ idx.ok[ 1 ] ]
    }    
    if ( FALSE == file.exists( nom.image ) ) {
        nom.image <- paste0( tempdir() , "/", nom.image.court )
    }

    ##  3) dans le dossier temporaire de R
    if ( FALSE == file.exists( nom.image ) ) {
        nom.image.court <- basename( nom.image )
    }

    ## Là, on ne l'a pas trouvé : problème de chemin relatif incorrect ?
    ##  ⇒ on réessaye avec juste le nom de fichier
    
    ##  4) dans le dossier de travail
    if ( FALSE == file.exists( nom.image ) ) {
        nom.image <- paste0( dossier.image, "/", nom.image.court )
    }

    ##  5) dans l'un des dossiers enregistrés
    existe <- file.exists( nom.image )
    if ( any( existe ) ) {
        idx.ok <- which( existe )
        if ( length( idx.ok ) > 1 ) {
            avertissement( 650, "coder_image.moodle",
                           "Image ", nom.image, " trouv\u00e9e",
                           " dans plusieurs dossiers.",
                           " Seule la premi\u00e9re sera utilis\u00e9e",
                           " [", nom.image[ idx.ok[ 1 ] ], "]" )
        }
        nom.image <- nom.image[ idx.ok[ 1 ] ]
    }    
    if ( FALSE == file.exists( nom.image ) ) {
        nom.image <- paste0( tempdir() , "/", nom.image.court )
    }
    
    ##  6) dans le dossier temporaire de R
    if ( FALSE == file.exists( nom.image ) ) {
        erreur( 103, "coder_image.moodle",
                "Image ", nom.image.court, " non trouv\u00e9e..." )
    }

    ## On isole le nom de fichier
    nom.court <- basename( nom.image )

    ## On vérifie que ce n'est pas déjà dans la liste
    ##   (on utilise le nom court, car c'est celui qui sera dans le lien...)
    lst.img <- get( "liste.images", SARP.Moodle.env )
#    print( lst.img ) ; cat( "\n" )
    if ( nom.court %in% lst.img ) {
        avertissement( 651, "coder_image.moodle",
                       "Image interne ", nom.court, " d\u00e9j\u00e0 cr\u00e9\u00e9e..." )
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
