## ─────────────────────────────────────────────────────────────────
## Création de XML Moodle avec R
## © Emmanuel Curis — mars 2015
##
## Fonctions permettant l'initialisation du système
## ─────────────────────────────────────────────────────────────────
## Historique
##   31 octobre 2016 : préparé pour la création de glossaires
##                     enjolivé les commentaires
##
##   19 février 2017 : change l'encodage de utf8 en UTF-8
##                     [sinon, erreur quand test CRAN sur Solaris…]
##
##   31 mars    2017 : en terminant, on remet les options comme elles étaient…
##
##   24 mars    2020 : liste des images internes créées, pour éviter les doublons…
##
##   19 avril   2020 : dossier local d'image paramétrable
## ─────────────────────────────────────────────────────────────────

# L'environnement local
# ==> qui contiendra toutes les variables locales
SARP.Moodle.env <- new.env()

######################################################################
#                       Les variables locales                        #
######################################################################

# Variables locales : il faut les créer dans l'environnement local pour qu'elles soient modifiables
assign( "fichier.xml"  , NULL, envir = SARP.Moodle.env )  # Le fichier XML de travail
assign( "questions"    , NULL, envir = SARP.Moodle.env )  # Le fichier qui contient les sorties par défaut = le 1er créé
assign( "decimal"      , ',' , envir = SARP.Moodle.env )  # Le séparateur décimal pour les _textes_
# assign( "en_tetes"      , NULL, envir = SARP.Moodle.env )  # Là où trouver les fichiers à copier etc.

assign( "nombre.chiffres",  2, envir = SARP.Moodle.env ) # Combien de chiffres _décimaux_ dans les sorties XML ?

assign( "URL_base"      , NULL, envir = SARP.Moodle.env ) # L'URL donnant le dossier des images dans Moodle
assign( "dossier.images", "." , envir = SARP.Moodle.env ) # Le dossier local contenant les images à insérer

assign( "styles", list( "erreur" = "color: Red; font-weight: bold;" ),
        envir = SARP.Moodle.env ) # Les styles par défaut pour divers éléments fréquents...

######################################################################
#                Initialisation des exports XML                      #
######################################################################
debuter_xml.moodle <- function( fichier.xml,   # Le nom du fichier XML à créer
                                n.chiffres = 2,# Nombre de chiffres après la virgule dans les sorties XML
                                dec = ",",     # Symbole décimal dans les sorties XML (des textes)
                                racine = 2004197487,
                                glossaire = FALSE # TRUE si on veut créer un glossaire
                              ) {
    ## Contrôles
    if ( any( length( fichier.xml ) != 1, is.character( fichier.xml ) == FALSE, nchar( fichier.xml ) < 1 ) ) {
        stop( "fichier.xml doit \u00eatre un vecteur de type cha\u00eene de caract\u00e8res, ",
              "contenant un seul \u00e9l\u00e9ment" )
    }

    ## On réinitialise le compteur des figures
    assign( "numero.figure", 0, envir = SARP.Moodle.env )
    assign( "liste.images", character(), envir = SARP.Moodle.env ) # La liste des images

    ## On mémorise les options avant l'analyse
    assign( "vieilles.options", options(), envir = SARP.Moodle.env )

    ## On prépare pour la sortie des résultats numériques
    ##   (moodle attend des nombres avec le . comme séparateur décimal : on l'impose)
    options( OutDec = "." )
    assign( "decimal"      , dec, envir = SARP.Moodle.env )  # Le séparateur décimal pour les _textes_
  
    options( digits = n.chiffres )
    assign( "nombre.chiffres", n.chiffres, envir = SARP.Moodle.env )
  
    ## On initialise le générateur de nombres aléatoires
    if ( is.finite( racine ) ) {
        set.seed( seed = racine )
        generateur <- RNGkind()
    }

    ## Au besoin, on ajoute l'extension .xml
    if ( length( grep( "\\.xml$", fichier.xml ) ) != 1 ) {
        fichier.xml <- paste0( fichier.xml, ".xml" )
    }

    ## On ouvre le fichier XML --- codage en utf8 selon la doc Moodle
    ##   (Attention, dans R « utf8 » non-portable
    ##    => utiliser « UTF-8 »)
    f <- file( fichier.xml, 'w', encoding = "UTF-8" )
    attr( f, "glossaire" ) <- glossaire
    assign( "fichier.xml", f, envir = SARP.Moodle.env )

    ## En-tête générique des fichiers XML
    cat( file = f, sep = "\n",
         "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
         if ( TRUE == glossaire ) "<GLOSSARY>\n <INFO>" else "<quiz>" )

    ## On renvoie le fichier...
    return( f )
}

######################################################################
#                      Fin des exports XML                           #
######################################################################
finir_xml.moodle <- function( fichier.xml = get( "fichier.xml", envir = SARP.Moodle.env ) ) {
    glossaire <- attr( fichier.xml, "glossaire" )
    
    cat( file = fichier.xml, sep = "", "\n",
         ## Suppose que l'on a créé un glossaire réel, sinon il y aura un souci…
         if ( TRUE == glossaire ) "  </ENTRIES>\n </INFO>\n</GLOSSARY>" else "</quiz>",
         "\n" )

    close( fichier.xml )

    ## On remet les options comme elles étaient
    vx.options <- get( "vieilles.options", envir = SARP.Moodle.env )
    options( vx.options )
}
