## ─────────────────────────────────────────────────────────────────
## Création de XML Moodle avec R
## © Emmanuel Curis — mars 2015
##
## Conversion d'un fichier au format « QCM optique »
##   (Sur une idée de Wilfrid CARIOU)
## ─────────────────────────────────────────────────────────────────
## Historique
##   27 avril 2020 : création du fichier
##
##   10 déc.  2020 : corrigé l'oubli de l'option  « quote = »
##
##   18 mai   2023 : conversion stop → erreur
##                   utilisation de la fonction de vérification
## ─────────────────────────────────────────────────────────────────

csv_optique.moodle <- function( fichier.csv,
                                forcer.multiple = FALSE, melanger.reponses = TRUE, somme.nulle = FALSE,
                                fichier.xml = if ( TRUE == nv.fichier ) gsub( "\\.[Cc][Ss][Vv]$",
                                                                              ".xml", fichier.csv )
                                              else get( "fichier.xml", envir = SARP.Moodle.env ),
                                nv.fichier = TRUE,
                                embellir = TRUE, deja.HTML = FALSE,
                                categorie.base = "",
                                dossier.images = ".", sep.images = c( '@@', '@@' ), inserer.images = TRUE,
                                sep.formules = c( '@\\$', '\\$@' ),
                                sep.SMILES = c( '@\\{', '\\}@' ),
                                sep = ";", header = TRUE, quote = '"', 
                                ... )
{
    ## Vérifications
    verification_conversions( fichier.xml = fichier.xml, nv.fichier = nv.fichier,
                              inserer.images = inserer.images,
                              dossier.images = dossier.images,
                              fonction = "csv_optique.moodle" )

    ## On contrôle les séparateurs d'images
    if ( length( sep.images ) == 1 ) {
        sep.images <- c( sep.images, sep.images )
    }
    if ( length( sep.images ) > 2 ) {
        erreur( 9, "csv_optique.moodle",
                "Il faut deux s\u00e9parateurs d'image au plus :",
                " pour le d\u00e9but et pour la fin." )
    }

    ## On contrôle les séparateurs de formules
    if ( length( sep.formules ) == 1 ) {
        sep.formules <- c( sep.formules, sep.formules )
    }
    if ( length( sep.formules ) > 2 ) {
        erreur( 10, "csv_optique.moodle",
                "Il faut deux s\u00e9parateurs de formule au plus :",
                " pour le d\u00e9but et pour la fin." )
    }

    ## On contrôle les séparateurs de codes SMILES
    if ( length( sep.SMILES ) == 1 ) {
        sep.SMILES <- c( sep.SMILES, sep.SMILES )
    }
    if ( length( sep.SMILES ) > 2 ) {
        erreur( 11, "csv_optique.moodle",
                "Il faut deux s\u00e9parateurs de code SMILES au plus :",
                " pour le d\u00e9but et pour la fin." )
    }

    
    ## Si demandé : on crée le fichier XML
    if ( TRUE == nv.fichier ) {
        if ( any( is.na( fichier.xml ), length( fichier.xml ) != 1,
                  FALSE == is.character( fichier.xml ), nchar( fichier.xml ) < 1 ) ) {
            erreur( 7, "csv_optique.moodle",
                    "Il faut indiquer un et un seul nom de fichier XML pour la sortie..." )
        }

        fichier.xml <- debuter_xml.moodle( fichier.xml )
    }

    
    ## On charge les fichiers CSV indiqués à tour de rôle...
    l <- as.list( rep( "", length( fichier.csv ) ) )
    names( l ) <- fichier.csv    
    for ( f in fichier.csv ) {
        cat( "*** Traitement du fichier \u00ab ", f, " \u00bb ***\n" )

        ## On fait le traitement réel...
        d <- csv.qcm_optique_vers_XML( fichier.csv = f,
                                       fichier.xml     = fichier.xml, 
                                       embellir        = embellir   ,
                                       deja.HTML       = deja.HTML  ,
                                       forcer.multiple = forcer.multiple,
                                       melanger.reponses = melanger.reponses,
                                       somme.nulle     = somme.nulle,
                                       categorie.base  = categorie.base,
                                       dossier.images  = dossier.images,
                                       sep.images      = sep.images,
                                       inserer.images  = inserer.images,
                                       sep.formules    = sep.formules,
                                       sep.SMILES      = sep.SMILES,
                                       sep = sep, header = header, quote = quote,
                                       ... )
        l[[ f ]] <- d
    }

    ## On a terminé
    if ( TRUE == nv.fichier ) {
        finir_xml.moodle( fichier.xml )
    }

    ## On renvoie les bases de questions traitées...
    ##  (mais sans l'afficher : ne sert à rien)
    invisible( l )
}

csv.qcm_optique_vers_XML <- function( fichier.csv, fichier.xml, 
                                      embellir, deja.HTML, 
                                      forcer.multiple, melanger.reponses, somme.nulle,
                                      categorie.base,
                                      dossier.images, sep.images, inserer.images,
                                      sep.formules, sep.SMILES,
                                      sep = sep, header = header, quote = quote,
                                      ... )
{
    ## Vérification : fichier CSV
    if ( FALSE == file.exists( fichier.csv ) ) {
        erreur( 100, "csv.qcm_optique_vers_XML",
                "Fichier inexistant [", fichier.csv, "]" )
    }

    ## On charge le fichier
    d <- read.table( fichier.csv,
                     sep = sep, header = header, quote = quote,
                     stringsAsFactors = FALSE, ... )
    cat( "\n Nombre de questions d\u00e9tect\u00e9 : ",
         ncol( d ) - 1 )
    if ( ncol( d ) < 2 ) {
        erreur( 1500, "csv.qcm_optique_vers_XML",
                "Fichier CSV sans question ? (une colonne seulement...)" )
    }
    
    if ( nrow( d ) < 4 ) {
        erreur( 1501, "csv.qcm_optique_vers_XML",
                "Fichier CSV sans question ? (deux lignes seulement...)" )
    }
    
    ## On indique la catégorie : base + nom du fichier…
    base <- gsub( "\\", "/", fixed = TRUE, fichier.csv ) # Sous Windows, \ au lieu de / comme séparateur...
    base <- strsplit( base, "/" )[[ 1 ]]
    base <- base[ length( base ) ]
    categorie <- paste( categorie.base, base, sep = "/" )
    if ( "/" == substr( categorie, 1, 1 ) ) categorie <- substr( categorie, 2, nchar( categorie ) )
    cat( "\n Cat\u00e9gorie globale : ", categorie, "\n" )
    categorie.moodle( nom.categorie = categorie,
                      fichier.xml = fichier.xml )

    ## Le nombre (maximal) de réponses
    n.reponses <- ( nrow( d ) - 2 ) / 2

    ## La 1re colonne ne sert à rien
    ##  On procède colonne par colonne
    
    for( j in 2:ncol( d ) ) {
        ## 1re ligne « mot-clef » = le titre
        titre.question <- d[ 1, j ]

        ## 2e ligne « question » = l'énoncé de la question
        texte.question <- d[ 2, j ]

        ## les lignes impaires donnent les réponses possibles
        reponses <- d[ 2 * ( 0:( n.reponses - 1 ) ) + 3, j ]

        ## les lignes paires donnent leur justesse
        justes <- d[ 2 * ( 0:( n.reponses - 1 ) ) + 4, j ]

        ## On supprime les réponses manquantes
        ##   (manquantes ou vide, "")
        idx.mq <- which( is.na( reponses ) |
                         ( nchar( reponses ) == 1 ) )
        if ( length( idx.mq ) > 0 ) {
            reponses <- reponses[ idx.mq ]
            justes   <- justes  [ idx.mq ]
        }

        cat( "\n  [Q", j - 1, "] : ",
             length( reponses ), " r\u00e9ponses",
             sep  = "" )
    
        ## Et l'on crée la question
        bonnes    <- reponses[ which( justes %in% c( 'BONNE' ) ) ]
        mauvaises <- reponses[ which( justes %in% c( 'MAUVAISE' ) ) ]
        notes <- list( 'Bonnes'  = rep( 100 / length( bonnes ), length( bonnes ) ) )
        if ( somme.nulle ) {
            notes$Fausses <- rep( -100 / length( mauvaises ), length( mauvaises ) )
        } else {
            notes$Fausses <- rep( 0, length( mauvaises ) )
        }
        qcm.moodle( titre = titre.question,
                    texte = texte.question,
                    bonnes.reponses = bonnes,
                    mauvaises.reponses = mauvaises,
                    melanger = melanger.reponses,
                    unique = if ( forcer.multiple ) FALSE else ( length( bonnes ) == 1 ),
                    fractions = notes )
    }

    cat( "\n\n Conversion termin\u00e9e...\n" )

    ## On renvoie la data.frame, au cas où...
    d
}
