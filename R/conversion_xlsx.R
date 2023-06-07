## ——————————————————————————————————————————————————————————————————————
## Création de XML Moodle avec R
## © Emmanuel Curis — mars 2015, mai 2019
##
## Conversion d'un fichier XLSX de format standardisé
##   en un fichier XML importable par Moodle
##
## Sur une idée de Virginie LASSERRE, février 2016
## ——————————————————————————————————————————————————————————————————————
## Historique
##   9 avril 2023 : création du fichier, inspiré de conversion_ods.R
##
##  15 mai   2023 : avertissement si extension inattendue
##                  message d'erreur si fichier inexistant
##
##  18 mai   2023 : conversion stop → erreur
## ——————————————————————————————————————————————————————————————————————

## La fonction d'appel
##  Mêmes paramètres que csv.moodle
xlsx.moodle <- function( fichier.xlsx, onglet = NA,
                         colonne.texte  = NA, colonne.reponse = NA,
                         colonne.note   = NA, colonne.note_question = NA,
                         colonne.titre  = NA, colonne.code = NA, colonne.type = NA,
                         colonne.retour = NA, colonne.global = NA, colonne.penalite = NA,
                         colonne.temps  = NA, colonne.decimale = NA,
                         fichier.xml = if ( TRUE == nv.fichier ) gsub( "\\.[Xx][Ll][Ss][Xx]$",
                                                                       ".xml",
                                                                       fichier.xlsx )
                                       else get( "fichier.xml", envir = SARP.Moodle.env ),
                         nv.fichier = TRUE,
                         creer.titre = TRUE, lg.titre = 30, embellir = TRUE, deja.HTML = FALSE,
                         forcer.multiple = TRUE, melanger.reponses = TRUE, somme.nulle = FALSE,
                         precision = 3,
                         categorie.base = "",
                         dossier.images = dirname( fichier.xlsx ),
                         sep.images = c( '@@', '@@' ), inserer.images = TRUE,
                         sep.formules = c( '@\\$', '\\$@' ),
                         sep.SMILES = c( '@\\{', '\\}@' ),
                         ... ) {
    if ( requireNamespace( "openxlsx" ) == FALSE ) {
        erreur( 151, "xlsx.moodle",
                "Installez le package openxlsx",
                " pour convertir un fichier XLSX (Excel r\u00e9cent)",
                " en XML." )
    }

    ## Vérifications initiales
    verification_conversions( fichier.xml = fichier.xml, nv.fichier = nv.fichier,
                              inserer.images = inserer.images,
                              dossier.images = dossier.images )
    
    ## Si demandé : on crée le fichier XML
    if ( TRUE == nv.fichier ) {
        if ( any( is.na( fichier.xml ), length( fichier.xml ) != 1,
                  FALSE == is.character( fichier.xml ), nchar( fichier.xml ) < 1 ) ) {
            erreur( 7, "xlsx.moodle",
                    "Il faut indiquer un et un seul nom de fichier XML pour la sortie..." )
        }

        fichier.xml <- debuter_xml.moodle( fichier.xml )
    }

    ## Catégorie de base
    if ( missing( categorie.base ) ) categorie.base <- ""
    if ( any( is.na( categorie.base ),
              is.null( categorie.base ) ) )
        categorie.base <- ""
    
    ## On charge les fichiers XLSX indiqués à tour de rôle
    l <- as.list( rep( "", length( fichier.xlsx ) ) )
    names( l ) <- fichier.xlsx
    for ( f in fichier.xlsx ) {
        cat( "*** Traitement du fichier \u00ab", f, "\u00bb ***\n" )

        if ( FALSE == file.exists( f ) ) {
            erreur( 100, "xlsx.moodle",
                    "Fichier inexistant [", f, "]" )
        }
        
        ## Contrôle de l'extension
        extension <- strsplit( basename( f ), ".", fixed = TRUE )[[ 1 ]]
        extension <- extension[ length( extension ) ]

        if ( tolower( extension ) != "xlsx" ) {
            avertissement( 103, "xlsx.moodle",
                           "Extension inhabituelle pour un fichier xlsx - ",
                           extension )
        }
        
        ## On ouvre le fichier
        classeur <- openxlsx::loadWorkbook( f )
        
        ## Quels sont les onglets disponibles ?
        onglets <- names( classeur )
        n.onglets <- length( onglets )
        cat( "  ", n.onglets, " onglet", if ( n.onglets > 1 ) "s",
             " d\u00e9tect\u00e9", if ( n.onglets > 1 ) "s",
             sep = "" )

        for ( i in 1:n.onglets ) {
            if ( !is.na( onglet ) ) {
                if ( all( onglet != onglets[ i ],
                         onglet != i ) ) {
                    cat( " onglet ", i, " [", onglets[ i ], "]",
                         " ignor\u00e9", sep = "" )
                    next;
                }
            }
            
            
            ## On ouvre l'onglet du fichier dans une data.frame
            d <- openxlsx::read.xlsx( xlsxFile = classeur, sheet = i )
            cat( "  ", nrow( d ), " lignes lues...\n", sep = "" )

            ## On fait le traitement réel...
            d <- df.moodle( d = d,
                            colonne.texte    = colonne.texte,
                            colonne.reponse  = colonne.reponse,
                            colonne.note     = colonne.note,
                            colonne.note_question = colonne.note_question,
                            colonne.titre    = colonne.titre,
                            colonne.code     = colonne.code,
                            colonne.type     = colonne.type,
                            colonne.retour   = colonne.retour,
                            colonne.global   = colonne.global,
                            colonne.penalite = colonne.penalite,
                            colonne.temps    = colonne.temps,
                            colonne.decimale = colonne.decimale,
                            fichier.xml      = fichier.xml, 
                            creer.titre      = creer.titre,
                            lg.titre         = lg.titre   ,
                            embellir         = embellir   ,
                            deja.HTML        = deja.HTML  ,
                            forcer.multiple  = forcer.multiple,
                            melanger.reponses = melanger.reponses,
                            somme.nulle      = somme.nulle,
                            precision        = precision,
                            categorie.base   = categorie.base,
                            dossier.images   = dossier.images,
                            sep.images       = sep.images,
                            inserer.images   = inserer.images,
                            sep.formules     = sep.formules,
                            sep.SMILES       = sep.SMILES,
                            nom.fichier      = f )
        }
        
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
