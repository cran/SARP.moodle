## ──────────────────────────────────────────────────────────────────────
## Création de XML Moodle avec R
## © Emmanuel Curis — mars 2015, février 2016
##
## Conversion d'un fichier CSV de format standardisé
##   en un fichier XML importable par Moodle
##
## Sur une idée de Virginie LASSERRE, février 2016
## ─────────────────────────────────────────────────────────────────
## Historique
##
##   9 février 2017 : début de l'historique (…)
##                    ajouté une option en ligne de commande : mélange des réponses…
##                    préparé pour une colonne de type de question (QCM/QCU)
##
##   1 avril   2017 : corrigé la sélection de type par la colonne Type / QCU
##                    (était ignoré avant, voire risque d'erreur « objet inexistant »)
## ──────────────────────────────────────────────────────────────────────

## La seule fonction visible de l'extérieur...
##
## fichier.csv = le nom du fichier csv à convertir [character]
## fichier.xml = le fichier contenant les questions formatées
##               soit le nom d'un fichier à créer,
##               soit un fichier XML en cours d'usage
## nv.fichier  = si TRUE, on crée un fichier XML en entier...
## lg.titre    = nombre de caractères de la question à garder pour générer son titre
##               ignoré si une colonne de titres est fournie
##    [Repérage des colonnes...
##     Chaque colonne peut être indiquée par son nom [Attention aux accents, codes spéciaux...]
##                                        ou son numéro
##     Colonnes manquantes : mettre NA, NULL, "", <0....
##     Les colonnes des textes et des réponses ne peuvent pas être manquantes]
## colonne.texte   = la colonne qui contient les textes des énoncés
## colonne.reponse = la colonne qui contient la ou les textes des réponses
## colonne.note    = la colonne qui contient la note associée à chaque réponse
## colonne.titre   = la colonne qui contient le titre des questions (NA : aucune)
## colonne.code    = la colonne qui contient le code de la question (NA : aucune)
## colonne.type    = la colonne qui contient le type de question (NA : aucune)
## colonne.retour  = la colonne qui contient le texte du commentaire à afficher en cas de (mauvaise) réponse
## colonne.global  = la colonne qui contient le texte du commentaire à afficher après résolution
##
## creer.titre : si TRUE, on construit un titre à la question
## embellir    : si TRUE, on modifie les textes pour enjoliver la présentation
## deja.HTML   : si TRUE, le texte contient du code HTML : ne pas protéger les balises !
## forcer.multiple   : si TRUE, les QCM sont à choix multiples même s'il n'y a qu'une seule réponse
##                      (Sauf si le type de question force à être un QCU)
## melanger.reponses : si TRUE, les réponses sont à mélanger par Moodle
## categorie.base    : nom de la catégorie de base où stocker les questions
## 
## sep, header,... : options passées à read.table
## 
csv.moodle <- function( fichier.csv,
                        colonne.texte = 'Question',
                        colonne.reponse = 'R\u00e9ponse', colonne.note = NA,
                        colonne.titre = NA, colonne.code = NA, colonne.type = NA,
                        colonne.retour = NA, colonne.global = NA,
                        fichier.xml = if ( TRUE == nv.fichier ) gsub( "\\.[Cc][Ss][Vv]$", ".xml", fichier.csv )
                                      else get( "fichier.xml", envir = SARP.Moodle.env ),
                        nv.fichier = TRUE,
                        creer.titre = TRUE, lg.titre = 30, embellir = TRUE, deja.HTML = FALSE,
                        forcer.multiple = TRUE, melanger.reponses = TRUE,
                        categorie.base = "",
                        sep = ";", header = TRUE, quote = '"',
                        ... ) {
    ## Vérification : fichier XML
    ##   [On ne peut pas utiliser « missing » pour les colonnes avec une valeur par défaut :
    ##                                        renvoie « TRUE » si non-spécifié sur la ligne de commande...]
    if ( all( FALSE == is.character( fichier.xml ),
              FALSE == ( "file" %in% ( fichier.xml ) ) ) ) {
        stop( "Le fichier XML doit \u00eatre un nom de fichier \u00e0 cr\u00e9er",
              " ou un fichier d\u00e9j\u00e0 ouvert par debuter_xml.moodle" )
    }

    ## On harmonise les colonnes absentes : NA
    if ( any( is.na( colonne.texte ), is.null( colonne.texte ),
              ( is.integer( colonne.texte ) && ( colonne.texte < 1 ) ),
              ( is.character( colonne.texte ) && ( nchar( colonne.texte ) < 1 ) ) ) ) {
        stop( "Vous devez indiquer quelle colonne contient le texte des questions" )
    }
              
    if ( any( is.na( colonne.reponse ), is.null( colonne.reponse ),
              ( is.integer( colonne.reponse ) && ( colonne.reponse < 1 ) ),
              ( is.character( colonne.reponse ) && ( nchar( colonne.reponse ) < 1 ) ) ) ) {
        stop( "Vous devez indiquer quelle colonne contient le texte des r\u00e9ponses" )
    }
    
    if ( any( is.null( colonne.titre ),
              ( is.integer( colonne.titre ) && ( colonne.titre < 1 ) ),
              ( is.character( colonne.titre ) && ( nchar( colonne.titre ) < 1 ) ) ) ) colonne.titre <- NA

    if ( any( is.null( colonne.note ),
              ( is.integer( colonne.note ) && ( colonne.note < 1 ) ),
              ( is.character( colonne.note ) && ( nchar( colonne.note ) < 1 ) ) ) ) colonne.note <- NA

    if ( any( is.null( colonne.code ),
              ( is.integer( colonne.code ) && ( colonne.code < 1 ) ),
              ( is.character( colonne.code ) && ( nchar( colonne.code ) < 1 ) ) ) ) colonne.code <- NA

    if ( any( is.null( colonne.type ),
              ( is.integer( colonne.type ) && ( colonne.type < 1 ) ),
              ( is.character( colonne.type ) && ( nchar( colonne.type ) < 1 ) ) ) ) colonne.type <- NA

    if ( any( is.null( colonne.retour ),
              ( is.integer( colonne.retour ) && ( colonne.retour < 1 ) ),
              ( is.character( colonne.retour ) && ( nchar( colonne.retour ) < 1 ) ) ) ) colonne.retour <- NA

    if ( any( is.null( colonne.global ),
              ( is.integer( colonne.global ) && ( colonne.global < 1 ) ),
              ( is.character( colonne.global ) && ( nchar( colonne.global ) < 1 ) ) ) ) colonne.global <- NA

    ## Si demandé : on crée le fichier XML
    if ( TRUE == nv.fichier ) {
        if ( any( is.na( fichier.xml ), length( fichier.xml ) != 1,
                  FALSE == is.character( fichier.xml ), nchar( fichier.xml ) < 1 ) ) {
            stop( "Il faut indiquer un et un seul nom de fichier XML pour la sortie..." )
        }

        fichier.xml <- debuter_xml.moodle( fichier.xml )
    }
    
    ## On charge les fichiers CSV indiqués à tour de rôle...
    l <- as.list( rep( "", length( fichier.csv ) ) )
    names( l ) <- fichier.csv
    for ( f in fichier.csv ) {
        cat( "*** Traitement du fichier \u00ab ", f, " \u00bb ***\n" )

        ## On fait le traitement réel...
        d <- csv_vers_moodle( fichier.csv = f,
                              colonne.texte   = colonne.texte,
                              colonne.reponse = colonne.reponse,
                              colonne.note    = colonne.note,
                              colonne.titre   = colonne.titre,
                              colonne.code    = colonne.code,
                              colonne.type    = colonne.type,
                              colonne.retour  = colonne.retour,
                              colonne.global  = colonne.global,
                              fichier.xml     = fichier.xml, 
                              creer.titre     = creer.titre,
                              lg.titre        = lg.titre   ,
                              embellir        = embellir   ,
                              deja.HTML       = deja.HTML  ,
                              forcer.multiple = forcer.multiple,
                              melanger.reponses = melanger.reponses,
                              categorie.base  = categorie.base,
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

csv_vers_moodle <- function( fichier.csv,
                             colonne.texte, colonne.reponse, colonne.note,
                             colonne.titre, colonne.code, colonne.type,
                             colonne.retour, colonne.global,
                             fichier.xml,
                             creer.titre, lg.titre, embellir, deja.HTML,
                             forcer.multiple, melanger.reponses,
                             categorie.base,
                             sep, header, quote,
                             ... ) {
    ## On ouvre le fichier dans une data.frame
    d <- read.table( file = fichier.csv,
                     stringsAsFactors = FALSE, # on ne convertit pas en facteur : superflu et complique...
                     sep = sep, header = header, quote = quote, ... )
    cat( "  ", nrow( d ), " lignes lues...\n", sep = "" )

    ## On récupère les noms des colonnes…
    noms.colonnes <- names( d )

    ## On essaye de détecter des colonnes facultatives…
    ##  1) colonne des notes
    if ( all( any( "note" %in% tolower( noms.colonnes ) ), is.na( colonne.note ) ) ) {
        colonne.note <- which( tolower( noms.colonnes ) %in% "note" )[ 1 ]
        cat( "  Colonne de note d\u00e9tect\u00e9e : colonne ", colonne.note,
             " [", noms.colonnes[ colonne.note ], "]\n", sep = "" )
    }

    ##  2) colonne des types de question
    if ( all( any( "type" %in% tolower( noms.colonnes ) ), is.na( colonne.type ) ) ) {
        colonne.type <- which( tolower( noms.colonnes ) %in% "type" )[ 1 ]
        cat( "  Colonne de type d\u00e9tect\u00e9e : colonne ", colonne.type,
             " [", noms.colonnes[ colonne.type ], "]\n", sep = "" )
    }


    ## … et on repère celles indiquées, si ce ne sont pas des nombres.
    ##   (remarque : si absente, NA : is.integer vaut FALSE
    ##                                sauf si NA_integer_ mais alors noms.colonnes[ ] renvoie NA
    ##    ==> devrait fonctionner...)
    if ( is.integer( colonne.texte   ) ) colonne.texte   <- noms.colonnes[ colonne.texte   ]
    if ( is.integer( colonne.reponse ) ) colonne.reponse <- noms.colonnes[ colonne.reponse ]
    if ( is.integer( colonne.note    ) ) colonne.note    <- noms.colonnes[ colonne.note    ]
    if ( is.integer( colonne.titre   ) ) colonne.titre   <- noms.colonnes[ colonne.titre   ]
    if ( is.integer( colonne.code    ) ) colonne.code    <- noms.colonnes[ colonne.code    ]
    if ( is.integer( colonne.type    ) ) colonne.type    <- noms.colonnes[ colonne.type    ]
    if ( is.integer( colonne.retour  ) ) colonne.retour  <- noms.colonnes[ colonne.retour  ]
    if ( is.integer( colonne.global  ) ) colonne.global  <- noms.colonnes[ colonne.global  ]

    cat( "  Colonne des \u00e9nonc\u00e9s  : ", colonne.texte  , "\n" )
    cat( "  Colonne des r\u00e9ponses : ", colonne.reponse, "\n" )
    
    ## On vérifie qu'il n'y a pas de doublons
    if ( any( duplicated( na.omit( c( colonne.texte, colonne.reponse,
                                      colonne.note, colonne.titre,
                                      colonne.code, colonne.type,
                                      colonne.retour, colonne.global ) ) ) ) ) {
        stop( "Une m\u00eame colonne ne peut pas servir \u00e0 deux informations distinctes !" )
    }

    ## On remplace les textes vides par des NA, plus faciles à repérer
    if ( any( nchar( d[ , colonne.texte ] ) < 1 ) )
        d[ which( nchar( d[, colonne.texte ] ) < 1 ) , colonne.texte ] <- NA

    ## On introduit un code interne unique...
    d$I_Code.interne <- NA

    ## On repère les codes/textes remplis (au moins deux caractères...)
    if ( is.na( colonne.code ) ) {
        idx.q <- which( ( nchar( d[ , colonne.texte ] ) > 2 ) & ( !is.na( d[ , colonne.texte ] ) ) )
    } else {
        idx.q <- which( ( nchar( d[ , colonne.code  ] ) > 1 ) & ( !is.na( d[ , colonne.code  ] ) ) )
    }
    n.existe <- length( idx.q )
    cat( " ", n.existe, " questions distinctes d\u00e9tect\u00e9es.\n" )
    codes <- paste0( "[Q", formatC( 1:n.existe, width = 1 + as.integer( log( n.existe, 10 ) ),
                                    flag = "0", format = "d" ), "]" )
    d$I_Code.interne[ idx.q ] <- codes

    ## On crée les titres des questions, s'il n'existent pas déjà…
    if ( is.na( colonne.titre ) ) {
        if ( TRUE == creer.titre ) {
            ## On crée les titres à partir du code
            ##   et du début de la question
            d$Titres <- paste0( d[ , if ( is.na( colonne.code ) ) 'I_Code.interne' else colonne.code ],
                                " ",
                                substr( d[ , colonne.texte ], 1, lg.titre ) )
        } else {
            ## On indique juste le code interne...
            d$Titres <- paste0( fichier.csv, " : ", d$I_Code.interne )
        }
        colonne.titre <- "Titres"
    }

    ## Existe-t-il des énoncés sans texte (vide, NA...) ?
    ##   (si oui : question à réponse multiple…)
    idx.na <- which( is.na( d$I_Code.interne ) )
    if ( length( idx.na ) > 0 ) {
        for( i in idx.na ) {
            d$I_Code.interne[ i ] <- d$I_Code.interne[ i - 1 ]
        }
    }
    ## ==> toutes les réponses d'une même question ont le même code interne…

    ## On indique la catégorie : base + nom du fichier…
    base <- gsub( "\\", "/", fixed = TRUE, fichier.csv ) # Sous Windows, \ au lieu de / comme séparateur…
    base <- strsplit( base, "/" )[[ 1 ]]
    base <- base[ length( base ) ]
    categorie <- paste( categorie.base, base, sep = "/" )
    if ( "/" == substr( categorie, 1, 1 ) ) categorie <- substr( categorie, 2, nchar( categorie ) )
    cat( "\n Cat\u00e9gorie globale : ", categorie, "\n\n" )
    categorie.moodle( nom.categorie = categorie,
                      fichier.xml = fichier.xml )
    
    ## On travaille par question…
    for( q in codes ) {
        cat( " Conversion de la question ", q, "\n" )
        dq <- d[ which( d$I_Code.interne == q ), ]

        convertir_question( question = dq,
                            colonne.texte   = colonne.texte,
                            colonne.reponse = colonne.reponse,
                            colonne.note    = colonne.note,
                            colonne.titre   = colonne.titre,
                            colonne.type    = colonne.type,
                            colonne.retour  = colonne.retour,
                            colonne.global  = colonne.global,
                            fichier.xml     = fichier.xml,
                            embellir        = embellir,
                            deja.HTML       = deja.HTML,
                            forcer.multiple = forcer.multiple,
                            melanger.reponses = melanger.reponses,
                            categorie.base  = categorie.base )
    }
    
    ## On renvoie la base des questions...
    d
}

convertir_question <- function( question,
                                colonne.texte, colonne.reponse, colonne.note,
                                colonne.titre, colonne.type,
                                colonne.retour, colonne.global,
                                fichier.xml,
                                embellir, deja.HTML,
                                forcer.multiple, melanger.reponses,
                                categorie.base ) {
    ## Le code [interne] de la question
    code.interne <- unique( question$I_Code.interne )
    if ( length( code.interne ) != 1 ) {
        ## Problème
        stop( "Probl\u00e8me : question avec double code interne [",
              paste0( code.interne, collapse = ", " ), "]" )
    }

    ## Le type de la question, s'il existe
    if ( FALSE == is.na( colonne.type ) ) {
        type.question <- question[ , colonne.type ]
        
        ## Harmonisation : minuscule, pas d'espace, pas de point
        type.question <- tolower( type.question )
        type.question <- gsub( ".", "", fixed = TRUE, type.question )
        type.question <- gsub( "[[:space:]]", "", fixed = FALSE, type.question )

        ## Suppression des types vides
        if ( any( is.na( type.question ) ) ) {
            type.question <- type.question[ -which( is.na( type.question ) ) ]
        }
        if ( "" %in% type.question ) {
            type.question <- type.question[ -which( type.question == "" ) ]
        }

        ## On ne garde qu'une seule valeur…
        type.question <- unique( type.question )
        if ( length( type.question ) > 1 ) {
            ## Problème
            stop( "Probl\u00e8me : question avec type multiple ou absent [",
                  paste0( code.interne, collapse = ", " ), "]" )
        } else if ( length( type.question ) == 0 ) {
            ## Si aucun type indiqué : devient un vecteur vide…
            ## ==> on bascule vers la détection automatique
            type.question <- NA
        }
    } else {
        type.question <- NA
    }
    if ( !is.na( type.question ) ) {
        cat( "  [Type de question impos\u00e9 :", type.question, "]\n" )
    }

    ## Combien de réponses à la question
    n.reponses <- nrow( question )

    ###################################
    ##
    ##  Cas des questions à réponse unique
    ## 
    if ( 1 == n.reponses ) {
        ## On récupère la réponse, et on analyse sa nature...
        reponse <- question[ 1, colonne.reponse ]
        
        ## Cas 1 : aucune réponse indiquée
        ##  ==> on change de catégorie
        if ( any( is.na( reponse ), nchar( reponse ) < 1 ) ) {
            categorie <- paste( categorie.base, question[ 1, colonne.texte ],
                                sep = "/" )
            if ( "/" == substr( categorie, 1, 1 ) ) categorie <- substr( categorie, 2, nchar( categorie ) )
            cat( "  Nouvelle cat\u00e9gorie :", categorie,
                 "\n\n" )
            categorie.moodle( nom.categorie = categorie,
                              fichier.xml = fichier.xml )
            return( categorie )
        }

        ## On regarde s'il y a un commentaire...
        
        ## Cas 2 : réponse numérique
        ##   (en évitant des avertissements superflus)
        reponse.n <- suppressWarnings( as.double( reponse ) )
        if ( is.na( reponse.n ) )
            reponse.n <- suppressWarnings( as.double( gsub( ",", ".", fixed = TRUE, reponse ) ) )
        if ( is.finite( reponse.n ) ) {
            cat( "  Question simple, r\u00e9ponse num\u00e9rique :",
                 reponse.n, "\n" )

            numerique.moodle( texte = question[ 1, colonne.texte ],
                              bonne.reponse = reponse.n,
                              titre = question[ 1, colonne.titre ],
                              fichier.xml = fichier.xml )
            return( reponse.n )
        }

        ## Cas 3 : réponse logique
        tmp <- tolower( reponse )
        reponse.l <- if ( tmp %in% c( 'v', 'vrai', 't', 'true'  ) ) TRUE  else
                     if ( tmp %in% c( 'f', 'faux', 'f', 'false' ) ) FALSE else NA
        if ( FALSE == is.na( reponse.l ) ) {
            cat( "  Affirmation, elle est ", if ( reponse.l ) "vraie" else "fausse", "\n" )
            vrai_faux.moodle( texte = question[ 1, colonne.texte ],
                              texte.vrai = if (  reponse.l ) "Vrai" else "Faux",
                              texte.faux = if ( !reponse.l ) "Vrai" else "Faux",
                              titre = question[ 1, colonne.titre ],
                              melanger = melanger.reponses,
                              fichier.xml = fichier.xml )
            return( reponse.l )
        }

        ## Cas 4 : réponse ouverte « QROC »
        cat( "  Question simple, r\u00e9ponse textuelle unique : ",
             reponse, "\n" )
        qroc.moodle( texte = question[ 1, colonne.texte ],
                     reponses = reponse,
                     titre = question[ 1, colonne.titre ],
                     fichier.xml = fichier.xml )
        return( reponse )
    }

    ## 
    ## Cas des questions à réponses multiples
    ## 

    ## Cas 1 : un seul énoncé, QCM
    if ( length( na.omit( question[ , colonne.texte ] ) ) == 1 ) {
        cat( "  QCM,", n.reponses, "r\u00e9ponses possibles\n" )

        if ( is.na( colonne.note ) ) {
            stop( "Pr\u00e9cisez une colonne avec les notes pour cr\u00e9er des questions \u00e0 choix multiples" )
        }

        ## On construit les notes...
        question[ , colonne.note ] <- tolower( question[ , colonne.note ] )
        question[ , colonne.note ] <- gsub( ",", ".", fixed = TRUE, question[ , colonne.note ] )
        question[ which( question[ , colonne.note ] %in% c( "f", "false", "faux" ) ), colonne.note ] <- 0
        nb.vrai <- length( which( question[ , colonne.note ] %in% c( "v", "true", "t", "vrai" ) ) )
        if ( nb.vrai > 0 ) {
            question[ which( question[ , colonne.note ] %in% c( "v", "true", "t", "vrai" ) ), colonne.note ] <- 100 / nb.vrai
        }
        question[ , colonne.note ] <- as.numeric( question[ , colonne.note ] )

        ## On vérifie qu'elles sont entre 0 et 100
        ##   Remarque : plus basse note possible dans moodle : 5 % (soit 0,05),
        ##               donc si 0 < |valeur| ≤ 1 c'est que l'on a donné une note fractionnaire et pas un pourcentage…
        idx <- which( ( abs( question[ , colonne.note ] ) <= 1 ) & ( question[ , colonne.note ] != 0 ) )
        if ( length( idx ) > 0 ) {
            warning( "Attention, des notes comprises entre 0 et 1 ont \u00e9t\u00e9 converties en pourcentages" )
            question[ idx , colonne.note ] <- 100.0 * question[ idx , colonne.note ]
        }
 
        ## On distingue les bonnes et les mauvaises réponses
        bonnes <- which( question[ , colonne.note ] > 0 )
        mauvaises <- setdiff( 1:n.reponses, bonnes )

        ## Y a-t-il une seule bonne réponse, ou plusieurs ?
        reponse.unique <- FALSE
        if ( FALSE == is.na( type.question ) ) {
            reponse.unique <- ( type.question == "qcu" )
        }
        reponse.unique <- any( length( bonnes )           ==   1, # Oui si une seule réponse avec une note > 0
                               question[ , colonne.note ] == 100, # ou si une réponse apporte 100 % des points
                               na.rm = TRUE )
        if ( all( !is.na( type.question ),
                  type.question == "qcu" ) ) {
            if ( FALSE == reponse.unique ) {
                stop( "Incoh\u00e9rence : QCU demand\u00e9,",
                      " mais plusieurs bonnes r\u00e9ponses indiqu\u00e9es !\n",
                      " Conversion interrompue." )
            }
            forcer.multiple <- FALSE;    # On empêche la conversion en multiple...
        }

        ## Et on crée la question...
        qcm.moodle( texte = question[ 1, colonne.texte ],
                    bonnes.reponses    = question[ bonnes   , colonne.reponse ],
                    mauvaises.reponses = question[ mauvaises, colonne.reponse ],
                    fractions = list( "Bonnes"  = question[ bonnes, colonne.note ],
                                      "Fausses" = question[ mauvaises, colonne.note ] ),
                    titre = question[ 1, colonne.titre ],
                    unique = if ( forcer.multiple ) FALSE else reponse.unique,
                    melanger = melanger.reponses,
                    fichier.xml = fichier.xml )

        return( c( bonnes, mauvaises ) )
    }

    ## Cas 2 : question « cloze »
    warning( "Cas des questions \u00ab cloze \u00bb pas encore v\u00e9rifi\u00e9" )
    cat( "  Question Cloze :\n" )
    
    ## a) les textes avant les champs
    textes.avant <- na.omit( question[ , colonne.texte ] )

    texte.final <- ""
    idx.final <- which( is.na( question[ , colonne.reponse ] ) )
    if ( length( idx.final ) > 0  ) {
        texte.final <- paste( question[ idx.final, colonne.texte ],
                              collapse = " " )

        textes.avant <- textes.avant[ -idx.final ]
    }

    ## b) les réponses pour chaque champ
    n.champs <- length( textes.avant )
    types <- rep( NA, n.champs )
    reponses <- list()
    for( i in 1:n.champs ) {
        reponse <- question[ , colonne.reponse ][ i ]

        if ( is.na( reponse ) ) {
            texte.final
            textes.avant[ i ]
            break;
        }
        
        reponse.n <- suppressWarnings( as.double( reponse ) )
        if ( is.na( reponse.n ) )
            reponse.n <- suppressWarnings( as.double( gsub( ",", ".",
                                                            fixed = TRUE,
                                                            reponse ) ) )
        if ( is.finite( reponse.n ) ) {
            cat( "   champ", i, ": num\u00e9rique, ", reponse.n, "\n" )

            types[ i ] <- "NUMERICAL"
            reponses[[ i ]] <- reponse.n
        } else {
            cat( "   champ", i, ": ouvert, \"", reponse, "\"\n" )

            types[ i ] <- "SHORTANSWER"
            reponses[[ i ]] <- list( "Textes" = reponse,
                                     "Correct" = TRUE )
        }
    }

    question_libre.moodle( texte.intro  = "",
                           textes.avant = textes.avant,
                           texte.final  = texte.final,
                           reponses = reponses, types = types,
                           fichier.xml = fichier.xml )
    return( "\u00ab cloze \u00bb" )
}
