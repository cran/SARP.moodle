## ——————————————————————————————————————————————————————————————————————
## Création de XML Moodle avec R
## © Emmanuel Curis — mars 2015, mai 2019
##
## Conversion d'un fichier CSV de format standardisé
##   en un fichier XML importable par Moodle
##
## Sur une idée de Virginie LASSERRE, février 2016
## ——————————————————————————————————————————————————————————————————————
## Historique
##
##   9 février 2017 : début de l'historique (…)
##                    ajouté une option en ligne de commande : mélange des réponses…
##                    préparé pour une colonne de type de question (QCM/QCU)
##
##   1 avril   2017 : corrigé la sélection de type par la colonne Type / QCU
##                    (était ignoré avant, voire risque d'erreur « objet inexistant »)
##
##  15 mai     2019 : possibilité d'avoir une somme des points nulle sur un QCM
##                    (option somme.nulle = TRUE)
##
##  15 nov.    2019 : la colonne des commentaires globaux est prise en compte
##                     [erreur signalée par Céline Bugli ce jour...]
##
##  19 avril   2020 : premiers essais d'image et de formules dans le texte
##                     => les formules semblent fonctionner (avec @$, $@)
##                    détection automatique d'une colonne code de question
##                    correction de la détection des questions si colonne de code
##
##  20 avril   2020 : syntaxe pour contrôler la taille des images
##
##  21 avril   2020 : syntaxe pour générer des formules chimiques par code SMILE
##
##  26 avril   2020 : corrections mineures pour soumission au CRAN
##
##  27 avril   2020 : possibilité de préciser le nombre de décimales
## ——————————————————————————————————————————————————————————————————————

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
##     Colonnes manquantes : mettre NA, NULL, "", <0...
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
## somme.nulle       : si TRUE, les réponses fausses ont des points négatifs [QCM]
## precision         : le nombre de décimales à garder pour les réponses numériques
## 
## categorie.base    : nom de la catégorie de base où stocker les questions
## 
## dossier.images  : le dossier contenant les images à intégrer
## sep.images      : les codes à utiliser pour indiquer le début et à la fin d'une image à insérer
## sep.formules    : les codes à utiliser pour indiquer le début et à la fin d'une formule latex à insérer
## sep.SMILES      : les codes à utiliser pour indiquer le début et à la fin d'un code SMILES
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
                        forcer.multiple = TRUE, melanger.reponses = TRUE, somme.nulle = FALSE,
                        precision = 3,
                        categorie.base = "",
                        dossier.images = ".", sep.images = c( '@@', '@@' ), inserer.images = TRUE,
                        sep.formules = c( '@\\$', '\\$@' ),
                        sep.SMILES = c( '@\\{', '\\}@' ),
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

    ## On contrôle la précision
    if ( missing( precision ) ) precision <- 3
    if ( is.null( precision ) ) precision <- 3
    if ( length( precision ) > 1 ) {
        warning( "Seule la premi\u00e8re valeur de pr\u00e9cision sera utilis\u00e9e." )
        precision <- precision[ 1 ]
    }
    if ( !is.na( precision) & !is.finite( precision ) ) {
        stop( "La pr\u00e9cision doit \u00eatre un entier positif ou NA" )
    }
    precision <- as.integer( precision )
    
    ## On contrôle le dossier d'images
    if ( !is.logical( inserer.images ) ) {
        stop( "inserer.images doit valoir TRUE ou FALSE" )
    }
    if ( length( inserer.images ) != 1 ) {
        stop( "inserer.images ne doit avoir qu'une seule valeur, TRUE ou FALSE" )
    }
    
    if ( length( dossier.images ) > 1 ) {
        stop( "Un seul dossier contenant les images",
              " doit \u00eatre sp\u00e9cifi\u00e9" )
    }
    if ( length( dossier.images ) == 1 ) {
        if ( FALSE == dir.exists( dossier.images ) ) {
            stop( "Le dossier d'images indiqu\u00e9 n'existe pas." )
        }
    }

    ## On contrôle les séparateurs d'images
    if ( length( sep.images ) == 1 ) {
        sep.images <- c( sep.images, sep.images )
    }
    if ( length( sep.images ) > 2 ) {
        stop( "Il faut deux s\u00e9parateurs d'image au plus :",
              " pour le d\u00e9but et pour la fin." )
    }

    ## On contrôle les séparateurs de formules
    if ( length( sep.formules ) == 1 ) {
        sep.formules <- c( sep.formules, sep.formules )
    }
    if ( length( sep.formules ) > 2 ) {
        stop( "Il faut deux s\u00e9parateurs de formule au plus :",
              " pour le d\u00e9but et pour la fin." )
    }

    ## On contrôle les séparateurs de codes SMILES
    if ( length( sep.SMILES ) == 1 ) {
        sep.SMILES <- c( sep.SMILES, sep.SMILES )
    }
    if ( length( sep.SMILES ) > 2 ) {
        stop( "Il faut deux s\u00e9parateurs de code SMILES au plus :",
              " pour le d\u00e9but et pour la fin." )
    }

    
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
                              somme.nulle     = somme.nulle,
                              precision       = precision,
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

csv_vers_moodle <- function( fichier.csv,
                             colonne.texte, colonne.reponse, colonne.note,
                             colonne.titre, colonne.code, colonne.type,
                             colonne.retour, colonne.global,
                             fichier.xml,
                             creer.titre, lg.titre, embellir, deja.HTML,
                             forcer.multiple, melanger.reponses, somme.nulle,
                             precision,
                             categorie.base,
                             dossier.images, sep.images, inserer.images,
                             sep.formules, sep.SMILES,
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

    ##  3) colonne des codes de question
    if ( all( any( c( "code", "id" ) %in% tolower( noms.colonnes ) ), 
              is.na( colonne.code ) ) ) {
        colonne.code <- which( tolower( noms.colonnes ) %in% c( "code", "id" ) )[ 1 ]
        cat( "  Colonne de code d\u00e9tect\u00e9e : colonne ", colonne.code,
             " [", noms.colonnes[ colonne.code ], "]\n", sep = "" )
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

    cat( "  Colonne des \u00e9nonc\u00e9s      : ", colonne.texte  , "\n" )
    cat( "  Colonne des r\u00e9ponses     : "     , colonne.reponse, "\n" )
    
    ## On vérifie qu'il n'y a pas de doublons
    if ( any( duplicated( na.omit( c( colonne.texte, colonne.reponse,
                                      colonne.note, colonne.titre,
                                      colonne.code, colonne.type,
                                      colonne.retour, colonne.global ) ) ) ) ) {
        stop( "Une m\u00eame colonne ne peut pas servir \u00e0 deux informations distinctes !" )
    }

    ## On remplace les textes vides par des NA, plus faciles à repérer
    if ( any( nchar( d[ , colonne.texte ] ) < 1, na.rm = TRUE ) )
        d[ which( nchar( d[, colonne.texte ] ) < 1 ) , colonne.texte ] <- NA

    ## On introduit un code interne unique...
    d$I_Code.interne <- NA

    ## On repère les codes/textes remplis (au moins deux caractères...)
    if ( is.na( colonne.code ) ) {
        idx.q <- which( ( nchar( d[ , colonne.texte ] ) > 2 ) & ( !is.na( d[ , colonne.texte ] ) ) )
    } else {
        idx.q <- which( ( nchar( d[ , colonne.code  ] ) > 0 ) & ( !is.na( d[ , colonne.code  ] ) ) )
    }
    n.existe <- length( idx.q )
    cat( " ", n.existe, " question", if ( n.existe > 1 ) "s",
         " distincte", if ( n.existe > 1 ) "s",
         " d\u00e9tect\u00e9e", if ( n.existe > 1 ) "s", ".\n",
         sep = "")
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
    base <- gsub( "\\", "/", fixed = TRUE, fichier.csv ) # Sous Windows, \ au lieu de / comme séparateur...
    base <- strsplit( base, "/" )[[ 1 ]]
    base <- base[ length( base ) ]
    categorie <- paste( categorie.base, base, sep = "/" )
    if ( "/" == substr( categorie, 1, 1 ) ) categorie <- substr( categorie, 2, nchar( categorie ) )
    cat( "\n Cat\u00e9gorie globale : ", categorie, "\n\n" )
    categorie.moodle( nom.categorie = categorie,
                      fichier.xml = fichier.xml )

    ## On adapte les textes des questions : images...
    if ( all( length( dossier.images ) > 0, length( sep.images ) == 2 ) ) { 
        cat( " Conversion des images...\n" )
        
        d[ , colonne.texte   ] <- traiter_images( d[ , colonne.texte   ], 
                                                  dossier.images, sep.images, inserer.images ) # Énoncé
        d[ , colonne.reponse ] <- traiter_images( d[ , colonne.reponse ],
                                                 dossier.images, sep.images, inserer.images ) # Réponse
        if ( !is.na( colonne.retour ) ) {
            d[ , colonne.retour  ] <- traiter_images( d[ , colonne.retour  ],
                                                     dossier.images, sep.images, inserer.images ) # Commentaire systématique
        }
        if ( !is.na( colonne.global ) ) {
            d[ , colonne.global  ] <- traiter_images( d[ , colonne.global  ],
                                                      dossier.images, sep.images, inserer.images ) # Commentaire global si faux
        }
    }

    if ( all( length( sep.formules ) == 2 ) ) { 
        cat( " Conversion des formules...\n\n" )
        
        d[ , colonne.texte   ] <- traiter_formules( d[ , colonne.texte   ], sep.formules ) # Énoncé
        d[ , colonne.reponse ] <- traiter_formules( d[ , colonne.reponse ], sep.formules ) # Réponse
        if ( !is.na( colonne.retour ) ) {
            d[ , colonne.retour  ] <- traiter_formules( d[ , colonne.retour  ], sep.formules ) # Commentaire systématique
        }
        if ( !is.na( colonne.global ) ) {
            d[ , colonne.global  ] <- traiter_formules( d[ , colonne.global  ], sep.formules ) # Commentaire global si faux
        }
    }

    if ( all( length( sep.SMILES ) == 2 ) ) { 
        cat( " Conversion des codes SMILES...\n\n" )
        
        d[ , colonne.texte   ] <- traiter_SMILES( d[ , colonne.texte   ], sep.SMILES ) # Énoncé
        d[ , colonne.reponse ] <- traiter_SMILES( d[ , colonne.reponse ], sep.SMILES ) # Réponse
        if ( !is.na( colonne.retour ) ) {
            d[ , colonne.retour  ] <- traiter_SMILES( d[ , colonne.retour  ], sep.SMILES ) # Commentaire systématique
        }
        if ( !is.na( colonne.global ) ) {
            d[ , colonne.global  ] <- traiter_SMILES( d[ , colonne.global  ], sep.SMILES ) # Commentaire global si faux
        }
    }

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
                            somme.nulle     = somme.nulle,
                            precision       = precision,
                            categorie.base  = categorie.base,
                            dossier.images  = dossier.images,
                            sep.images      = sep.images,
                            sep.formules    = sep.formules )
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
                                forcer.multiple, melanger.reponses, somme.nulle,
                                precision = precision,
                                categorie.base,
                                dossier.images  = dossier.images,
                                sep.images      = sep.images,
                                sep.formules    = sep.formules )
{
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
        if ( all( length( type.question ) > 1, # Plus d'un type indiqué
                  length( unique( question[ , colonne.texte ] ) ) == 1 ) # Alors qu'il n'y a qu'une question )
            ) {
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

    ## Un commentaire global éventuel
    if ( FALSE == is.na( colonne.global ) ) {
        commentaire.global <- question[, colonne.global ]
        if ( any( is.na( commentaire.global ) ) ) {
            commentaire.global <- commentaire.global[ -which( is.na( commentaire.global ) ) ]
        }
        commentaire.global <- paste0( commentaire.global,
                                      collapse = "<br />\n" )
    } else {
        commentaire.global <- NA
    }

    
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

            if ( all( is.finite( precision ),
                      ( reponse - as.integer( reponse ) ) == 0 ) ) {
                ## Réponse apparemment entière : pas d'indication sur le nombre de décimales
                precision <- NA
            }
            
            numerique.moodle( texte = question[ 1, colonne.texte ],
                              bonne.reponse = reponse.n,
                              titre = question[ 1, colonne.titre ],
                              n.decimales = precision,
                              fichier.xml = fichier.xml,
                              commentaire.global = commentaire.global )
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
                              fichier.xml = fichier.xml,
                              commentaire.global = commentaire.global )
            return( reponse.l )
        }

        ## Cas 4 : réponse ouverte « QROC »
        cat( "  Question simple, r\u00e9ponse textuelle unique : ",
             reponse, "\n" )
        qroc.moodle( texte = question[ 1, colonne.texte ],
                     reponses = reponse,
                     titre = question[ 1, colonne.titre ],
                     fichier.xml = fichier.xml,
                     commentaire.global = commentaire.global )
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
        ## reponse.unique <- FALSE
        ## if ( FALSE == is.na( type.question ) ) {
        ##     reponse.unique <- ( type.question == "qcu" )
        ## }
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

        ## Veut-on un QCM à somme nulle (ie, si l'étudiant coche tout, il a 0)
        if ( all( TRUE == somme.nulle,
                  length( mauvaises ) > 0 ) ) {
            ## Oui => on met des points négatifs aux mauvaises réponses en fonction
            question[ mauvaises, colonne.note ] <- - 100 / length( mauvaises )
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
                    fichier.xml = fichier.xml,
                    commentaire.global = commentaire.global )

        return( c( bonnes, mauvaises ) )
    }

    ## Cas 2 : question « cloze »
    warning( "Cas des questions \u00ab cloze \u00bb pas encore v\u00e9rifi\u00e9" )
    cat( "  Question Cloze :\n" )
    
    ## a) les textes avant les champs
    textes.avant <- na.omit( question[ , colonne.texte ] )

    texte.final <- ""
    idx.final <- which( is.na( question[ , colonne.reponse ] ) |
                        ( nchar( question[ , colonne.reponse ] ) < 1 ) )
    if ( length( idx.final ) > 0  ) {
        texte.final <- paste( question[ idx.final, colonne.texte ],
                              collapse = " " )

        textes.avant <- textes.avant[ -idx.final ]

        n.reponses <- n.reponses <- length( idx.final )
    }

    ## b) les réponses pour chaque champ
    n.champs <- length( textes.avant )
    choix.multiple <- ( n.champs < n.reponses )
    if ( n.champs > n.reponses ) {
        stop( "Format de question cloze incorrect : plus de champs que de r\u00e9ponses !" )
    }
    types <- rep( NA, n.champs )
    reponses <- list()
    i.ligne <- 1
    for( i in 1:n.champs ) {
        reponse <- question[ , colonne.reponse ][ i.ligne ]
        if ( is.na( reponse ) ) {
            ## On est sur le texte final, normalement...
            texte.final
            textes.avant[ i ]
            break;
        }

        ## Est-ce que l'on commence une question à réponses multiples ?
        if ( all( choix.multiple, 
                  i.ligne < nrow( question ),
                  is.na( question[ , colonne.texte ][ i.ligne + 1 ] ) ) ) {
            ## Si la ligne suivante n'a pas de question : oui

            ## On repère les index des lignes des réponses
            ##  1) index de la ligne commençant le champ suivant
            idx.suivant <- which( !is.na( question[ (i.ligne + 1):n.reponses, colonne.texte ] ) )
            if ( length( idx.suivant ) > 0 ) {
                idx.suivant <- idx.suivant[ 1 ]
            } else {
                idx.suivant <- nrow( question ) + 1 - i.ligne
            }
                
            ##  2) index des lignes d'ici là
            ##     attention, pour idx.suivant, 1 = 2e réponse
            idx.reponses <- i.ligne + ( 0:( idx.suivant - 1 ) )
            
            ## Question à réponse multiple
            correct <- rep( TRUE, length( idx.reponses ) )
            if ( !is.na( colonne.note ) ) {
                notes <- tolower( question[ idx.reponses, colonne.note ] )
                correct[ which( notes %in% c( "f", "false", "faux" ) ) ] <- FALSE
            }
            multi.reponses <- list( "Textes" = question[ idx.reponses, colonne.reponse ],
                                    "Correct" = correct )

            ## Le type : par défaut, SHORTANSWER
            types[[ i ]] <- "SHORTANSWER"
            if ( !is.na( colonne.type ) ) {
                type.champ <- tolower( question[ i.ligne, colonne.type ] )
                if ( type.champ %in% c( 'm', 'mc', 'multi', 'multichoice' ) ) {
                    types[[ i ]] <- "MULTICHOICE"
                } else if ( type.champ %in% c( 'ms', 'mcs', 'multichoice_s', 's', 'sm' ) ) {
                    types[[ i ]] <- "MULTICHOICE_S"
                } else if ( type.champ %in% c( 'h', 'mch', 'multichoice_h' ) ) {
                    types[[ i ]] <- "MULTICHOICE_H"
                } else if ( type.champ %in% c( 'hs', 'mchs', 'multichoice_hs', 'sh' ) ) {
                    types[[ i ]] <- "MULTICHOICE_HS"
                } else if ( type.champ %in% c( 'v', 'mcv', 'multichoice_v' ) ) {
                    types[[ i ]] <- "MULTICHOICE_V"
                } else if ( type.champ %in% c( 'vs', 'mcvs', 'multichoice_vs', 'sv' ) ) {
                    types[[ i ]] <- "MULTICHOICE_VS"
                } else if ( type.champ %in% c( 'sa', 'as', 'shortanswer' ) ) {
                    types[[ i ]] <- "SHORTANSWER"
                } else if ( type.champ %in% c( 'sc', 'sac', 'shortanswer_c', 'cs', 'cas', 'acs', 'asc', 'csa', 'sca' ) ) {
                    types[[ i ]] <- "SHORTANSWER_C"
                } else {
                    warnings( "Type de champ inconnu [", type.champ, "], ignor\u00e9" )
                }
            }

            ## On prépare ce champ
            reponses[[ i ]] <- multi.reponses

            ## On passe au champ suivant
            i.ligne <- i.ligne + idx.suivant
        } else {
            ## Question à réponse unique
            reponse.n <- suppressWarnings( as.double( reponse ) )
            if ( is.na( reponse.n ) ) {
                reponse.n <- suppressWarnings( as.double( gsub( ",", ".",
                                                               fixed = TRUE,
                                                               reponse ) ) )
            }
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
            ## Le champ suivant est à la ligne suivante
            i.ligne <- i.ligne + 1
        }   
    }

    question_libre.moodle( texte.intro  = "",
                           textes.avant = textes.avant,
                           texte.final  = texte.final,
                           reponses = reponses, types = types,
                           fichier.xml = fichier.xml,
                           commentaire.global = commentaire.global )
    return( "\u00ab cloze \u00bb" )
}

## ——————————————————————————————————————————————————————————————————————
##
##                       Prétraitement des textes
##
## ——————————————————————————————————————————————————————————————————————

## ——————————————————————————————————————————————————————————————————————
##
## [Prétraitement] Insertion des images
##

traiter_images <- function( textes, dossier.images, sep.images, inserer.images )
{
    ## Le masque de recherche
    masque <- paste0( sep.images[ 1 ], "(.*?)", sep.images[ 2 ] )
    
    ## On travaille texte par texte : plus simple
    for ( i in 1:length( textes ) ) {
        texte <- textes[ i ]

        av.images <- grepl( sep.images[ 1 ], fixed = TRUE, texte )
        if ( TRUE == av.images ) {
            pos.images <- gregexpr( masque, fixed = FALSE, texte )[[ 1 ]]
            lg <- attr( pos.images, "match.length" )

            images <- lapply( 1:length( lg ),
                              function( j ) {
                                  ## On récupère le lien de l'image
                                  img <- substr( texte,
                                                 start = pos.images[ j ],
                                                 stop = pos.images[ j ] + lg[ j ] - 1 )

                                  ## On fait sauter les séparateurs
                                  img <- gsub( masque, '\\1', img )

                                  ## On distingue le nom de fichier et les indications de taille
                                  pos.sep <- gregexpr( "[:!]", img )[[ 1 ]]
                                  if ( -1 == pos.sep ) {
                                      ## Pas d'indication de taille
                                      largeur <- hauteur <- -1
                                  } else {
                                      taille <- substr( img, pos.sep + 1, nchar( img ) )
                                      img <- substr( img, 1, pos.sep - 1 )

                                      largeur <- gsub( "^([[:digit:]]*)x.*", "\\1", taille )
                                      hauteur <- gsub( "^.*x([[:digit:]]*)", "\\1", taille )

                                      if ( nchar( largeur ) == 0 ) largeur <- -1
                                      if ( nchar( hauteur ) == 0 ) hauteur <- -1

                                      largeur <- as.integer( largeur )
                                      hauteur <- as.integer( hauteur )
                                  }
                                  
                                  ## On lie l'image
                                  img <- lier_image.moodle( paste0( dossier.images, "/", img ), 
                                                            largeur = largeur, hauteur = hauteur,
                                                            interne = inserer.images )

                                  ## On renvoie ce lien
                                  img
                              } )

            ## On va du dernier au premier, car l'insertion change les positions
            ##   => on modifie la fin, ne change pas les positions d'avant
            for ( j in seq( from = length( lg ), to = 1, by = -1 ) ) {
                texte <- paste0( substr( texte, 1, pos.images[ j ] - 1 ),
                                 images[[ j ]],
                                 substr( texte, pos.images[ j ] + lg[ j ], nchar( texte ) ) )
            }
        }
        
        textes[ i ] <- texte
    }

    ## On renvoie les textes traités
    textes
}

## ——————————————————————————————————————————————————————————————————————
##
## [Prétraitement] Insertion des formules latex
##

traiter_formules <- function( textes, sep.formules )
{
    ## Le masque de recherche
    masque <- paste0( sep.formules[ 1 ], "(.*?)", sep.formules[ 2 ] )
    
    ## On travaille texte par texte : plus simple
    for ( i in 1:length( textes ) ) {
        texte <- textes[ i ]

        ## Attention, avec le séparateur @$ choisi, $ doit être protégé par \
        ##   pour ne pas marquer la fin d'une chaîne
        ##  => on ne peut pas utiliser fixed = TRUE qui lui chercherait \$...
        av.formules <- grepl( sep.formules[ 1 ], texte )
        if ( TRUE == av.formules ) {
            pos.formules <- gregexpr( masque, fixed = FALSE, texte )[[ 1 ]]
            lg <- attr( pos.formules, "match.length" )

            formules <- lapply( 1:length( lg ),
                                function( j ) {
                                    ## On récupère le lien de l'image
                                    frm <- substr( texte,
                                                   start = pos.formules[ j ], 
                                                   stop = pos.formules[ j ] + lg[ j ] - 1 )

                                    ## On fait sauter les séparateurs
                                    frm <- gsub( masque, '\\1', frm )

                                    ## On créer la formule
                                    frm <- inserer_formule.moodle( frm )

                                    ## On renvoie le code pour l'insérer
                                    frm
                                } )

            ## On va du dernier au premier, car l'insertion change les positions
            ##   => on modifie la fin, ne change pas les positions d'avant
            for ( j in seq( from = length( lg ), to = 1, by = -1 ) ) {
                texte <- paste0( substr( texte, 1, pos.formules[ j ] - 1 ),
                                 formules[[ j ]],
                                 substr( texte, pos.formules[ j ] + lg[ j ], nchar( texte ) ) )
            }
        }
        
        textes[ i ] <- texte
    }

    ## On renvoie les textes traités
    textes
}

## ——————————————————————————————————————————————————————————————————————
##
## [Prétraitement] Insertion des formules chimiques générées par code SMILES
##

traiter_SMILES <- function( textes, sep.SMILES )
{
    ## Le masque de recherche
    masque <- paste0( sep.SMILES[ 1 ], "(.*?)", sep.SMILES[ 2 ] )
    
    ## On travaille texte par texte : plus simple
    for ( i in 1:length( textes ) ) {
        texte <- textes[ i ]

        ## Attention, avec le séparateur @{ choisi, { et } doivent être protégés par \
        ##   pour ne pas marquer les nombres de répétition d'une expression régulière
        ##  => on ne peut pas utiliser fixed = TRUE qui lui chercherait \{...
        av.SMILES <- grepl( sep.SMILES[ 1 ], texte )
        if ( TRUE == av.SMILES ) {
            pos.SMILES <- gregexpr( masque, fixed = FALSE, texte )[[ 1 ]]
            lg <- attr( pos.SMILES, "match.length" )

            SMILES <- lapply( 1:length( lg ),
                              function( j ) {
                                  ## On récupère le code SMILES
                                  frm <- substr( texte,
                                                 start = pos.SMILES[ j ], 
                                                 stop = pos.SMILES[ j ] + lg[ j ] - 1 )

                                  ## On fait sauter les séparateurs
                                  frm <- gsub( masque, '\\1', frm )

                                  ## On créer la formule
                                  frm <- inserer_SMILES.moodle( frm, marges = FALSE )

                                  ## On renvoie le code pour l'insérer
                                  frm
                              } )

            ## On va du dernier au premier, car l'insertion change les positions
            ##   => on modifie la fin, ne change pas les positions d'avant
            for ( j in seq( from = length( lg ), to = 1, by = -1 ) ) {
                texte <- paste0( substr( texte, 1, pos.SMILES[ j ] - 1 ),
                                 SMILES[[ j ]],
                                 substr( texte, pos.SMILES[ j ] + lg[ j ], nchar( texte ) ) )
            }
        }
        
        textes[ i ] <- texte
    }

    ## On renvoie les textes traités
    textes
}
