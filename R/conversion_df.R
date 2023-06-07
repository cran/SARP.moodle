## ——————————————————————————————————————————————————————————————————————
## Création de XML Moodle avec R
## © Emmanuel Curis — mars 2015, mai 2019
##
## Conversion d'une data.frame de format standardisé
##   en un fichier XML importable par Moodle
##
## Sur une idée de Virginie LASSERRE, février 2016
## ——————————————————————————————————————————————————————————————————————
## Historique
##  30 mars 2023 : création du fichier
##                  (repris de conversion_csv.R)
##                 possibilité d'indiquer une colonne pour le nombre de décimales
##                 détection des cases ne contenant que des espaces
##                   (dans les énoncés) et conversion en NA
##
##   9 avr. 2023 : prise en compte des questions « texte à trou »
##                 détection de la précision dans les questions numériques
##                   (automatique ou par colonne dédiée)
##
##  12 avr. 2023 : possibilité d'ignorer la casse pour les questions courtes
##
##   9 mai  2023 : corrigé une coquille dans un message
##
##  18 mai  2023 : conversion stop → erreur() et warning → avertissement()
##
##  31 mai  2023 : si une colonne d'identifiant est présente, mais vide,
##                  elle est ignorée avec un avertissement
## ——————————————————————————————————————————————————————————————————————

## ——————————————————————————————————————————————————————————————————————
## 
## Conversion d'une data-frame « format CSV » en XML
##   (comme ça, appelable aussi pour du excel ou autre !)
## 
## ——————————————————————————————————————————————————————————————————————
## La seule fonction visible de l'extérieur...
##
## fichier.xml = le fichier contenant les questions formatées
##               soit le nom d'un fichier à créer,
##               soit un fichier XML en cours d'usage
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
## colonne.note_question = la colonne qui contient la note globale de la question
## colonne.titre   = la colonne qui contient le titre des questions (NA : aucune)
## colonne.code    = la colonne qui contient le code de la question (NA : aucune)
## colonne.type    = la colonne qui contient le type de question (NA : aucune)
## colonne.retour  = la colonne qui contient le texte du commentaire à afficher en cas de (mauvaise) réponse
## colonne.global  = la colonne qui contient le texte du commentaire à afficher après résolution
## colonne.penalite = la colonne qui contient la pénalité de nouvelle tentative de la question
## colonne.decimale = la colonne qui indique combien de décimales compter dans les réponses numériques
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
## ——————————————————————————————————————————————————————————————————————
df.moodle <- function( d,
                       colonne.texte  = NA, colonne.reponse  = NA,
                       colonne.note   = NA, colonne.note_question = NA,
                       colonne.titre  = NA, colonne.code     = NA, colonne.type     = NA,
                       colonne.retour = NA, colonne.global   = NA, colonne.penalite = NA,
                       colonne.temps  = NA, colonne.decimale = NA,
                       fichier.xml,
                       creer.titre = TRUE, lg.titre = 30, embellir = TRUE, deja.HTML = FALSE,
                       forcer.multiple = TRUE, melanger.reponses = TRUE, somme.nulle = FALSE,
                       precision = 3,
                       categorie.base = "", 
                       dossier.images,
                       sep.images = c( '@@', '@@' ), inserer.images = TRUE,
                       sep.formules = c( '@\\$', '\\$@' ),
                       sep.SMILES = c( '@\\{', '\\}@' ),
                       nom.fichier = deparse( substitute( d ) ) )
{
    if ( nrow( d ) < 1 ) {
        erreur( 1000, "df.moodle",
                "Aucune question, conversion annul\u00e9e." )
    }
    if ( ncol( d ) < 2 ) {
        erreur( 1001, "df.moodle",
                "Il faut au moins deux colonnes, \u00e9nonc\u00e9s et r\u00e9ponses." )
    }

    ## On contrôle la précision
    if ( missing( precision ) ) precision <- 3
    if ( is.null( precision ) ) precision <- 3
    if ( length( precision ) > 1 ) {
        avertissement( 2, "df.moodle",
                       "Seule la premi\u00e8re valeur de pr\u00e9cision sera utilis\u00e9e." )
        precision <- precision[ 1 ]
    }
    if ( !is.na( precision) & !is.finite( precision ) ) {
        erreur( 8, "df.moodle",
                "La pr\u00e9cision doit \u00eatre un entier positif ou NA" )
    }
    precision <- as.integer( precision )
    
    ## On contrôle le dossier d'images
    if ( !is.logical( inserer.images ) ) {
        erreur( 4, "df.moodle",
                "inserer.images doit valoir TRUE ou FALSE" )
    }
    if ( length( inserer.images ) != 1 ) {
        erreur( 5, "df.moodle",
                "inserer.images ne doit avoir qu'une seule valeur, TRUE ou FALSE" )
    }
    
    if ( length( dossier.images ) > 1 ) {
        erreur( 6, "df.moodle",
                "Un seul dossier contenant les images",
                " doit \u00eatre sp\u00e9cifi\u00e9" )
    }
    if ( length( dossier.images ) == 1 ) {
        if ( FALSE == dir.exists( dossier.images ) ) {
            erreur( 101, "df.moodle",
                    "Le dossier d'images indiqu\u00e9 n'existe pas",
                    " [", dossier.images, "]." )
        }
    }

    ## On contrôle les séparateurs d'images
    if ( length( sep.images ) == 1 ) {
        sep.images <- c( sep.images, sep.images )
    }
    if ( length( sep.images ) > 2 ) {
        erreur( 9, "df.moodle",
                "Il faut deux s\u00e9parateurs d'image au plus :",
                " pour le d\u00e9but et pour la fin." )
    }

    ## On contrôle les séparateurs de formules
    if ( length( sep.formules ) == 1 ) {
        sep.formules <- c( sep.formules, sep.formules )
    }
    if ( length( sep.formules ) > 2 ) {
        erreur( 10, "df.moodle",
                "Il faut deux s\u00e9parateurs de formule au plus :",
                " pour le d\u00e9but et pour la fin." )
    }

    ## On contrôle les séparateurs de codes SMILES
    if ( length( sep.SMILES ) == 1 ) {
        sep.SMILES <- c( sep.SMILES, sep.SMILES )
    }
    if ( length( sep.SMILES ) > 2 ) {
        erreur( 11, "df.moodle",
                "Il faut deux s\u00e9parateurs de code SMILES au plus :",
                " pour le d\u00e9but et pour la fin." )
    }
    
    ## On récupère les noms des colonnes…
    noms.colonnes <- names( d )

    ## On essaye de détecter les colonnes obligatoires
    ##  1) colonne des énoncés
    colonne.texte <- trouver_colonne( colonne.texte,
                                      variantes = c( "\u00e9nonc\u00e9", "\u00e9nonc\u00e9s",
                                                     "Question", "Questions", 
                                                     "Texte", "Textes" ),
                                      noms.colonnes = noms.colonnes,
                                      type = "\u00e9nonc\u00e9",
                                      obligatoire = TRUE,
                                      message.erreur = "le texte des \u00e9nonc\u00e9s des questions" )    

    ##  2) colonne des réponses
    colonne.reponse <- trouver_colonne( colonne.reponse,
                                        variantes = c( "R\u00e9ponse", "R\u00e9ponses",
                                                       "Solution", "Solutions" ),
                                        noms.colonnes = noms.colonnes,
                                        type = "r\u00e9ponse",
                                        obligatoire = TRUE,
                                        message.erreur = "les r\u00e9ponses aux questions" )
    
    ## On essaye de détecter des colonnes facultatives…
    ##  1) colonne des notes
    colonne.note <- trouver_colonne( colonne.note, variantes = c( "Note" ),
                                     noms.colonnes = noms.colonnes, type = "note" )

    ##  1bis) colonne des notes globales de la question
    colonne.note_question <- trouver_colonne( colonne.note_question, variantes = c( "Note.question", "Note.globale",
                                                                                    "Notes.question", "Notes.globales" ),
                                              noms.colonnes = noms.colonnes, type = "note globale" )
    
    ##  2) colonne des types de question
    colonne.type <- trouver_colonne( colonne.type, variantes = c( "type" ),
                                     noms.colonnes = noms.colonnes, type = "type" )

    ##  3) colonne des codes de question
    colonne.code <- trouver_colonne( colonne.code, variantes = c( "code", "id" ),
                                     noms.colonnes = noms.colonnes, type = "code" )

    ##  4) colonne des pénalités de nouvelle tentative de la question
    colonne.penalite <- trouver_colonne( colonne.penalite, variantes = c( "P\u00e9nalit\u00e9",
                                                                          "Penalty" ),
                                         noms.colonnes = noms.colonnes, type = "p\u00e9nalit\u00e9" )

    ##  5) colonne des temps conseillés
    colonne.temps <- trouver_colonne( colonne.temps, variantes = c( "Temps",
                                                                    "Time" ),
                                      noms.colonnes = noms.colonnes, type = "temps" )

    ##  6) colonne des titres
    colonne.titre <- trouver_colonne( colonne.titre, variantes = c( "Titre", "Titres",
                                                                    "Title", "Titles" ),
                                      noms.colonnes = noms.colonnes, type = "titre" )

    ##  7) colonne des décimales
    colonne.decimale <- trouver_colonne( colonne.decimale, variantes = c( "D\u00e9cimale", "D\u00e9cimales",
                                                                          "Pr\u00e9cision" ),
                                         noms.colonnes = noms.colonnes, type = "titre" )


    ## … et on repère celles indiquées, si ce ne sont pas des nombres.
    ##   (remarque : si absente, NA : is.integer vaut FALSE
    ##                                sauf si NA_integer_ mais alors noms.colonnes[ ] renvoie NA
    ##    ==> devrait fonctionner...)
    if ( is.integer( colonne.texte   ) ) colonne.texte   <- noms.colonnes[ colonne.texte   ]
    if ( is.integer( colonne.reponse ) ) colonne.reponse <- noms.colonnes[ colonne.reponse ]
    if ( is.integer( colonne.note    ) ) colonne.note    <- noms.colonnes[ colonne.note    ]
    if ( is.integer( colonne.note_question ) ) colonne.note_question <- noms.colonnes[ colonne.note_question ]
    if ( is.integer( colonne.titre   ) ) colonne.titre   <- noms.colonnes[ colonne.titre   ]
    if ( is.integer( colonne.code    ) ) colonne.code    <- noms.colonnes[ colonne.code    ]
    if ( is.integer( colonne.type    ) ) colonne.type    <- noms.colonnes[ colonne.type    ]
    if ( is.integer( colonne.retour  ) ) colonne.retour  <- noms.colonnes[ colonne.retour  ]
    if ( is.integer( colonne.global  ) ) colonne.global  <- noms.colonnes[ colonne.global  ]
    if ( is.integer( colonne.penalite ) ) colonne.penalite <- noms.colonnes[ colonne.penalite ]
    if ( is.integer( colonne.temps   ) ) colonne.temps   <- noms.colonnes[ colonne.temps ]
    if ( is.integer( colonne.temps   ) ) colonne.temps   <- noms.colonnes[ colonne.temps ]

    cat( "  Colonne des \u00e9nonc\u00e9s        :", colonne.texte  , "\n" )
    cat( "  Colonne des r\u00e9ponses       :"     , colonne.reponse, "\n" )
    
    ## On vérifie qu'il n'y a pas de doublons
    if ( any( duplicated( na.omit( c( colonne.texte , colonne.reponse,
                                      colonne.note  , colonne.note_question, colonne.titre,
                                      colonne.code  , colonne.type,
                                      colonne.retour, colonne.global, colonne.penalite,
                                      colonne.temps , colonne.decimale ) ) ) ) ) {
        erreur( 1002, "df.moodle",
                "Une m\u00eame colonne ne peut pas servir \u00e0 deux informations distinctes !" )
    }

    ## On supprime toute éventuelle mention d'un temps global
    if ( exists( "temps.categorie", envir = SARP.Moodle.env ) ) {
        remove( "temps.categorie", envir = SARP.Moodle.env )
    }
    
    ## On remplace les textes vides par des NA, plus faciles à repérer
    if ( any( nchar( d[ , colonne.texte ] ) < 1, na.rm = TRUE ) )
        d[ which( nchar( d[, colonne.texte ] ) < 1 ) , colonne.texte ] <- NA
    idx <- grep( "^[[:space:]]+$", d[ , colonne.texte ] )
    if ( length( idx ) > 0 ) {
        n <- length( idx )
        avertissement( 1000, "df.moodle",
                       n, " case", if ( n > 1 ) "s",
                       " avec seulement des espaces",
                       " dans les \u00e9nonc\u00e9s -",
                       " consid\ue009r\u00e9e", if ( n > 1 ) "s",
                       " vide", if ( n > 1 ) "s", "." )
        d[ idx, colonne.texte ] <- NA
    } 

    if ( any( nchar( d[ , colonne.reponse ] ) < 1, na.rm = TRUE ) )
        d[ which( nchar( d[, colonne.reponse ] ) < 1 ) , colonne.reponse ] <- NA

    if ( !is.na( colonne.temps ) ) {
        if ( is.character( d[ , colonne.temps ] ) ) {
            if ( any( nchar( d[ , colonne.temps ] ) < 1, na.rm = TRUE ) )
                d[ which( nchar( d[, colonne.temps ] ) < 1 ) , colonne.temps ] <- NA
        }
    }
    
    ## On introduit un code interne unique...
    d$I_Code.interne <- NA

    if ( !is.na( colonne.code ) ) {
        if ( is.character( d[ , colonne.code ] ) ) {
            if ( any( nchar( d[ , colonne.code ] ) < 1, na.rm = TRUE ) )
                d[ which( nchar( d[, colonne.code ] ) < 1 ) , colonne.code ] <- NA
        }

        if ( all( is.na( d[ , colonne.code ] ) ) ) {
            avertissement( 1008,
                           "Une colonne d'identifiants a \u00e9t\u00e9 d\ue009tect\u00e9e,",
                           " mais elle semble vide. Elle sera ignor\u00e9e." )
            colonne.code <- NA
        }
    }
    
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
            d$Titres <- paste0( nom.fichier, " : ", d$I_Code.interne )
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

    ## Le type de la question, s'il existe
    if ( FALSE == is.na( colonne.type ) ) {
        type.question <- d[ , colonne.type ]
        
        ## Harmonisation : minuscule, pas d'espace, pas de point
        type.question <- tolower( type.question )
        type.question <- gsub( ".", "", fixed = TRUE, type.question )
        type.question <- gsub( "[[:space:]]", "", fixed = FALSE, type.question )

        ## Harmonisation : on se débarrasse des accents
        type.question <- oter_accents( type.question )
        ## type.question <- gsub( "[\u00e0\u00e1\u00e2\u00e3\u00e4\u00e5]", "a", type.question )
        ## type.question <- gsub( "\u00e7",                                 "c", type.question )
        ## type.question <- gsub( "[\u00e8\u00e9\u00ea\u00eb]",             "e", type.question )
        ## type.question <- gsub( "[\u00ec\u00ed\u00ee\u00ef]",             "i", type.question )
        ## type.question <- gsub( "\u00f1",                                 "n", type.question )
        ## type.question <- gsub( "[\u00f2\u00f3\u00f4\u00f5\u00f6\u00f8]", "o", type.question )
        ## type.question <- gsub( "[\u00f9\u00fa\u00fb\u00fc]",             "u", type.question )
        ## type.question <- gsub( "[\u00fd\u00ff]",                         "y", type.question )
        
        ## On remplace les vides par des NA
        idx <- which( ( type.question == "" ) |
                      ( nchar( type.question ) < 1 ) )
        if ( length( idx ) > 0 ) {
            type.question[ idx ] <- NA
        }
        
        ## On mémorise
        d[ , colonne.type ] <- type.question
    }

    ## On repère les catégories
    tbl.codes <- table( d$I_Code.interne )
    id.simple <- names( tbl.codes )[ which( tbl.codes == 1 ) ]
    d$I.Q_simple <- FALSE
    d$I.Q_simple[ which( d$I_Code.interne %in% id.simple ) ] <- TRUE

    d$I.Categorie <- FALSE
    d$I.Categorie[ which( ( d$I.Q_simple ) &
                          is.na( d[ , colonne.reponse ] ) ) ] <- TRUE
    if ( !is.na( colonne.type ) ) {
        d$I.Categorie[ which( nchar( d[ , colonne.type ] ) > 0 ) ] <- FALSE

        d$I.Categorie[ which( ( d[ , colonne.type ] %in% c( 'categorie',
                                                            'category',
                                                            'c', 'cat' ) ) &
                              ( d$I_Code.interne %in% id.simple ) ) ] <- TRUE
    }
    n.categories <- length( which( d$I.Categorie ) )
    if ( n.categories > 0 ) {
        cat( "  dont ",
             n.categories, " cat\u00e9gorie", if ( n.categories > 1 ) "s",
             ".\n", sep = "" )
    }

    ## On indique la catégorie : base + nom du fichier…
    if ( d$I.Categorie[ 1 ] == FALSE ) {
        base <- gsub( "\\", "/", fixed = TRUE, nom.fichier ) # Sous Windows, \ au lieu de / comme séparateur...
        base <- strsplit( base, "/" )[[ 1 ]]
        base <- base[ length( base ) ]
        categorie <- paste( categorie.base, base, sep = "/" )
        if ( "/" == substr( categorie, 1, 1 ) ) categorie <- substr( categorie, 2, nchar( categorie ) )
        cat( "\n Cat\u00e9gorie globale : ", categorie )
        if ( any( d$I.Categorie ) ) {
            cat( " (questions avant la 1re cat\u00e9gorie du fichier)" )
        } else {
            cat( " (toutes les questions)" )
        }
        cat( "\n\n" )
        categorie.moodle( nom.categorie = categorie,
                         fichier.xml = fichier.xml )
    } else {
        cat( "\n Cat\u00e9gorie", if ( n.categories > 1 ) "s",
             " donn\u00e9e", if ( n.categories > 1 ) "s",
             " par le fichier\n\n", sep = "" )
    }

    ## On numérote les questions de façon logique
    d$Numero.interne <- NA
    idx.categorie <- which( d$I.Categorie )
    idx.l1 <- setdiff( 1:nrow( d ),
                       which( duplicated( d$I_Code.interne ) ) )
    idx.l1 <- setdiff( idx.l1, idx.categorie )
    if ( length( idx.categorie ) > 0 ) {
        d$Numero.interne[ idx.categorie ] <- paste0( "C-",
                                                     formatC( 1:n.categories,
                                                              width = 1 + as.integer( log( n.categories, 10 ) ),
                                                              flag = "0", format = "d" ) )
    }
    if ( length( idx.l1 ) > 0 ) {
        n.q_vraie <- length( idx.l1 )
        d$Numero.interne[ idx.l1 ] <- paste0( "Q",
                                              formatC( 1:n.q_vraie,
                                                       width = 1 + as.integer( log( n.q_vraie, 10 ) ),
                                                       flag = "0", format = "d" ) )
    }
    
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
        cat( " Conversion des formules...\n" )
        
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
        cat( " Conversion des codes SMILES...\n" )
        
        d[ , colonne.texte   ] <- traiter_SMILES( d[ , colonne.texte   ], sep.SMILES ) # Énoncé
        d[ , colonne.reponse ] <- traiter_SMILES( d[ , colonne.reponse ], sep.SMILES ) # Réponse
        if ( !is.na( colonne.retour ) ) {
            d[ , colonne.retour  ] <- traiter_SMILES( d[ , colonne.retour  ], sep.SMILES ) # Commentaire systématique
        }
        if ( !is.na( colonne.global ) ) {
            d[ , colonne.global  ] <- traiter_SMILES( d[ , colonne.global  ], sep.SMILES ) # Commentaire global si faux
        }
    }

    ## Étape de conversions éventuelles finie
    cat( "\n" )

    ## On adapte la mise en forme des textes
    d[ , colonne.texte   ] <- preparer_texte( d[ , colonne.texte   ] )
    d[ , colonne.reponse ] <- preparer_texte( d[ , colonne.reponse ] )
    if ( !is.na( colonne.retour ) ) {
        d[ , colonne.retour  ] <- preparer_texte( d[ , colonne.retour  ] ) # Commentaire systématique
    }
    if ( !is.na( colonne.global ) ) {
        d[ , colonne.global  ] <- preparer_texte( d[ , colonne.global  ] ) # Commentaire global si faux
    }

    ## On prépare les colonnes internes recevant le résultat
    d$Avertissements__ <- NA_character_
    d$Type.final__ <- NA_character_

    ## On travaille par question…
    for( q in codes ) {
        cat( " Conversion de la question ", q )
        idx.question <- which( d$I_Code.interne == q )
        dq <- d[ idx.question, ]

        avertissements.avant <- get( "liste.avertissements", envir = SARP.Moodle.env )
        if ( nrow( dq ) == 1 ) {
            cat( " [simple]\n" )
            
            type <- convertir.question_simple( question = dq,
                                               colonne.texte     = colonne.texte,
                                               colonne.reponse   = colonne.reponse,
                                               colonne.note      = colonne.note,
                                               colonne.note_question = colonne.note_question,
                                               colonne.titre     = colonne.titre,
                                               colonne.type      = colonne.type,
                                               colonne.retour    = colonne.retour,
                                               colonne.global    = colonne.global,
                                               colonne.penalite  = colonne.penalite,
                                               colonne.temps     = colonne.temps,
                                               colonne.decimale  = colonne.decimale,
                                               fichier.xml       = fichier.xml,
                                               embellir          = embellir,
                                               deja.HTML         = deja.HTML,
                                               forcer.multiple   = forcer.multiple,
                                               melanger.reponses = melanger.reponses,
                                               precision.defaut  = precision,
                                               categorie.base    = categorie.base )
        } else {
            cat( " [complexe,", nrow( dq ), "lignes]\n" )
            
            type <- convertir_question( question = dq,
                                        colonne.texte     = colonne.texte,
                                        colonne.reponse   = colonne.reponse,
                                        colonne.note      = colonne.note,
                                        colonne.note_question = colonne.note_question,
                                        colonne.titre     = colonne.titre,
                                        colonne.type      = colonne.type,
                                        colonne.retour    = colonne.retour,
                                        colonne.global    = colonne.global,
                                        colonne.penalite  = colonne.penalite,
                                        colonne.temps     = colonne.temps,
                                        colonne.decimale  = colonne.decimale,
                                        fichier.xml       = fichier.xml,
                                        embellir          = embellir,
                                        deja.HTML         = deja.HTML,
                                        forcer.multiple   = forcer.multiple,
                                        melanger.reponses = melanger.reponses,
                                        somme.nulle       = somme.nulle,
                                        precision         = precision )
        }

        ## On indique le type trouvé de la question
        d$Type.final__[ idx.question ] <- type

        ## On informe sur les avertissements rencontrés
        avertissements.apres <- get( "liste.avertissements", envir = SARP.Moodle.env )
        if ( length( avertissements.apres ) > length( avertissements.avant ) ) {
            d$Avertissements__[ idx.question ] <- paste0( avertissements.apres[ ( length( avertissements.avant ) + 1 ):length( avertissements.apres ) ], collapse = ", " )
        }
    }
    
    ## On renvoie la base des questions...
    d
}


## ——————————————————————————————————————————————————————————————————————
##
## Le cas des questions « simples » (en une ligne)
##
## Les paramètres sont ceux de la fonction précédente
##   sauf somme.nulle qui ne sert à rien
convertir.question_simple <- function( question,
                                       colonne.texte, colonne.reponse, 
                                       colonne.note, colonne.note_question,
                                       colonne.titre, colonne.type,
                                       colonne.retour, colonne.global, colonne.penalite,
                                       colonne.temps, colonne.decimale,
                                       fichier.xml,
                                       embellir, deja.HTML,
                                       forcer.multiple, melanger.reponses,
                                       precision.defaut = precision,
                                       categorie.base )
{
    if ( nrow( question ) != 1 ) {
        erreur( 1003, "convertir.question_simple",
                "Une question simple doit correspondre \u00e0 une seule ligne." )
    }
    
    ## Le code [interne] de la question
    code.interne <- question$I_Code.interne
    
    ## Le type de la question, s'il existe
    if ( FALSE == is.na( colonne.type ) ) {
        type.question <- tolower( question[ , colonne.type ] )
    } else {
        type.question <- NA
    }
    if ( !is.na( type.question ) ) {
        cat( "  [Type de question impos\u00e9 :", type.question, "]\n" )
    }

    ## Un commentaire global éventuel
    if ( FALSE == is.na( colonne.global ) ) {
        commentaire.global <- question[ , colonne.global ]
    } else {
        commentaire.global <- NA
    }

    ## Une note globale éventuelle
    note.question <- NA
    if ( FALSE == is.na( colonne.note_question ) ) {
        note.question <- question[ , colonne.note_question ]
    }
    if ( is.na( note.question ) ) {
        ## Pour une question simple, la note peut être
        ##  dans la colonne Note...
        if ( FALSE == is.na( colonne.note ) ) {
            note.question <- question[ , colonne.note ]
        }
    }
    if ( is.na( note.question ) ) {
        ## Note par défaut : 1
        note.question <- 1
    }

    ## Une pénalité éventuelle
    penalite.question <- NA
    if ( FALSE == is.na( colonne.penalite ) ) {
        penalite.question <- question[ , colonne.penalite ]
    }

    ## Un temps conseillé éventuel
    temps <- NULL # Pour ne pas oublier un temps de catégorie
    if ( FALSE == is.na( colonne.temps ) ) {
        temps <- question[ , colonne.temps ]
        if ( is.na( temps ) ) temps <- NULL # Pour utiliser le temps de catégorie s'il est défini
    }

    ## On récupère la réponse, et on analyse sa nature...
    reponse <- question[ 1, colonne.reponse ]
        
    ## Cas 1 : aucune réponse indiquée
    ##  ==> on change de catégorie ou question de composition
    if ( any( is.na( reponse ), nchar( reponse ) < 1 ) ) {
        if ( !is.na( type.question ) ) {
            ## Si type imposé « Composition », question de composition
            if ( type.question %in% c( 'compo', 'composition',
                                       'redaction', 'r\u00e9daction',
                                       'ouvert', 'ouverte',
                                       'c', 'o', 'r' ) ) {
                cat( "  Question de r\u00e9daction, r\u00e9ponse libre\n" )

                if ( question$I.Categorie ) {
                    erreur( 1004, convertir.question_simple,
                            "Question aussi identifi\u00e9e comme cat\u00e9gorie !" )
                }
                
                question_ouverte.moodle( texte = question[ 1, colonne.texte ],
                                         titre = question[ 1, colonne.titre ],
                                         temps = temps,
                                         fichier.xml = fichier.xml )
                return( "R\u00e9daction" )
            }

            ## Si type imposé « Description », « question » descriptive
            if ( type.question %in% c( 'desc', 'description', 'd' ) ) {
                cat( "  Question de descriptive, pas de r\u00e9ponse\n" )

                if ( question$I.Categorie ) {
                    erreur( 1004, convertir.question_simple,
                            "Question aussi identifi\u00e9e comme cat\u00e9gorie !" )
                }
                
                description.moodle( texte = question[ 1, colonne.texte ],
                                    titre = question[ 1, colonne.titre ],
                                    fichier.xml = fichier.xml )
                return( "Description" )
            }

            ## Si texte à trous
            if ( type.question %in% c( 'trou', 'trous', 't' ) ) {
                cat( "  Texte \u00e0 trous\n" )

                if ( question$I.Categorie ) {
                    erreur( 1004, convertir.question_simple,
                            "Question aussi identifi\u00e9e comme cat\u00e9gorie !" )
                }
                
                glisser_textes.moodle( texte = question[ 1, colonne.texte ],
                                       titre = question[ 1, colonne.titre ],
                                       temps = temps,
                                       commentaire.global = commentaire.global,
                                       note.question = note.question,
                                       penalite = penalite.question,
                                       fichier.xml = fichier.xml )
                return( "Description" )
            }
        }

        ## Par défaut, nouvelle catégorie
        categorie <- paste( categorie.base, question[ 1, colonne.texte ],
                            sep = "/" )
        if ( "/" == substr( categorie, 1, 1 ) ) categorie <- substr( categorie, 2, nchar( categorie ) )
        cat( "  Nouvelle cat\u00e9gorie :", categorie )
        if ( all( !is.null( temps ), !is.na( temps ) ) ) {
            cat( " [Temps : ", temps, "]" )
        }
        cat( "\n\n" )

        if ( !question$I.Categorie ) {
            erreur( 1005, convertir.question_simple,
                    "Question non-identifi\u00e9e comme cat\u00e9gorie !" )
        }

        categorie.moodle( nom.categorie = categorie,
                          temps = temps,
                          fichier.xml = fichier.xml )
        return( "Cat\u00e9gorie" )
    }

    if ( question$I.Categorie ) {
        erreur( 1006, convertir.question_simple,
                "Question identifi\u00e9e \u00e0 tort comme cat\u00e9gorie !" )
    }

    ## Cas 2 : réponse numérique
    ##   (en évitant des avertissements superflus)
    reponse.n <- suppressWarnings( as.double( reponse ) )
    if ( is.na( reponse.n ) )
        reponse.n <- suppressWarnings( as.double( gsub( ",", ".", fixed = TRUE, reponse ) ) )
    if ( is.finite( reponse.n ) ) {
        cat( "  Question simple, r\u00e9ponse num\u00e9rique :",
             reponse.n, "\n" )

        ## Quel nombre de décimales pour la réponse ?
        precision <- NA
        ## 1) valeur donnée dans une colonne spéciale ?
        if ( !is.na( colonne.decimale ) ) {
            precision <- question[ 1, colonne.decimale ]
        }

        ## 2) détection à partir du nombre de chiffres après la virgule
        if ( is.na( precision ) ) {
            pos.decimale <- regexpr( ",|\\.", reponse )
            if( pos.decimale > 0 ) {
                decimales <- substr( reponse, pos.decimale + 1, nchar( reponse ) )
                decimales <- gsub( "[[:space:]]", "", decimales )
                if ( grepl( "e[-+0-9]+", decimales, ignore.case = TRUE ) ) {
                    avertissement( 1001, "convertir.question_simple",
                                   "Nombre en notation scientifique,",
                                   " la d\u00e9tection automatique du nombre de d\u00e9cimales",
                                   " risque d'\u00e9chouer.",
                                   " Utilisez la colonne Pr\u00e9cision." )
                    decimales <- gsub( "e[-+0-9]+", "", decimales, ignore.case = TRUE )
                }
                precision <- nchar( decimales )
            } else {
                reponse.n <- as.integer( reponse.n ) # Entier => pas de message
            }
        }

        ## 3) valeur par défaut, si réponse non-entière
        if ( is.na( precision ) ) {
            if ( ( reponse.n - as.integer( reponse.n ) ) == 0 ) {
                ## Réponse apparemment entière : pas d'indication sur le nombre de décimales
                precision <- NA
                reponse.n <- as.integer( reponse.n )
            } else {
                precision <- precision.defaut
            }
        }

        numerique.moodle( texte = question[ 1, colonne.texte ],
                          bonne.reponse = reponse.n,
                          titre = question[ 1, colonne.titre ],
                          n.decimales = precision,
                          temps = temps,
                          fichier.xml = fichier.xml,
                          commentaire.global = commentaire.global,
                          note.question = note.question, penalite = penalite.question )
        return( "Num\u00e9rique" )
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
                          temps = temps,
                          commentaire.global = commentaire.global,
                          note.question = note.question, penalite = penalite.question )
        return( "Vrai-Faux" )
    }

    ## Cas 4 : réponse ouverte « QROC »
    cat( "  Question simple, r\u00e9ponse textuelle unique : ",
         reponse, "\n" )
    casse <- TRUE                       # Par défaut, sensible à la casse
    if ( !is.na( type.question ) ) {
        if ( substr( type.question, 1, 1 ) == "i" ) {
            casse <- FALSE
        }
    }
    qroc.moodle( texte = question[ 1, colonne.texte ],
                 reponses = reponse, casse = casse,
                 titre = question[ 1, colonne.titre ],
                 fichier.xml = fichier.xml,
                 temps = temps,
                 commentaire.global = commentaire.global,
                 note.question = note.question, penalite = penalite.question )
    return( "QROC" )
}


## ——————————————————————————————————————————————————————————————————————
##
## Le cas des questions « complexes » (en plusieurs lignes)
##
## Les paramètres sont ceux de la fonction précédente
##   sauf somme.nulle qui ne sert à rien
convertir_question <- function( question,
                                colonne.texte, colonne.reponse, 
                                colonne.note, colonne.note_question,
                                colonne.titre, colonne.type,
                                colonne.retour, colonne.global, colonne.penalite,
                                colonne.temps, colonne.decimale,
                                fichier.xml,
                                embellir, deja.HTML,
                                forcer.multiple, melanger.reponses, somme.nulle,
                                precision = precision )
{
    if ( nrow( question ) < 2 ) {
        erreur( 1007, "convertir_question",
                "Il doit y avoir au moins deux lignes" )
    }
    
    ## Le code [interne] de la question
    code.interne <- unique( question$I_Code.interne )
    if ( length( code.interne ) != 1 ) {
        ## Problème
        erreur( 1008, "convertir_question",
                "Probl\u00e8me : question avec double code interne [",
                paste0( code.interne, collapse = ", " ), "]" )
    }

    if ( any( question$I.Categorie ) ) {
        erreur( 1009, "convertir_question",
                "Au moins une ligne faussement d\u00e9tect\u00e9e",
                " comme une cat\u00e9gorie." )
    }

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

    ## Une note globale éventuelle
    note.question <- NA
    if ( FALSE == is.na( colonne.note_question ) ) {
        note.question <- unique( question[ , colonne.note_question ] )
        if ( length( note.question ) > 1 ) {
            note.question <- na.omit( note.question )
        }
        if ( length( note.question ) > 1 ) {
            avertissement( 1002, "convertir_question",
                           "Plusieurs notes distinctes pour la m\u00eame question.",
                           " Seule la premi\u00e8re sera utilis\u00e9e." )
            note.question <- note.question[ 1 ]
        }
    }
    if ( is.na( note.question ) ) {
        ## Note par défaut : 1
        note.question <- 1
    }

    ## Une pénalité éventuelle
    penalite.question <- NA
    if ( FALSE == is.na( colonne.penalite ) ) {
        penalite.question <- unique( question[ , colonne.penalite ] )
        if ( length( penalite.question ) > 1 ) {
            penalite.question <- na.omit( penalite.question )
        }
        if ( length( penalite.question ) > 1 ) {
            avertissement( 1003, "convertir_question",
                           "Plusieurs p\u00e9nalit\u00e9s distinctes pour la m\u00eame question.",
                           " Seule la premi\u00e8re sera utilis\u00e9e." )
            penalite.question <- penalite.question[ 1 ]
        }
    }

    ## Un temps conseillé éventuel
    temps <- NULL # Pour ne pas oublier un temps de catégorie
    if ( FALSE == is.na( colonne.temps ) ) {
        temps <- na.omit( question[ , colonne.temps ] )
        if ( length( temps ) > 1 ) {
            avertissement( 1004, "convertir_question",
                           "Plusieurs temps conseill\u00e9s pour la m\u00eame question.",
                           " Seul le premier sera utilis\u00e9." )
            temps <- temps[ 1 ]
        }
    }

    ## Est-ce une question cloze ?
    q.cloze <- length( na.omit( question[ , colonne.texte ] ) ) > 1
    if ( q.cloze ) {
        convertir_cloze( question = question,
                         colonne.texte     = colonne.texte,
                         colonne.reponse   = colonne.reponse,
                         colonne.note      = colonne.note,
                         colonne.type      = colonne.type,
                         colonne.retour    = colonne.retour,
                         note.question     = note.question,
                         titre              = question[ 1, colonne.titre ],
                         penalite.question  = penalite.question,
                         commentaire.global = commentaire.global,
                         temps              = temps,
                         fichier.xml       = fichier.xml,
                         embellir          = embellir,
                         deja.HTML         = deja.HTML,
                         precision         = precision )
        return( "\u00ab cloze \u00bb" )
    }
    
    ## Le type de la question, s'il existe
    if ( FALSE == is.na( colonne.type ) ) {
        type.question <- question[ , colonne.type ]
        
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
            erreur( 1010, "convertir_question",
                    "Probl\u00e8me : question avec type multiple ou absent [",
                    paste0( code.interne, collapse = ", " ), "]" )
        } else if ( length( type.question ) == 0 ) {
            ## Si aucun type indiqué : devient un vecteur vide
            ## ==> on bascule vers la détection automatique
            type.question <- NA
        }
    } else {
        type.question <- NA
    }
    if ( !is.na( type.question[ 1 ] ) ) {
        cat( "  [Type de question impos\u00e9 :", type.question, "]\n" )
    }

    ## Combien de réponses à la question
    n.reponses <- nrow( question )

    ## Cas 1 : un seul énoncé, QCM
    cat( "  QCM,", n.reponses, "r\u00e9ponses possibles\n" )

    if ( is.na( colonne.note ) ) {
        erreur( 1011, "convertir_question",
                "Pr\u00e9cisez une colonne avec les notes",
                " pour cr\u00e9er des questions \u00e0 choix multiples." )
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
        avertissement( 1005, "convertir_question",
                       "Attention, des notes comprises entre 0 et 1",
                       " ont \u00e9t\u00e9 converties en pourcentages." )
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
            erreur( 1012, "convertir_question",
                    "Incoh\u00e9rence : QCU demand\u00e9,",
                    " mais plusieurs bonnes r\u00e9ponses indiqu\u00e9es !\n",
                    " Conversion interrompue." )
        }
        forcer.multiple <- FALSE;    # On empêche la conversion en multiple...
    }

    ## Veut-on un QCM à somme nulle (ie, si l'étudiant coche tout, il a 0)
    ##   (attention à ne pas le faire pour un QCU)
    if ( all( TRUE == somme.nulle,
              length( mauvaises ) > 0,
              forcer.multiple | !reponse.unique ) ) {
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
               commentaire.global = commentaire.global,
               note.question = note.question, penalite = penalite.question,
               temps = temps )

    return( "QCM ou QCU" )
}


## Cas 2 : question « cloze »
convertir_cloze <- function( question,
                             colonne.texte, colonne.reponse,
                             colonne.note , colonne.type,
                             colonne.retour,
                             titre,
                             note.question, penalite.question,
                             commentaire.global, temps,
                             fichier.xml,
                             embellir, deja.HTML,
                             somme.nulle,
                             precision )
{
    warning( "Cas des questions \u00ab cloze \u00bb pas encore v\u00e9rifi\u00e9" )
    cat( "  Question Cloze :\n" )
#    print( question )

    n.reponses <- nrow( question )

    ## a) les textes avant les champs
    textes.avant <- na.omit( question[ , colonne.texte ] )

    texte.final <- ""
    idx.final <- which( is.na( question[ , colonne.reponse ] ) |
                        ( nchar( question[ , colonne.reponse ] ) < 1 ) )
    if ( length( idx.final ) > 0  ) {
        if ( length( idx.final ) > 1 ) {
            cat( "   * Attention * - plus d'une ligne sans r\u00e9ponse...\n" )
        }
        if( !identical( sort( idx.final ),
                       min( idx.final ):max( idx.final ) ) ) {
            cat( "   * Attention * - ligne sans r\u00e9ponse ins\u00e9r\u00e9e entre des champs...\n" )
        }
        texte.final <- paste( question[ idx.final, colonne.texte ],
                              collapse = " " )
        cat( "   Texte final d\u00e9tect\u00e9 : ", texte.final, "\n" )
        
        textes.avant <- textes.avant[ 1:( length( textes.avant ) - length( idx.final ) ) ]

        n.reponses <- n.reponses - length( idx.final )
    }


    ## b) les réponses pour chaque champ
    n.champs <- length( textes.avant )
    cat( "   Nombre de champs de r\u00e9ponse : ", n.champs, "\n" )
    choix.multiple <- ( n.champs < n.reponses )
    if ( n.champs > n.reponses ) {
        erreur( 1013, "convertir_question",
                "Format de question cloze incorrect : plus de champs que de r\u00e9ponses !" )
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
            ## Question à réponse multiple

            ## On repère les index des lignes des réponses
            ##  1) index de la ligne commençant le champ suivant
            idx.suivant <- which( !is.na( question[ (i.ligne + 1):n.reponses, colonne.texte ] ) )
            if ( length( idx.suivant ) > 0 ) {
                idx.suivant <- idx.suivant[ 1 ]
            } else {
                idx.suivant <- nrow( question ) + 1 - i.ligne - length( idx.final )
            }

            ##  2) index des lignes d'ici là
            ##     attention, pour idx.suivant, 1 = 2e réponse
            idx.reponses <- i.ligne + ( 0:( idx.suivant - 1 ) )

            ## Le type : par défaut, SHORTANSWER
            types[ i ] <- "SHORTANSWER"
            if ( !is.na( colonne.type ) ) {
                type.champ <- tolower( question[ i.ligne, colonne.type ] )
                if ( type.champ %in% c( 'm', 'mc', 'multi', 'multichoice',
                                        'me', 'men', 'menu' ) ) {
                    types[ i ] <- "MULTICHOICE"
                } else if ( type.champ %in% c( 'ms', 'mcs', 'multichoice_s', 's', 'sm',
                                               'ma', 'mca', 'a', 'am',
                                               'mea', 'mena', 'menua', 'ame', 'amen', 'amenu' ) ) {
                    types[ i ] <- "MULTICHOICE_S"
                } else if ( type.champ %in% c( 'h', 'mch', 'multichoice_h' ) ) {
                    types[ i ] <- "MULTICHOICE_H"
                } else if ( type.champ %in% c( 'hs', 'mchs', 'multichoice_hs', 'sh' ) ) {
                    types[ i ] <- "MULTICHOICE_HS"
                } else if ( type.champ %in% c( 'v', 'mcv', 'multichoice_v' ) ) {
                    types[ i ] <- "MULTICHOICE_V"
                } else if ( type.champ %in% c( 'vs', 'mcvs', 'multichoice_vs', 'sv' ) ) {
                    types[ i ] <- "MULTICHOICE_VS"
                } else if ( type.champ %in% c( 'mr', 'multiresponse',
                                               'mv', 'vm', 'mrv', 'multiresponse_v' ) ) {
                    types[ i ] <- "MULTIRESPONSE"
                } else if ( type.champ %in% c( 'mh', 'mrh', 'hm', 'multiresponse_h' ) ) {
                    types[ i ] <- "MULTIRESPONSE_H"
                } else if ( type.champ %in% c( 'mhs', 'msh', 'hms', 'hsm', 'shm', 'smh',
                                               'mrhs', 'multiresponse_hs',
                                               'mha', 'mah', 'ahm', 'amh', 'hma', 'ham' ) ) {
                    types[ i ] <- "MULTIRESPONSE_HS"
                } else if ( type.champ %in% c( 'mvs', 'msv', 'vms', 'vsm', 'svm', 'smv',
                                               'mrs', 'mrvs', 'multiresponse_s',  'multiresponse_vs',
                                               'mva', 'mav', 'avm', 'amv', 'vma', 'vam' ) ) {
                    types[ i ] <- "MULTIRESPONSE_S"
                } else if ( type.champ %in% c( 'sa', 'as', 'shortanswer' ) ) {
                    types[ i ] <- "SHORTANSWER"
                } else if ( type.champ %in% c( 'sc', 'sac', 'shortanswer_c', 'cs',
                                               'cas', 'acs', 'asc', 'csa', 'sca' ) ) {
                    types[ i ] <- "SHORTANSWER_C"
                } else if ( type.champ %in% c( 'n', 'numerical' ) ) {
                    types[ i ] <- "NUMERICAL"
                } else {
                    avertissement( 1006, "convertir_cloze",
                                   "Type de champ inconnu [", type.champ, "], ignor\u00e9." )
                }
            }

            cat( sep = "", "   champ ", i, " : multir\u00e9ponses, type ", types[ i ] ,
                 " (", length( idx.reponses ), " r\u00e9ponses)\n" )
            multi.reponses <- list( "Textes" = question[ idx.reponses, colonne.reponse ] )
            
            ## Construction des notes (par défaut, toutes les réponses sont correctes)
            correct <- rep( TRUE, length( idx.reponses ) )
            if ( !is.na( colonne.note ) ) {
                ## Détection des VRAI - FAUX
                notes <- tolower( question[ idx.reponses, colonne.note ] )
                correct[ which( notes %in% c( "f", "faux", "false"     ) ) ] <- FALSE
                correct[ which( notes %in% c( "v", "vrai", "true", "t" ) ) ] <- TRUE

                correct[ which( notes == 0 ) ] <- FALSE
                correct[ which( notes %in% c( 1, 100 ) ) ] <- TRUE

                multi.reponses$Correct <- correct
                
                ## On vérifie s'il y a des notes
                if ( types[[ i ]] %in% c( 'MULTIRESPONSE'  , 'MULTIRESPONSE_S',
                                          'MULTIRESPONSE_H', 'MULTIRESPONSE_HS',
                                          'SHORTANSWER', 'SHORTANSWER_C' ) ) {
                    notes <- question[ idx.reponses, colonne.note ]
                    if ( is.numeric( notes ) ) {
                        if ( max( abs( notes ) ) <= 1 ) {
                            notes <- notes * 100
                        }
                        multi.reponses$Notes <- round( notes, 0 )
                    }
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
                cat( "   champ ", i, " : ouvert, \"", reponse, "\"",
                     sep = "" )

                types[ i ] <- "SHORTANSWER"
                if ( !is.na( colonne.type ) ) {
                    type.champ <- question[ i.ligne, colonne.type ]
                    type.champ <- tolower( type.champ )
                    if ( type.champ %in% c( 'c', 'sc', 'sac', 'shortanswer_c', 'cs',
                                            'cas', 'acs', 'asc', 'csa', 'sca' ) ) {
                        types[ i ] <- "SHORTANSWER_C"

                        cat( " (sensible \u00e0 la casse)" )
                    }
                }
                cat( "\n" )
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
                           titre = titre,
                           fichier.xml = fichier.xml,
                           commentaire.global = commentaire.global,
                           note.question = note.question, penalite = penalite.question,
                           temps = temps )

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

                                  ## On distingue le nom de fichier et les indications textuelles
                                  pos.sep <- gregexpr( "[|]", img )[[ 1 ]]
                                  if ( -1 == pos.sep ) {
                                      ## Pas d'indication de texte alternatif
                                      description <- NA
                                  } else {
                                      description <- substr( img, pos.sep + 1, nchar( img ) )
                                      img <- substr( img, 1, pos.sep - 1 )
                                  }

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
                                  if ( is.na( description ) ) description <- img
                                  img <- lier_image.moodle( paste0( dossier.images, "/", img ), 
                                                            largeur = largeur, hauteur = hauteur,
                                                            description = description,
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

                                  ## On distingue le code SMILES et les indications textuelles
                                  pos.sep <- gregexpr( "[|]", frm )[[ 1 ]]
                                  if ( -1 == pos.sep ) {
                                      ## Pas d'indication de texte alternatif
                                      description <- NA
                                  } else {
                                      description <- substr( frm, pos.sep + 1, nchar( frm ) )
                                      frm <- substr( frm, 1, pos.sep - 1 )
                                  }

                                  ## On créer la formule
                                  if ( is.na( description ) ) description <- frm
                                  frm <- inserer_SMILES.moodle( frm, marges = FALSE,
                                                                nom.molecule = description )

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
