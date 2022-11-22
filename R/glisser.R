## —————————————————————————————————————————————————————————————————
## Création de XML Moodle avec R
## Emmanuel Curis — juin 2015
##
## Question de type « glisser-déposer »
## —————————————————————————————————————————————————————————————————
## HISTORIQUE
##   28 avr. 2020 : création du fichier
##
##   29 avr. 2020 : variante avec zones libres (« légendage »)
##
##   30 avr. 2020 : corrigé le légendage (récupération des zones)
##                  questions « glisser du texte dans un texte »
##
##   12 mai  2020 : options génériques (note, pénalité) gérées
##
##   15 mai  2020 : si une seule indication « infini » pour les textes
##                   à trou, elle est utilisée pour toutes les étiquettes
##
##   30 mai  2020 : les coordonnées hors-image sont ramenées au bord
##                   (pour le légendage ; à voir si nécessaire pour glisser_deposer)
##
##   31 mai  2020 : ajout du temps conseillé pour répondre
##
##    1 jan. 2021 : prise en compte de l'identifiant numérique
##                  gestion des groupes d'étiquettes pour le glisser-déposer sur image
##                  possibilité d'étiquettes sans zone associée dans le glisser-déposer sur image
##
##    3 jui. 2022 : adaptation pour utiliser le temps de catégorie
## —————————————————————————————————————————————————————————————————

## —————————————————————————————————————————————————————————————————
##
##                    Glisser-déposer : image ou texte
## 
## —————————————————————————————————————————————————————————————————

glisser_deposer.moodle <- function( texte, titre = "Glisser-d\u00e9poser...",
                                    f.creer_figure,
                                    fichier.image,
                                    x.zones, y.zones, txt.zones,
                                    indications = paste0( "Zone ", 1:n.zones ),
                                    img.zones = NULL, zone.unique = TRUE, 
                                    grp.zones = rep( 1, n.zones ),
                                    ordre.aleatoire = TRUE,
                                    commentaire.global = NA, penalite = NA, note.question = NA,
                                    idnum = NA, temps,
                                    fichier.xml = get( "fichier.xml", envir = SARP.Moodle.env ),
                                    ... )
{
    ## On ajoute l'indication de temps éventuelle
    texte <- paste0( texte, 
                     temps_necessaire.moodle( temps ) )
    
    ## On commence la question
    debut_question.moodle( type = "ddimageortext",
                           titre = titre, texte = texte,
                           penalite = penalite, note = note.question,
                           commentaire.global = commentaire.global,
                           idnum = idnum,
                           fichier.xml = fichier.xml )

    if ( ordre.aleatoire ) {
        cat( file = fichier.xml, sep = "",
             "    <shuffleanswers/>\n" )
    }

    ## On crée l'image de fond
    if ( !missing( f.creer_figure ) ) {
        ## On crée l'image
        fichier.image <- tempfile( "gd_", fileext = ".png" )
        png( fichier.image, width = 600, height = 400 )

        ## On appelle la fonction pour le dessin
        ##   Elle doit renvoyer une data.frame avec au moins $X, $Y, $Textes
        zones <- f.creer_figure( ... )

        ## On convertit les coordonnées en pixels
        x.zones <- grconvertX( zones$X, from = "user", to = "device" )
        y.zones <- grconvertY( zones$Y, from = "user", to = "device" )

        ## On récupère les textes
        ##   (dans une data.frame, ont pu être convertis en facteurs :()
        txt.zones <- as.character( zones$Textes )

        ## On récupère les indications, s'il y en a
        ##   (dans une data.frame, ont pu être convertis en facteurs :()
        if ( length( zones$Indications ) > 0 ) {
            indications <- as.character( zones$Indications )
        } else {
            indications <- paste0( "Zone ", 1:nrow( zones ) )
        }

        ## On récupère les images, s'il y en a
        if ( length( zones$Images ) > 0 ) {
            img.zones <- zones$Images
        }

        ## On récupère les groupes d'étiquettes, s'il y en a
        if ( length( zones$Groupes ) > 0 ) {
            grp.zones <- zones$Groupes
        } else {
            grp.zones <- rep( 1, nrow( zones ) )
        }

        ## On récupère l'unicité de l'étiquette
        if ( length( zones$Unique ) > 0 ) {
            zone.unique <- zones$Unique
        } else {
            zone.unique <- rep( TRUE, nrow( zones ) )
        }        

        ## On termine l'image
        dev.off()
    }

    ## On crée l'image de fond
    if ( FALSE == file.exists( fichier.image ) ) {
        stop( "Fichier d'image de fond inexistant !" )
    }
    cat( file = fichier.xml, sep = "",
         "    <file name=\"", gsub( "^.*/", "", fichier.image ),
         "\" encoding=\"base64\">",
         base64enc::base64encode( fichier.image ),
         "</file>\n" )

    ## Combien de zones ?
    n.zones <- length( x.zones )
    if ( length( y.zones ) != n.zones ) {
        stop( "Pas autant de X que de Y !" )
    }
    if ( length( txt.zones ) != n.zones ) {
        stop( "Pas autant de textes que de coordonn\u00e9es !" )
    }

    if ( 1 == length( zone.unique ) ) {
        zone.unique <- rep( zone.unique, n.zones )
    }
    if ( length( zone.unique )!= n.zones ) {
        stop( "Pas autant d'indicateurs d'unicit\u00e9 que de coordonn\u00e9es !" )
    }
    
    ## On crée les éléments de réponse
    for ( i in 1:n.zones ) {
        cat( file = fichier.xml, sep = "",
             "    <drag>\n",
             "      <no>", i, "</no>\n" )
        coder.texte( txt.zones[ i ], fichier.xml = fichier.xml )
        cat( file = fichier.xml, sep = "",
             "      <draggroup>", grp.zones[ i ], "</draggroup>\n" )

        if ( zone.unique[ i ] == FALSE ) {
            ## On peut s'en resservir
            cat( file = fichier.xml, sep = "",
                 "      <infinite/>\n" )
        }
        cat( file = fichier.xml, sep = "",
             "    </drag>\n" )
    }

    ## On crée les zones où les placer
    ordre.choix <- 1:n.zones
    if ( ordre.aleatoire ) {
        ordre.choix <- sample( ordre.choix, n.zones, replace = FALSE )
    }
    for ( i in 1:n.zones ) {
        if ( all( is.finite( x.zones[ i ] ),
                  is.finite( y.zones[ i ] ) ) ) {
            cat( file = fichier.xml, sep = "",
                 "    <drop>\n" )
            coder.texte( indications[ i ], fichier.xml = fichier.xml )
            cat( file = fichier.xml, sep = "",
                 "      <no>", ordre.choix[ i ], "</no>\n", # Ordre d'apparition ?
                 "      <choice>", i , "</choice>\n", # Numéro de la zone ?
                 "      <xleft>", x.zones[ i ], "</xleft>\n",
                 "      <ytop>" , y.zones[ i ], "</ytop>\n",
                "    </drop>\n" )
        }
    }
    
    ## On a fini la question
    fin_question.moodle( fichier.xml = fichier.xml  )
}


## —————————————————————————————————————————————————————————————————
##
##                  Glisser-déposer : légender une image
## 
## —————————————————————————————————————————————————————————————————

legender_image.moodle <- function( texte, titre = "L\u00e9gender...",
                                   f.creer_figure,
                                   fichier.image,
                                   zones, marques,
                                   ordre.aleatoire = TRUE, afficher.erreurs = TRUE,
                                   commentaire.global = NA, penalite = NA, note.question = NA,
                                   idnum = NA,
                                   temps,
                                   fichier.xml = get( "fichier.xml", envir = SARP.Moodle.env ),
                                   ... )
{
    ## On ajoute l'indication de temps éventuelle
    texte <- paste0( texte, 
                     temps_necessaire.moodle( temps ) )
    
    ## On commence la question
    debut_question.moodle( type = "ddmarker",
                           titre = titre, texte = texte,
                           penalite = penalite, note = note.question,
                           commentaire.global = commentaire.global,
                           idnum = idnum,
                           fichier.xml = fichier.xml )

    if ( ordre.aleatoire ) {
        cat( file = fichier.xml, sep = "",
             "    <shuffleanswers/>\n" )
    }

    if ( afficher.erreurs ) {
        cat( file = fichier.xml, sep = "",
             "    <showmisplaced/>\n" )
    }
    
    ## On crée l'image de fond
    if ( !missing( f.creer_figure ) ) {
        ## On crée l'image
        fichier.image <- tempfile( "gd_", fileext = ".png" )
        png( fichier.image, width = 600, height = 400 )

        ## On appelle la fonction pour le dessin
        ##   Elle doit renvoyer une liste de zones
        lst.zones <- f.creer_figure( ... )

        ## On récupère les marques
        marques <- lst.zones$Marques
        marques$Marque <- as.character( marques$Marque )

        ## On récupère la liste des zones
        zones <- lapply( lst.zones$Zones,
                         function( z ) {
                             z$Type <- tolower( z$Type )
                             ## print( z$Coord )
                             z$Coord$X <- grconvertX( z$Coord$X,
                                                      from = "user", to = "device" )
                             z$Coord$Y <- grconvertY( z$Coord$Y,
                                                      from = "user", to = "device" )

                             ## On vérifie que l'on ne déborde pas de l'image…
                             if ( any( z$Coord$X < 0 ) ) {
                                 z$Coord$X[ which( z$Coord$X < 0 ) ] <- 0
                             }
                             if ( any( z$Coord$Y < 0 ) ) {
                                 z$Coord$Y[ which( z$Coord$Y < 0 ) ] <- 0
                             }
                             if ( any( z$Coord$X > 600 ) ) {
                                 z$Coord$X[ which( z$Coord$X > 600 ) ] <- 600
                             }
                             if ( any( z$Coord$Y > 400 ) ) {
                                 z$Coord$Y[ which( z$Coord$Y > 400 ) ] <- 400
                             }
                             ## print( z$Coord )

                             ## Rectangle : coin (h,g) + largeur, hauteur
                             if ( z$Type == 'rectangle' ) {
                                 ## On convertit en largeur, hauteur
                                 z$Coord$Largeur <- abs( diff( z$Coord$X ) )
                                 z$Coord$Hauteur <- abs( diff( z$Coord$Y ) )

                                 z$Coord$X <- min( z$Coord$X )
                                 z$Coord$Y <- min( z$Coord$Y )
                             }

                             ## Cercle : centre + rayon
                             if ( z$Type == 'circle' ) {
                                 ## On convertit le rayon en X et en Y
                                 rX <- grconvertX( z$Coord$Rayon, from = "user", to = "device" )
                                 rY <- grconvertY( z$Coord$Rayon, from = "user", to = "device" )

                                 ## Et on prend le max...
                                 z$Coord$Rayon <- max( rX, rY )
                             }

                             z
                         } )

        ## On termine l'image
        dev.off()
    }
    
    ## On crée l'image de fond
    if ( FALSE == file.exists( fichier.image ) ) {
        stop( "Fichier d'image de fond inexistant !" )
    }
    cat( file = fichier.xml, sep = "",
         "    <file name=\"", gsub( "^.*/", "", fichier.image ),
         "\" encoding=\"base64\">",
         base64enc::base64encode( fichier.image ),
         "</file>\n" )

    ## On crée les étiquettes
    if ( FALSE == is.data.frame( marques ) ) {
        marques <- data.frame( "Marque" = marques,
                               "Nombre" = rep( 1, length( marques ) ),
                               stringAsFactors = FALSE )
    }
    if ( FALSE == ( 'Nombre' %in% names( marques ) ) ) {
        marques$Nombre <- rep( 1, nrow( marques ) )
    }
    marques$Nombre <- as.integer( marques$Nombre )
    for ( i in 1:nrow( marques ) ) {
        ##        marque <- marques[ i, ]

        ## Le texte de la marque
        cat( file = fichier.xml, sep = "",
             "    <drag>\n",
             "      <no>", i, "</no>\n    " )
            coder.texte( marques$Marque[ i ], fichier.xml = fichier.xml )

        multiple <- marques$Nombre[ i ]
        if ( any( is.na( multiple ), multiple < 1,
                  multiple == +Inf ) ) {
            cat( file = fichier.xml, sep = "",
                 "      <infinite/>\n" )
            multiple <- 1
        }
        cat( file = fichier.xml, sep = "",
             "      <noofdrags>", multiple, "</noofdrags>\n",
             "    </drag>\n" )
    }
    
    ## On crée les zones
    ## print( marques ) ;
    ## print( zones )
    for ( i in 1:length( zones ) ) {
        zone <- zones[[ i ]]
#        print( zone )
        
        ## On repère quelle est la bonne marque
        num.marque <- zone$Marque[ 1 ]
        if ( is.character( zone$Marque[ 1 ] ) ) {
            num.marque <- which( marques$Marque == num.marque )
#            zone$Marque[ 1 ] <- marques$Marque[ zone$Marque[ 1 ] ]
        }
        if ( num.marque > nrow( marques ) ) {
            stop( "Marque voulue : ", num.marque,
                  " mais seulement ", nrow( marques ), " d\u00e9finies" )
        }
        
  #      num.marque <- which( marques$Marque == zone$Marque[ 1 ] )
        ## cat( sep = "", "Marque voulue : " , zone$Marque[ 1 ], "\n",
        ##      "Marque textuelle : ", marques$Marque[ zone$Marque[ 1 ] ], "\n",
        ##      "Marque trouvée : ", num.marque , "\n" ) 
        
        ## On crée la zone
        cat( file = fichier.xml, sep = "",
             "    <drop>\n",
             "      <no>", i, "</no>\n" )

        ## La forme de la zone
        type <- tolower( zone$Type )
        if ( type %in% c( 'cercle', 'circle' ) ) type <- "circle"
        if ( type %in% c( 'carre', 'carr\u00e9', 'square', 'rect' ) ) type <- "rectangle"
        if ( type %in% c( 'polygone' ) ) type <- "polygon"

        ## Les coordonnées en pixels : entières
        zone$Coord$X <- as.integer( round( zone$Coord$X, 0 ) )
        zone$Coord$Y <- as.integer( round( zone$Coord$Y, 0 ) )
        ## print( zone$Coord )
        if ( 'Rayon' %in% names( zone$Coord ) )
            zone$Coord$Rayon <- as.integer( round( zone$Coord$Rayon, 0 ) )
        if ( 'Largeur' %in% names( zone$Coord ) )
            zone$Coord$Largeur <- as.integer( round( zone$Coord$Largeur, 0 ) )
        if ( 'Hauteur' %in% names( zone$Coord ) )
            zone$Coord$Hauteur <- as.integer( round( zone$Coord$Hauteur, 0 ) )
        cat( file = fichier.xml, sep = "",
             "      <shape>", type, "</shape>\n",
             "      <coords>",
             switch( type,
                     "circle"    = paste0( zone$Coord$X[ 1 ], ",",
                                           zone$Coord$Y[ 1 ], ";",
                                           zone$Coord$Rayon[ 1 ], collapse = "" ),
                     "rectangle" = paste0( zone$Coord$X[ 1 ], ",",
                                           zone$Coord$Y[ 1 ], ";",
                                           zone$Coord$Largeur[ 1 ], ",",
                                           zone$Coord$Hauteur[ 1 ], collapse = "" ),
                     "polygon"   = paste0( paste0( zone$Coord$X, ",", zone$Coord$Y ),
                                           collapse = ";" ),
                     stop( "Type de forme inconnu [", type, "]" ) ),
             "</coords>\n" )

        ## La réponse qui va avec        
        cat( file = fichier.xml, sep = "",
             "      <choice>", num.marque, "</choice>\n",
             "    </drop>\n" )
    }
    
    ## On a fini la question
    fin_question.moodle( fichier.xml = fichier.xml  )
}


## —————————————————————————————————————————————————————————————————
##
##               Glisser-déposer : des textes sur un texte
## 
## —————————————————————————————————————————————————————————————————

glisser_textes.moodle <- function( texte, titre = "Glisser les textes...",
                                   groupe = rep( 1, n.zones ), infini = FALSE,
                                   ordre.aleatoire = TRUE, afficher.erreurs = TRUE,
                                   commentaire.global = NA, penalite = NA, note.question = NA,
                                   idnum = NA, temps,
                                   fichier.xml = get( "fichier.xml", envir = SARP.Moodle.env ) )
{
    ## On prépare l'énoncé et les zones
    ## print( texte )
    zones <- strsplit( texte, split = "[[", fixed = TRUE )
    zones <- zones[[ 1 ]]

    n.zones <- length( zones ) - 1
    if ( n.zones < 1 ) {
        stop( "Aucune zone d\u00e9finie dans le texte" )
    }

    txt.zones <- gsub( "(.*)\\]\\].*$", "\\1", zones[ -1 ] )
    zones <- c( zones[ 1 ],  gsub( ".*\\]\\](.*)$", "\\1", zones[ -1 ] ) )
    ordre <- 1:n.zones
    if ( ordre.aleatoire ) {
        ordre <- sample( 1:n.zones, n.zones, replace = FALSE )
    }
    ## print( ordre )

    ## On reconstruit l'énoncé, avec les numéros des zones...
    texte <- paste0( zones, "[[", ordre, "]]", collapse = "" )
    texte <- gsub( "\\[\\[[0-9]\\]\\]$", "", texte )
    ## print( texte )

    ## On ajoute l'indication de temps éventuelle
    texte <- paste0( texte, 
                     temps_necessaire.moodle( temps ) )
    
    ## On commence la question
    debut_question.moodle( type = "ddwtos",
                           titre = titre, texte = texte,
                           penalite = penalite, note = note.question,
                           commentaire.global = commentaire.global,
                           idnum = idnum,
                           fichier.xml = fichier.xml )

    if ( ordre.aleatoire ) {
        cat( file = fichier.xml, sep = "",
             "    <shuffleanswers/>\n" )
    }

    if ( afficher.erreurs ) {
        cat( file = fichier.xml, sep = "",
             "    <showmisplaced/>\n" )
    }    

    if ( length( infini ) == 1 ) {
        infini <- rep( infini, n.zones )
    }
    if ( length( infini ) != n.zones ) {
        stop( "Pas autant d'indications \"Infini\" que de zones de texte" )
    }
    
    ## On crée les étiquettes à déplacer
    for ( i in 1:n.zones ) {
        ## On crée l'étiquette
        idx <- which( ordre == i )
        ## print( paste( ordre[ i ], " — ", txt.zones[ i ], " — ", txt.zones[ ordre[ i ] ], " — ", txt.zones[ idx ] ) )
        
        cat( file = fichier.xml, sep = "",
             "    <dragbox>\n" )
        coder.texte( txt.zones[ idx ], fichier.xml = fichier.xml )
        cat( file = fichier.xml, sep = "",
             "      <group>", groupe[ idx ], "</group>\n",
             if ( infini[ idx ] ) "      <infinite/>\n",
             "    </dragbox>\n" )
    }
    
    ## On a fini la question
    fin_question.moodle( fichier.xml = fichier.xml  )
}
