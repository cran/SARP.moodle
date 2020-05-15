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
                                    ordre.aleatoire = TRUE,
                                    commentaire.global = NA, penalite = NA, note.question = NA,
                                    fichier.xml = get( "fichier.xml", envir = SARP.Moodle.env ),
                                    ... )
{
    ## On commence la question
    debut_question.moodle( type = "ddimageortext",
                           titre = titre, texte = texte,
                           penalite = penalite, note = note.question,
                           commentaire.global = commentaire.global,
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
        ##   Elle doit renvoyer une data.frame $X, $Y, $Textes
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
             "      <draggroup>1</draggroup>\n" ) # ???

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
                                   fichier.xml = get( "fichier.xml", envir = SARP.Moodle.env ),
                                    ... )
{
    ## On commence la question
    debut_question.moodle( type = "ddmarker",
                           titre = titre, texte = texte,
                           penalite = penalite, note = note.question,
                           commentaire.global = commentaire.global,
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
                             z$Coord$X <- grconvertX( z$Coord$X,
                                                      from = "user", to = "device" )
                             z$Coord$Y <- grconvertY( z$Coord$Y,
                                                      from = "user", to = "device" )

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
        marque <- marques[ i, ]

        ## Le texte de la marque
        cat( file = fichier.xml, sep = "",
             "    <drag>\n",
             "      <no>", i, "</no>\n" )
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
    for ( i in 1:length( zones ) ) {
        zone <- zones[[ i ]]
#        print( zone )
        
        ## On repère quelle est la bonne marque
        if ( is.integer( zone$Marque[ 1 ] ) ) zone$Marque[ 1 ] <- marques[ zone$Marque[ 1 ] ]
        num.marque <- which( marques == zone$Marque[ 1 ] )

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
                                   groupe = rep( 1, n.zones ), infini = rep( FALSE, n.zones ),
                                   ordre.aleatoire = TRUE, afficher.erreurs = TRUE,
                                   commentaire.global = NA, penalite = NA, note.question = NA,
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
    
    ## On commence la question
    debut_question.moodle( type = "ddwtos",
                           titre = titre, texte = texte,
                           penalite = penalite, note = note.question,
                           commentaire.global = commentaire.global,
                           fichier.xml = fichier.xml )

    if ( ordre.aleatoire ) {
        cat( file = fichier.xml, sep = "",
             "    <shuffleanswers/>\n" )
    }

    if ( afficher.erreurs ) {
        cat( file = fichier.xml, sep = "",
             "    <showmisplaced/>\n" )
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
