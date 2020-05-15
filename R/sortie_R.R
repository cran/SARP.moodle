## —————————————————————————————————————————————————————————————————
## Création de XML Moodle avec R
## Emmanuel Curis — juin 2015 - mars 2020
##
## Fonctions permettant la création de sorties R
## —————————————————————————————————————————————————————————————————
## HISTORIQUE
##   17 avr. 2020 : création du fichier
##
##   30 avr. 2020 : version avec enjolivement
##                   (vecteurs numériques)
## —————————————————————————————————————————————————————————————————

sortie_R.moodle <- function( objet.R, ... ) {
    UseMethod( "sortie_R.moodle", objet.R )
}

encadrer <- function( texte, commande = NULL, cadre = TRUE, courrier = cadre,
                      couleur.commande = "Blue", prompt = "&gt;",
                      couleur.cadre = "Black", largeur.cadre = 2 )
{ 
    ## On commence la section spéciale
    chaine <- "<div style=\""
    
    ## On précise le cadre, si demandé
    if ( cadre ) {
        chaine <- paste0( chaine,
                          "border: ", largeur.cadre, "px solid",
                          " ", couleur.cadre, ";" )
    }

    ## Police « largeur fixe »
    if ( courrier ) {
        chaine <- paste0( chaine,
                          " font-family: monospace;" )
    }

    ## On a fini le style
    chaine <- paste0( chaine, "\">\n" )

    ## Si demandé, on met la commande
    if ( length( commande ) > 0 ) {
        chaine <- paste0( chaine,
                          "<span style=\"color: ", couleur.commande, ";\">",
                          prompt, " ", commande,
                          "</span><br />\n" )
    }

    ## On met le texte
    chaine <- paste0( chaine, texte )

    ## On fait la fin de la section
    chaine <- paste0( chaine, "</div>\n" )
    
    ## On a fini, on renvoie le résultat
    chaine
}

##
## Conversion pour une commande protégée (par quote)
## 

sortie_R.moodle.call <- function( objet.R, precision = 4, ... ) {
    ## On exécute la commande et convertit son résultat en chaîne
    resultat <- capture.output( eval( objet.R ) )

    ## On fait sauter les numéros de ligne
    resultat <- gsub( "^[[:space:]]*\\[[[:digit:]]+\\]", "", resultat )
    
    ## On en fait une chaîne unique, en respectant les lignes
    resultat <- paste0( chaine, collapse = "<br />\n" )

    ## On encadre
    chaine <- encadrer( resultat,
                        commande = deparse( objet.R ), ... )
    
    ## Fini
    chaine
}

##
## Conversion pour un résultat de test [classe htest]
## 

sortie_R.moodle.htest <- function( objet.R, precision = 4, ... )
{
    ## Le titre, centré
    chaine <- paste0( "<br />\n",
                      "<div style=\"text-align: center;\">",
                      objet.R$method,
                      "</div>\n" )

    ## Le jeu de données
    chaine <- paste0( chaine, "<br />\n",
                      "data:  ", objet.R$data.name )

    ## La statistique de test
    tmp <- c()
    if ( !is.null( objet.R$statistic ) ) {
        if ( is.numeric( objet.R$statistic ) ) {
            noms <- names( objet.R$statistic )
            objet.R$statistic <- format( objet.R$statistic, 
                                         digits = precision )
            names( objet.R$statistic ) <- noms
        }
        
        tmp <- c( tmp, paste( names( objet.R$statistic ), "=", 
                              objet.R$statistic ) )
    }
    if ( !is.null( objet.R$parameter ) ) {
        if ( is.numeric( objet.R$parameter ) ) {
            noms <- names( objet.R$parameter )
            objet.R$parameter <- format( objet.R$parameter, 
                                            digits = precision )
            names( objet.R$parameter ) <- noms
        }
        tmp <- c( tmp, paste( names( objet.R$parameter ), "=", 
                              objet.R$parameter ) )
    }

    ## Le degré de signification
    if ( !is.null( objet.R$p.value ) ) {
        txt.p <- objet.R$p.value
        if ( is.numeric( txt.p ) ) {
            txt.p <- format.pval( txt.p, digits = precision )
        }
        tmp <- c( tmp, paste( "p-value", 
                              if ( substr( txt.p, 1L, 1L) == "<") txt.p else paste( "=" , txt.p ) ) )
    }
    
    chaine <- paste0( chaine, "<br />\n",
                      paste( tmp, collapse = ", " ) )

    ## L'hypothèse alternative
    if ( !is.null( objet.R$alternative ) ) {
        chaine <- paste0( chaine, "<br />\n",
                          "alternative hypothesis: ",
                          if ( !is.null( objet.R$null.value) ) {
                              if ( length( objet.R$null.value ) == 1L ) {
                                  alt.char <- switch( objet.R$alternative, 
                                                      two.sided = "not equal to", 
                                                      less = "less than", 
                                                      greater = "greater than" )
                                  paste0( "true ", names( objet.R$null.value ),
                                          " is ", alt.char, 
                                          " ", objet.R$null.value, "\n" )
                              } else {
                                  paste0( objet.R$alternative,
                                          "<br />\n",
                                          "null values:", "<br />\n",
                                          format( objet.R$null.value, digits = precision, ... ) )
                              }
                          } else {
                              objet.R$alternative
                          } )
    }

    ## L'intervalle de confiance
    if ( !is.null( objet.R$conf.int ) ) {
        chaine <- paste0( chaine, "<br />\n",
                          format( 100 * attr( objet.R$conf.int, "conf.level") ),
                          " percent confidence interval:",
                          "<br />\n", 
                          " ", paste( format( c( objet.R$conf.int[ 1L ],
                                                 objet.R$conf.int[ 2L ] ),
                                              digits = precision ),
                                      collapse = " ") )
    }

    ## Les valeurs estimées
    if ( !is.null( objet.R$estimate ) ) {
        chaine <- paste0( chaine, "<br />\n",
                          "sample estimates:",
                           "<br />\n" )
        if ( length( objet.R$estimate ) > 1 ) {
            chaine <- paste0( chaine,
                              "<table noborder",
                              " style=\"margin: 1px 25px; border-collapse: separate; border-spacing: 20px 0px;\">",
                              " <tr>",
                              paste( "<td>", names( objet.R$estimate ), "</td>",
                                     collapse = " " ),
                              "</tr>\n",
                              " <tr>",
                              paste( "<td>",
                                     format( objet.R$estimate,
                                             digits = precision ),
                                     "</td>",
                                     collapse = " " ),
                              "</tr>\n",
                              "</table>" )
        } else {
            chaine <- paste0( chaine,
                              names( objet.R$estimate ), "<br />\n",
                              format( signif( objet.R$estimate, digits = precision ),
                                      digits = precision ) )
        }
    }

    ## Fini
    chaine <- encadrer( chaine, ... )

    ## On renvoie la chaîne
    chaine
}


##
## Conversion pour un vecteur numérique [classe numeric]
##
##  enjoliver     : si TRUE, on fait un joli tableau
##  couleur.trait : la couleur à utiliser pour les traits du tableau
##  lg.trait      : l'épaisseur de trait à utiliser pour le tableau (px)
##  marge         : les valeurs de marge à mettre dans les case
##                   gauche, droite, haut, bas — en pixels
sortie_R.moodle.table <- function( objet.R, precision = 4,
                                   enjoliver = !cadre, cadre = TRUE,
                                   couleur.trait = "Black", lg.trait = "2",
                                   marge = if ( enjoliver ) c( 10, 10, 1, 1 ) else c( 25, 25, 1, 1 ),
                                   avec.marges = c( FALSE, FALSE ),
                                   ... )
{
    ## On prépare les styles
    if ( FALSE == enjoliver ) {
        style.table <- paste0( paste0( "margin-", c( 'left', 'right', 'top', 'bottom' ),
                                       ": ", marge, "px",
                                       collapse = "; " ), ";",
                               " border-collapse: separate;",
                               " border-spacing: 20px 0px;" )

        style.noms <- "text-align: right;"
        style.contenu <- "text-align: right;"
        style.ligne <- ""
    } else {
        style.table <- paste0( "border-collapse: collapse;",
                               " text-align: center;" )
        style.noms <- "font-weight: bold;"
        style.contenu <- paste0( paste0( "padding-", c( 'left', 'right', 'top', 'bottom' ),
                                         ": ", marge, "px",
                                         collapse = "; " ), ";" )
        style.ligne <- paste0( paste0( "border-", c( "top", "bottom" ), ": ",
                                       lg.trait, "px solid ", couleur.trait, ";",
                                       collapse = " " ) )
    }

    ## Nombre de dimensions de la table
    n.dimensions <- length( dim( objet.R ) )

    ## On commence la table
    chaine <- paste0( "<div style=\"overflow-x:auto;\">",
                      "<table noborder style=\"", style.table, "\">" )

    if ( n.dimensions == 1 ) {
    }
    
    ## Table finie
    chaine <- paste0( chaine, "</table>\n",
                      "</div>\n" )
    
    ## On encadre
    chaine <- encadrer( chaine, cadre = cadre, ... )
    chaine
}

##
## Conversion pour un vecteur numérique [classe numeric]
##
##  enjoliver     : si TRUE, on fait un joli tableau
##  couleur.trait : la couleur à utiliser pour les traits du tableau
##  lg.trait      : l'épaisseur de trait à utiliser pour le tableau (px)
##  marge         : les valeurs de marge à mettre dans les case
##                   gauche, droite, haut, bas — en pixels
##  pre.X, pre.X.nom : des textes à mettre en 1re colonne, comme légende
##                     (utilisés seulement si enjoliver = TRUE)
sortie_R.moodle.numeric <- function( objet.R, precision = 4,
                                     enjoliver = !cadre, cadre = TRUE,
                                     couleur.trait = "Black", lg.trait = "2",
                                     marge = if ( enjoliver ) c( 10, 10, 1, 1 ) else c( 25, 25, 1, 1 ),
                                     pre.X = NA, pre.X.nom = NA, noms.gras = TRUE,
                                     ... )
{
    ## On prépare les styles
    if ( FALSE == enjoliver ) {
        style.table <- paste0( paste0( "margin-", c( 'left', 'right', 'top', 'bottom' ),
                                       ": ", marge, "px",
                                       collapse = "; " ), ";",
                               " border-collapse: separate;",
                               " border-spacing: 20px 0px;" )

        style.noms <- "text-align: right;"
        style.contenu <- "text-align: right;"
        style.ligne <- ""
        pre.colonne <- FALSE
    } else {
        style.table <- paste0( "border-collapse: collapse;",
                               " text-align: center;" )
        style.noms <- if( noms.gras ) "font-weight: bold;" else ""
        style.contenu <- paste0( paste0( "padding-", c( 'left', 'right', 'top', 'bottom' ),
                                         ": ", marge, "px",
                                         collapse = "; " ), ";" )
        style.ligne <- paste0( paste0( "border-", c( "top", "bottom" ), ": ",
                                       lg.trait, "px solid ", couleur.trait, ";",
                                       collapse = " " ) )

        pre.colonne <- all( ( length( pre.X ) > 0 ) | ( length( pre.X.nom ) > 0 ),
                            ( nchar( pre.X ) > 0 ) | ( nchar( pre.X.nom ) > 0 ),
                            ( !is.na( pre.X ) ) | ( !is.na( pre.X.nom ) ) )
        style.precolonne <- paste0( "text-align: right; ",
                                    "font-weight: bold; ",
                                     paste0( "padding-", c( 'left', 'right', 'top', 'bottom' ),
                                             ": ", marge, "px",
                                             collapse = "; " ), ";" )
#        print( pre.colonne ) ; print( pre.X )
    }

    ## Le début de la table
    chaine <- paste0( "<div style=\"overflow-x:auto;\">",
                      "<table noborder style=\"", style.table, "\">" )

    ## Les titres (s'il y a des noms)
    noms <- names( objet.R )
    if ( length( noms ) > 0 ) {
        chaine <- paste0( chaine, " <tr style=\"", style.ligne, "\">" )
        if ( pre.colonne ) {
            chaine <- paste0( chaine,
                              " <th style=\"", style.precolonne, "\">",
                              pre.X.nom, "</th>" )
        }
        chaine <- paste0( chaine,
                          paste0( "  <td style=\"", style.noms, "\">",
                                  noms,
                                  "</td>", collapse = " " ),
                          "</tr>\n" )
    }

    ## Le contenu
    if ( enjoliver ) {
        nombres <- unlist( lapply( objet.R, afficher_nombre.moodle,
                                   n.chiffres = precision ) )
    } else {
        nombres <- format( objet.R, digits = precision )
    }
    chaine <- paste0( chaine,
                      " <tr style=\"", style.ligne, "\">" )
    if ( pre.colonne ) {
        chaine <- paste0( chaine,
                          " <th style=\"", style.precolonne, "\">",
                          pre.X, "</th>" )
    }
    chaine <- paste0( chaine,
                      paste0( "  ",
                              "<td style=\"", style.contenu, "\">",
                              "<! V", 1:length( objet.R ), ">", # On précise les débuts de case, si on veut le retravailler ensuite !
                              nombres,
                              "</td>", collapse = " " ),
                      "</tr>\n" )

    ## La table est finie
    chaine <- paste0( chaine, "</table>\n",
                      "</div>\n" )
    
    ## On encadre
    chaine <- encadrer( chaine, cadre = cadre, ... )
    chaine
}


##
## Conversion par défaut [résultat brut de print]
## 

sortie_R.moodle.default <- function( objet.R, precision = 4, ... ) {
    ## On exécute la commande
    ##  avec les bons nombres de chiffres significatifs
    vx <- options( "digits" = precision )
    chaine <- capture.output( print( objet.R ) )
    options( vx )

    ## On fait sauter les numéros de ligne
    chaine <- gsub( "^[[:space:]]*\\[[[:digit:]]+\\]", "", chaine )
    
    ## On en fait une chaîne unique, en respectant les lignes
    chaine <- paste0( chaine, collapse = "<br />\n" )

    ## On encadre
    chaine <- encadrer( chaine, ... )
    chaine
}
