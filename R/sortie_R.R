## —————————————————————————————————————————————————————————————————
## Création de XML Moodle avec R
## Emmanuel Curis — juin 2015 - mars 2020
##
## Fonctions permettant la création de sorties R
## —————————————————————————————————————————————————————————————————
## HISTORIQUE
##   17 avr. 2020 : création du fichier
## —————————————————————————————————————————————————————————————————

sortie_R.moodle <- function( objet.R, ... ) {
    UseMethod( "sortie_R.moodle", objet.R )
}

encadrer <- function( texte, commande = NULL, cadre = TRUE, 
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
    chaine <- paste0( chaine,
                      " font-family: monospace;" )

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
sortie_R.moodle.numeric <- function( objet.R, precision = 4, ... ) {
    noms <- names( objet.R )

    chaine <- paste0( "<table noborder",
                      " style=\"margin: 1px 25px;",
                      " border-collapse: separate; border-spacing: 20px 0px;",
                      "\">" )
    if ( length( noms ) > 0 ) {
        chaine <- paste0( chaine, " <tr>",
                          paste0( "  <td style=\"text-align: right;\">",
                                  noms,
                                  "</td>" ),
                          "</tr>\n" )
    }
    chaine <- paste0( chaine, " <tr>",
                      paste0( "  <td style=\"text-align: right;\">",
                              format( objet.R, digits = precision ),
                              "</td>" ),
                      "</tr>\n" )
    chaine <- paste0( chaine, "</table>\n" )
    
    ## On encadre
    chaine <- encadrer( chaine, ... )
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
