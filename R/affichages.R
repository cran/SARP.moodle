## —————————————————————————————————————————————————————————————————
## Création de XML Moodle avec R
## Emmanuel Curis — mars 2015, avril 2020
##
## Fonctions facilitant les affichages
## ——————————————————————————————————————————————————————————————————————
## HISTORIQUE
##   30 avr. 2020 : début de l'historique
##                  modifier affichage d'échantillon : utilise les sorties R enjolivées
##                  nombre de *décimale* correct quand on utilise la notation scientifique
##
##   31 mai  2020 : ajout d'une unité, et espace insécable forcée dans ce cas
##
##   10 juin 2020 : quand notation scientifique, on force l'insécabilité...
##
##   13 juin 2020 : affichage de plusieurs échantillons de valeurs
##
##   26 fév. 2021 : correction de caractères spéciaux mal reconnus / CRAN
##
##   18 mai  2023 : erreurs avec stop converties avec erreur()
##                  alertes avec warning converties avec avertissement()
## ——————————————————————————————————————————————————————————————————————

######################################################################
# Affichage d'un nombre
#
afficher_nombre.moodle <- function( x,
                                    dec = get( "decimal", envir = SARP.Moodle.env ),
                                    n.chiffres = get( "nombre.chiffres", envir = SARP.Moodle.env ),
                                    unite = "" )
{
    ## Y a-t-il bien une et une seule valeur ?
    if ( is.null( x ) || ( 0 == length( x ) ) ) {
        return( paste0( "<span style=\"", 
                        get( "styles", envir = SARP.Moodle.env )$erreur,
                        "\">&empty;</span>" ) )
    }
    if ( length( x ) > 1 ) {
        avertissement( 1, "afficher_nombre.moodle",
                       "Attention, seule la premi\u00e8re valeur sera convertie,",
                       " les autres seront ignor\u00e9es..." )
        x <- x[ 1 ]
    }

    ## Valeur manquante ?
    if ( is.na( x ) ) {
        return( paste0( "<span style=\"", 
                        get( "styles", envir = SARP.Moodle.env )$erreur,
                        "\">NA</span>" ) )
    }

    ## Valeur non-numérique ?
    if ( is.nan( x ) ) {
        return( "<span style=\"", 
                get( "styles", envir = SARP.Moodle.env )$erreur,
                "\">NaN</span>" )
    }

    ## Valeur infinie ?
    if ( is.infinite( x ) ) {
        if ( x > 0 )
            return( "&infin;" )
        else
            return( "&minus;&infin;" )
    }

    ## Si c'est une chaîne, on l'affiche telle qu'elle...
    if ( is.character( x ) ) {
        return( x )
    }

    ## Valeur numérique
    ##  1) conversion
    txt <- paste0( if ( log( abs( x ), 10 ) > -n.chiffres ) round( x, n.chiffres ) else signif( x, n.chiffres + 1 ) )
    
    ##  2) nettoyage : le signe -
    txt <- gsub( "-", "&minus;", fixed = TRUE, txt )
    
    ##                 le séparateur décimal
    txt <- gsub( ".", dec, fixed = TRUE, txt )

    ##                 les puissances de 10
    if ( any( grepl( "e", fixed = TRUE, txt ) ) ) {
        ## Recodage de la partie puissance de 10
        ##   (en forçant l'insécabilité, s'il n'y a pas d'unité qui le fait ensuite)
        txt <- paste0( gsub( "e", "&thinsp;&times;&thinsp;10<sup>", txt ),
                      "</sup>" )
        
        ## On ôte le signe de l'exposant, si c'est un plus, et les 0 initiaux éventuels
        txt <- gsub( "p>[+]0*", "p>", txt )
        txt <- gsub( ";0*", ";", txt )
        ## On corrige le 1×10^... en 10^...
        txt <- gsub( "^1&thinsp;&times;&thinsp;", "", txt )

        ## S'il n'y a pas d'unité, on force l'insécabilité
        ##   (sinon, sera fait en même temps que l'unité)
        if ( nchar( unite ) == 0 ) {
            txt <- paste0( "<span style=\"white-space: nowrap;\">",
                           txt,
                           "</span>" )
        }
    }

    ## On s'occupe de l'unité
    if ( nchar( unite ) > 0 ) {
        ## On enjolive l'unité si besoin
        unite <- gsub( ".", "\u00b7", unite, fixed = TRUE )
        
        ## Avec une astuce pour forcer que ce soit insécable…
        txt <- paste0( "<span style=\"white-space: nowrap;\">",
                       txt, "&nbsp;", unite,
                       "</span>" )
    }

  return( txt )
}

## ——————————————————————————————————————————————————————————————————————
## Affichage d'un échantillon de valeurs
##  (dans un tableau idéalement)
afficher_echantillon.moodle <- function( x, tableau = TRUE, trier = FALSE,
                                         n.chiffres = get( "nombre.chiffres", envir = SARP.Moodle.env ),
                                         marge = c( 10, 10, 1, 1 ), ... )
{
    ## Veut-on trier les valeurs ?
    if ( TRUE == trier ) {
        x <- sort( x )
    }

    ## Cas simple : échantillon de valeurs entre parenthèses
    if ( FALSE == tableau ) {
        txt <- unlist( lapply( x, afficher_nombre.moodle,
                               n.chiffres = n.chiffres ) )
        txt <- paste0( "(",
                       paste0( txt, collapse = "&thinsp;; " ),
                       ")" )
    } else {
        ## Ici : on veut faire un tableau
        ##   (on met des espaces dans les cellules
        ##    parce que certaines versions de moodle ne mettent aucun blanc entre les colonnes...)
        txt <- sortie_R.moodle( x, enjoliver = TRUE, cadre = FALSE,
                                marge = marge, ... )
    }
        
    ## On a fini...
    return( txt )
}


## ——————————————————————————————————————————————————————————————————————
## Affichage de plusieurs échantillons de valeurs
##  (dans un tableau)
afficher_echantillons.moodle <- function( x, trier = FALSE,
                                          n.chiffres = get( "nombre.chiffres", envir = SARP.Moodle.env ),
                                          marge = c( 10, 10, 1, 1 ), 
                                          pre.texte = paste0( "\u00c9chantillon\u00a0",
                                                              1:length( x ) ),
                                          couleur.trait = "Black", lg.trait = "2",
                                          ... )
{
    ## On traite le cas d'un seul échantillon directement…
    if ( any( length( x ) == 1, !is.list( x ) ) ) {
        return( afficher_echantillon.moodle( x, trier = trier, tableau = TRUE,
                                             marge = marge, pre.X = pre.texte[ 1 ],
                                             couleur.trait = couleur.trait, lg.trait = lg.trait,
                                             ... ) )
    }
    
    ## Veut-on trier les valeurs ?
    if ( TRUE == trier ) {
        x <- lapply( x, sort )
    }

    ## Nombre d'échantillons
    n.echantillons <- length( x )

    ## Un peu de préparation…
    style.ligne <- paste0( paste0( "border-", c( "top", "bottom" ), ": ",
                                   lg.trait, "px solid ", couleur.trait, ";",
                                   collapse = " " ) )
    style.precolonne <- paste0( "text-align: right; ",
                                "font-weight: bold; ",
                                paste0( "padding-", c( 'left', 'right', 'top', 'bottom' ),
                                        ": ", marge, "px",
                                        collapse = "; " ), ";" )
    style.contenu <- paste0( paste0( "padding-", c( 'left', 'right', 'top', 'bottom' ),
                                     ": ", marge, "px",
                                     collapse = "; " ), ";" )

    chaine <- paste0( "<div style=\"overflow-x: auto; min-height: 30px;\">",
                      "<table noborder style=\"",
                      "border-collapse: collapse;",
                      " text-align: center;",
                      "\">" )

    for ( i in 1:n.echantillons ) {
        chaine <- paste0( chaine,
                          " <tr style=\"", style.ligne, "\">",
                          "<!E", i, ">" ) # Début d'échantillon, si retravail ensuite...

        if ( length( pre.texte ) > 0 ) {
            chaine <- paste0( chaine,
                              "<th style=\"", style.precolonne, "\">",
                              pre.texte[ i ],
                             "</th>" )
        }

        nombres <- unlist( lapply( x[[ i ]], afficher_nombre.moodle,
                                   n.chiffres = n.chiffres ) )
        chaine <- paste0( chaine,
                          paste0( "  <td style=\"", style.contenu, "\">",
                                  ## On précise les débuts de case, si on veut le retravailler ensuite !
                                  "<!E", i, "V", 1:length( nombres ), ">",
                                  nombres,
                                   "</td>", collapse = " " ),
                          "</tr>" )
    }
        
    ## On a fini...
    chaine <- paste0( chaine,
                      "</table></div>" )
    return( chaine )
}

######################################################################
# Affichage d'un polynôme
#
afficher_poly.moodle <- function( degre, variable = "x", a, ... )
{
    ## Contrôles initiaux
    degre <- as.integer( degre )    # Le degré est forcément entier...
    if ( degre < 0 ) {
        erreur( 500, "afficher_poly.moodle",
                "Degr\u00e9 absurde demand\u00e9 [", degre, "]" )
    }

    if ( length( a ) != degre + 1 ) {
        erreur( 501, "afficher_poly.moodle",
                "Polyn\u00f4me de degr\u00e9 ", degre,
                " : il faut ", degre + 1, " coefficients - ", length( a ), " fournis" )
    }

    ## Degré 0 : une constante
    if ( degre == 0 ) {
        if ( is.numeric( a ) ) return( afficher_nombre.moodle( a ) )
        if ( a %in% letters ) return( paste0( "<i>", a, "</i>" ) )
        return( as.character( a ) )
    }

    ## Mise en forme améliorée
    if ( variable %in% letters ) {
        variable <- paste0( "<i>", variable, "</i>" )
    }

    ## Les exposants
    expo <- seq( from = degre, to = 0, by = -1 )

    ## Les coefficients
    ##  1) on convertit les nombres en chaînes
    if ( is.numeric( a ) ) {
        signes <- ifelse( a > 0, "+", "-" )

        a <- unlist( lapply( abs( a ), afficher_nombre.moodle, ... ) )
    } else {
        signes <- ifelse( substr( a, 1, 1 ) == "-", "-", "+" )
        a <- gsub( "-", "", fixed = TRUE, a )
    }

    ## 2) si coefficient = 1 : on ne l'affiche pas (sauf si dernier, constante !)
    if ( any( a[ 1:degre ] == 1 ) ) {
        a[ which( a[ 1:degre ] == 1 ) ] <- ""
    }

    ## 3) on supprime les coefficients nuls
    if ( any( a == 0 ) ) {
        idx <- which( a == 0 )
        expo <- expo[ -idx ]
        signes <- signes[ -idx ]
        a <- a[ -idx ]
    }

    ## On construit le polynome
    idx.p <- which( expo > 1 )
    txt <- paste0( a[ idx.p ], "&thinsp;", variable, "<sup>", expo[ idx.p ], "</sup>" )
    if ( any( expo == 1 ) ) {
        a.x <- a[ which( expo == 1 ) ]
        if( nchar( a.x ) > 0 ) {
            txt <- c( txt, 
                      paste0( a.x, "&thinsp;", variable ) )
        } else {
            txt <- c( txt, variable )
        }
    }
    if ( expo[ length( expo ) ] == 0 ) {
        txt <- c( txt, a[ length( expo ) ] )
    }
    
    ## Les signes
    if ( signes[ 1 ] == "-" ) txt[ 1 ] <- paste0( "-", txt[ 1 ] )
    if ( length( txt ) > 1 ) {
        txt[ -1 ] <- paste0( "&nbsp;", signes[ -1 ], "&nbsp;", txt[ -1 ] )
    }

    ## On rassemble
    txt <- paste0( txt, collapse = "" )

    ## On remplace les - par de vrais signes moins
    txt <- gsub( "-", "&minus;", fixed = TRUE, txt )

    ## On protège contre la césure [?]
    txt <- paste0( "<span style=\"white-space: nowrap;\">",
                   txt,
                   "</span>" )
    
    ## On renvoie le texte contenant le polynôme créé
    txt
}
