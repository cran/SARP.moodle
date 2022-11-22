## ─────────────────────────────────────────────────────────────────
## Création de XML Moodle avec R
## © Emmanuel Curis — mars 2015
##
## Fonctions permettant la création de questions numériques « simples »
## ─────────────────────────────────────────────────────────────────
## Historique
##   24 octobre 2016 : mise en page adaptée
##
##    2 mai     2020 : paramétrage possible de la tolérance
##                     tolérance automatique de ±1,1 sur la décimale demandée
##                     préparation pour les unités
##
##   31 mai     2020 : ajout du temps conseillé pour répondre
##
##    1 janvier 2021 : prise en compte de l'identifiant numérique
##
##    3 juillet 2022 : adaptation pour utiliser le temps de catégorie
## ─────────────────────────────────────────────────────────────────

## ─────────────────────────────────────────────────────────────────
## Question à réponse numérique
##
## texte           : l'énoncé de la question
## bonne.reponse   : la ou les réponses autorisées [vecteur numérique]
## notes           : la note associée à chacune des réponses
## n.decimales     : combien de chiffres *après la virgule* sont attendus
## n.significatifs : combien de chiffres *significatifs* sont attendus
## titre           : le titre de la question
## commentaire.global : le commentaire à faire après réponse de l'étudiant
## tolerance.type     : le type de tolérance pour la réponse [2 : ± tolérance]
## tolerance          : la valeur de la tolérance
##                       "auto" : k décimales -> ± 1,1 × 10^-k
## unites             : les unités autorisées
##                       vecteur numérique, les noms sont les symboles
## unite.avant        : si TRUE, l'unité doit être placée avant
## unite.penalite     : la pénalité en cas d'erreur d'unité
## unite.visible      : si TRUE, l'unité est à choisir dans un menu déroulant
numerique.moodle <- function( texte, bonne.reponse, notes = 100,
                              n.decimales = get( "nombre.chiffres", envir = SARP.Moodle.env ),
                              n.significatifs = NA,
                              titre = "Question num&eacute;rique...",
                              commentaire.global = NA, penalite = NA, note.question = NA, idnum = NA,
                              tolerance.type = 2, tolerance = "auto",
                              unites = NULL, unite.avant = FALSE, unite.penalite = 0.1, unite.visible = FALSE,
                              commentaires = NULL, couleur.consigne = "Orange",
                              temps,
                              fichier.xml = get( "fichier.xml", envir = SARP.Moodle.env ) )
{
    ## Les contrôles initiaux
    ##   -> la réponse doit être numérique...
    if ( FALSE == is.numeric( bonne.reponse ) ) {
        stop( "Pour une question num\u00e9rique, la bonne r\u00e9ponse doit \u00eatre un nombre" )
    }

    ##   -> il faut autant de notes que de réponses
    if ( length( notes ) != length( bonne.reponse ) ) {
        stop( "Il faut autant de notes que de r\u00e9ponses" )
    }
    
    ## Si la réponse est un entier, pas de notion de décimales, arrondi etc.
    if ( is.integer( bonne.reponse ) ) {
        tolerance <- 0 ; tolerance.type <- 2
        n.decimales <- NA
        n.significatifs <- NA
    }

    ##   -> on ne doit pas imposer à la fois le nombre de décimales
    ##        et le nombre de chiffres significatifs
    if ( all( is.finite( n.decimales ), is.finite( n.significatifs ) ) ) {
        stop( "On peut imposer le nombre de chiffres apr\u00e8 la virgule",
              " ou le nombre de chiffre significatifs, mais pas les deux !" )
    }

    reponse <- bonne.reponse

    
    ## On prépare les réponses
    texte.consigne <- ""
    n.decimales <- as.integer( n.decimales )
    if ( is.finite( n.decimales ) ) {
        if ( any( n.decimales < 0 ) ) {
            stop( "Un nombre de d\u00e9cimales doit \u00eatre positif !" )
        }
        
        bonne.reponse <- round( bonne.reponse, n.decimales )

        texte.consigne <- "Vous donnerez la r&eacute;ponse"
        if ( n.decimales > 0 ) {
            texte.consigne <- paste0( texte.consigne,
                                      " avec ", n.decimales,
                                      "&nbsp;chiffre", if ( n.decimales > 1 ) "s",
                                      " apr&egrave;s la virgule" )
        } else {
            texte.consigne <- paste0( texte.consigne,
                                      " arrondie &agrave; l'entier le plus proche" )
        }

        if ( tolerance == "auto" ) {
            tolerance <- 1.1 * 10^(-n.decimales) # dernier chiffre : on tolère une erreur de ±1,1
        }
    }

    n.significatifs <- as.integer( n.significatifs )
    if ( is.finite( n.significatifs ) ) {
        if ( any( n.significatifs <= 0 ) ) {
            stop( "Un nombre de chiffres significatifs doit \u00eatre strictement positif !" )
        }
        
        bonne.reponse <- round( bonne.reponse, n.significatifs )

        texte.consigne <- paste0( "Vous donnerez la r&eacute;ponse",
                                  " avec ", n.significatifs,
                                  "&nbsp;chiffre", if ( n.significatifs > 1 ) "s",
                                  " significatif", if ( n.significatifs > 1 ) "s" )

        if ( tolerance == "auto" ) {
            tolerance <- 0
        }
    }

    ## Pour le cas d'aucun arrondi
    if ( tolerance == "auto" ) {
        tolerance <- 0
    }

    ## On construit le texte *avec la consigne*
    if ( nchar( texte.consigne ) > 0 ) {
        texte <- paste0( texte,
                         "<br />",
                         "<i style=\"color: ", couleur.consigne, ";\">",
                         texte.consigne, ".</i>" )
    }

    ## On ajoute l'indication de temps éventuelle
    texte <- paste0( texte, 
                     temps_necessaire.moodle( temps ) )
    
    if ( missing( commentaire.global ) ) commentaire.global <- NA

    ## ————————————————————————————————————
    ## On peut commencer la question
    debut_question.moodle( type = "numerical",
                           titre = titre, texte = texte,
                           penalite = penalite, note = note.question,
                           commentaire.global = commentaire.global,
                           idnum = idnum,
                           fichier.xml = fichier.xml )
    
    reponse <- as.character( bonne.reponse )
    attr( reponse, "fractions" ) <- 100

    ## ————————————————————————————————————
    ## On crée les réponses
    if ( is.null( commentaires ) ) {
        commentaires <- rep( NA, length( bonne.reponse ) )
    }
    if ( length( tolerance ) == 1 ) tolerance <- rep( tolerance, length( bonne.reponse ) )
    tolerance <- abs( tolerance )
    for ( i in 1:length( bonne.reponse ) ) {
        cat( file = fichier.xml, sep = "",
             "  <answer fraction=\"", notes[ i ], "\"",
             " format=\"moodle_auto_format\">\n",
             "    <text>", reponse[ i ], "</text>\n" )

        ## Le commentaire, s'il existe
        if ( all( !is.na( commentaires[ i ] ), 
                  nchar( commentaires[ i ] ) > 0 ) ) {
            cat( file = fichier.xml, sep = "",
                 "    <feedback format=\"html\">" )
            coder.texte( commentaires[ i ], fichier.xml = fichier.xml )
            cat( file = fichier.xml, sep = "",
                 "</feedback>\n" )
        }

        ## La tolérance si elle existe
        if ( is.finite( tolerance[ i ] ) ) {
            cat( file = fichier.xml, sep = "",
                 "    <tolerance>", tolerance[ i ],
                 "</tolerance>\n" )
        }

        cat( file = fichier.xml, sep = "",
             "  </answer>\n" )
    }

    ## ————————————————————————————————————
    ##  Gestion des unités
    if ( length( unites ) > 0 ) {
        cat( file = fichier.xml, sep = "",
             "  <units>\n" )
        for ( i in 1:length( unites ) ) {
            cat( file = fichier.xml, sep = "",
                 "    <unit>\n",
                 "      <multiplier>", unites[ i ], "</multiplier>\n",
                 "      <unit_name>", names( unites )[ i ], "</unit_name>\n",
                 "    </unit>\n" )
        }
        cat( file = fichier.xml, sep = "",
             "  </units>\n" )

        ## On ne considère que le mode de notation
        ##  où l'unité est prise en compte dans la note
        cat( file = fichier.xml, sep = "",
             "  <unitgradingtype>3</unitgradingtype>\n" )
        
        ## Pénalité d'erreur d'unité
        cat( file = fichier.xml, sep = "",
             "  <unitpenalty>", unite.penalite, "</unitpenalty>\n" )

        ## Comment est choisie l'unité ?
        cat( file = fichier.xml, sep = "",
             "  <showunits>", as.integer( unite.visible ), "</showunits>\n" )             
        
        ## Unité à gauche ? (par défaut, à droite)
        cat( file = fichier.xml, sep = "",
             "  <unitsleft>", as.integer( unite.avant ), "</unitsleft>\n" )        
    }
    
    
    ## On a fini la question
    fin_question.moodle( fichier.xml = fichier.xml )
}
