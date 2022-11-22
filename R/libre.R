## ─────────────────────────────────────────────────────────────────
## Création de XML Moodle avec R
## © Emmanuel Curis — mars 2015
##
## Fonctions permettant la création de questions « libres »
## ─────────────────────────────────────────────────────────────────
## Historique
##   12 juillet 2016 : supprimé les accents directs restants
##                     enjolivé les commentaires
##
##   19 avril   2020 : types MULTICHOICE_?S ajoutés
##                       (ordre des réponses mélangé)
##
##   20 avril   2020 : souplesse et contrôles dans les réponses
##                       multiples aux questions cloze
##
##   11 mai     2020 : souplesse sur le nom Valeur/Valeurs
##
##   19 mai     2020 : types MULTIRESPONSE gérés
##                     meilleur contrôle de cohérence champ/réponses
##                     protection des } et ~ dans les réponses
##
##   21 mai     2020 : scindage du code pour meilleure lecture
##                     prise en compte des notes pour les MULTIRESPONSE
##
##   22 mai     2020 : dans MULTIRESPONSE, points <0 équilibrés par défaut
##                      pour les réponses fausses (sinon, tout cocher => OK)
##
##   28 mai     2020 : contrôle de validité, il ne faut pas de NaN ou Inf
##                     pas non plus de « vide »
##
##   30 mai     2020 : contrôle de validité, autant de notes que de questions
##
##   31 mai     2020 : ajout du temps conseillé pour répondre
##
##    8 juin    2020 : correction de la conversion vecteur -> liste de réponses
##
##   25 juin    2020 : déplacé les questions rédactionnelles ailleurs
##
##    1 janvier 2020 : prise en charge de l'identifiant numérique unique
##
##    3 juillet 2022 : adaptation pour utiliser le temps de catégorie
## ─────────────────────────────────────────────────────────────────

## ─────────────────────────────────────────────────────────────────
##
##                           Question « cloze »
##
## ─────────────────────────────────────────────────────────────────

## ———————————————————————————————————
##
## On normalise les noms des éléments de réponse
##   (variantes singulier/pluriel, synonymes etc.)
##

normaliser.nom <- function( reponses, nom, variantes.nom )
{
    for ( vn in variantes.nom ) {
        if ( hasName( reponses, vn ) ) {
            names( reponses )[ which( names( reponses ) == vn ) ] <- nom
        }
    }
    
    reponses
}

## ———————————————————————————————————
##
## On protège les textes des réponses
##   (caractères interdits...)
##

nettoyer.reponses <- function( reponses, strict = FALSE )
{
    ## Caractère } interdit : indique la fin du champ de question...
    reponses <- gsub( "}", "\\}", fixed = TRUE, reponses )
    
    ## Caractère ~ interdit : sépare les différentes réponses possibles...
    reponses <- gsub( "~", "\\~", fixed = TRUE, reponses )

    ## Tout code HTML est-il autorisé ?
    if ( strict ) {
        ## Quelques entités HTML à convertir en UTF-8

        ## On supprime d'éventuelles balises HTML
    }

    ## On renvoie la version nettoyée
    reponses
}

## ─────────────────────────────────────────────────────────────────
## 
## Les questions avec plusieurs réponses, complètement libres
##  (attention, fonction non-exportée)
##
generer_question <- function( note = NA, type, reponses, commentaires ) {
    ## Préparation des réponses : le plus long

    ## Cas un : on a fourni un simple vecteur de reponses
    ##   => ce sont toutes des réponses correctes, 100 % des points
    if ( !is.list( reponses ) ) {
        reponses <- list( "Reponses" = reponses,
                          "Notes"    = rep( 100, length( reponses ) ) )
    }

    ## Correction d'erreurs de nom fréquentes
    reponses <- normaliser.nom( reponses, 'Reponses',
                                c( 'Reponse', 'R\u00e9ponse', 'R\u00e9ponses' ) )

    reponses <- normaliser.nom( reponses, 'Notes',
                                c( 'Note' ) )

    reponses <- normaliser.nom( reponses, 'Correct',
                                c( 'Corrects', 'Correcte', 'Correctes' ) )

    ## On prépare les champs « réponses »
    if ( type == "NUMERICAL" ) {
        reponse <- gr.numerique( reponses = reponses,
                                 commentaires = commentaires )
    } else if ( type %in% c( "SHORTANSWER"    , "SHORTANSWER_C" ) ) {
        reponse <- gr.qroc( reponses = reponses,
                            commentaires = commentaires )
    } else if ( type %in% c( "MULTICHOICE"    , "MULTICHOICE_V"   , "MULTICHOICE_H" ,
                             "MULTICHOICE_S"  , "MULTICHOICE_VS"  , "MULTICHOICE_HS" ) ) {
        reponse <- gr.qcu( type = type, 
                           reponses = reponses,
                           commentaires = commentaires )
    } else if ( type %in% c( "MULTIRESPONSE"  , "MULTIRESPONSE_H" ,
                             "MULTIRESPONSE_S", "MULTIRESPONSE_HS" ) ) {
        reponse <- gr.qcm( type = type, 
                           reponses = reponses,
                           commentaires = commentaires )
    } else {
        stop( "[CLOZE] Type de question inconnu [", type, "]" )
    }

    ## À partir des réponses, on construit la question proprement dite
    question <- paste0( "{", 
                        if ( is.finite( note ) ) note,
                        ":", type, ":",
                        reponse,
                        "}" )
#       print( question )
    question
}

## ———————————————————————————————————
##
## Construction des questions de type « numérique » 
##
## reponses = une liste avec trois éléments :
##   Valeurs   : les réponses autorisées ; numériques forcément
##   Tolerance : les tolérances associées (NA = 0, réponse exacte)
##   Notes     : les notes associées (en pourcentages du point)
gr.numerique <- function( reponses, commentaires )
{
    if ( is.list( reponses ) ) {
        ## On normalise le nom des réponses : « Valeurs »
        reponses <- normaliser.nom( reponses, 'Valeurs',
                                    c( 'Valeur', 'Reponses' ) )

        reponses <- normaliser.nom( reponses, 'Tolerance',
                                    c( 'Tol\u00e9rances', 'Tolerances', 'Tol\u00e9rance' ) )

        ## Les valeurs attendues pour les réponses
        valeurs <- reponses$Valeurs
        if ( length( valeurs ) < 1 ) {
            stop( "Aucune r\u00e9ponse propos\u00e9 :",
                  " l'import dans Moodle \u00e9choura." )
        }
        if ( any( is.na( valeurs ) ) ) {
            stop( "Valeur NA : l'import dans Moodle \u00e9choura." )
        }
        if ( any( is.nan( valeurs ) ) ) {
            stop( "Valeur NaN : l'import dans Moodle \u00e9choura." )
        }
        if ( any( !is.finite( valeurs ) ) ) {
            stop( "Valeur Inf : l'import dans Moodle \u00e9choura." )
        }

        ## On construit les tolérances
        tolerance <- reponses$Tolerance
        if ( is.null( tolerance ) ) {
            tolerance <- rep( 0, length( valeurs ) )
        }
        if ( length( tolerance ) == 1 ) {
            tolerance <- rep( tolerance, length( valeurs ) )
        }
        if ( any( is.na( tolerance ) ) ) {
            warning( "Tol\u00e9rances manquantes, impos\u00e9es \u00e0 0." )
            tolerance[ which( is.na( tolerance ) ) ] <- 0
        }
        if ( length( tolerance ) != length( valeurs ) ) {
            stop( "Il y a ", length( valeurs ), " r\u00e9ponses",
                  " mais ", length( tolerance ), " tol\u00e9rances." )
        }

        ## On récupère les notes
        notes <- reponses$Notes
        if ( is.null( notes ) ) {
            ## Toutes les réponses sont correctes
            notes <- rep( "100", length( valeurs ) )
        }
        if ( length( notes ) == 1 ) {
            ## Même note pour tout le monde...
            notes <- rep( notes, length( valeurs ) )
        }
        if ( length( notes ) != length( valeurs ) ) {
            stop( "Il y a ", length( valeurs ), " r\u00e9ponse", if ( length( valeurs ) > 1 ) "s",
                  " mais ", length( notes ), " note", if ( length( notes ) > 1 ) "s." )
        }

        ## On construit le texte décrivant les réponses
        reponse <- paste0( "%", notes, "%", valeurs, ":", tolerance, "#", commentaires )
        reponse <- paste0( reponse, collapse = "~" )

        ## On le renvoie
        return( reponse )
    }

    ## Si l'on est là : c'est que l'on n'a qu'un vecteur de réponses
    ##   => pas de tolérance [réponse exacte attendue]
    ##   => toutes sont correctes à 100 %
    
    if ( length( reponses ) > 1 ) {
        ## Plusieurs variantes de réponses correctes
        reponse <- paste0( "=", reponses, "#", commentaires )
        reponse <- paste0( reponse, collapse = "~" )
    } else {
        reponse <- paste0( "=", reponses )
        if ( nchar( commentaires[ 1 ] ) > 0 ) reponse <- paste0( reponses, "#", commentaires )
    }

    reponse
}


## ———————————————————————————————————
##
## Construction des questions de type « QROC »
##  Types : SHORTANSWER   = insensible à la casse
##          SHORTANSWER_C = sensible à la casse
##  Champ de texte à remplir, réponse confrontée à celles enregistrées
##
## reponses = une liste avec deux éléments :
##   Reponse : les réponses autorisées ; numériques forcément
##   Notes   : les notes associées (en pourcentages du point)
gr.qroc <- function( reponses, commentaires )
{
    ## On normalise le nom des réponses : « Textes »
    reponses <- normaliser.nom( reponses, 'Textes',
                                c( 'Texte', 'Reponses' ) )
    
    ## On récupère les textes des réponses
    textes <- reponses$Textes

    ## On protège quelques caractères spéciaux
    textes <- nettoyer.reponses( textes, strict = TRUE )

    ## On récupère les notes
    notes <- reponses$Notes
    if( is.null( notes ) ) {
        if ( hasName( reponses, "Correct" ) ) {
            notes <- ifelse( reponses$Correct, 100, 0 )
        } else {
            notes <- rep( 100, length( textes ) )
        }
    }
    if ( length( notes ) == 1 ) {
        notes <- rep( notes, length( textes ) )
    }
    if ( length( notes ) != length( textes ) ) {
        stop( "Il y a ", length( textes ), " r\u00e9ponses",
              " mais ", length( notes ), " notes." )
    }

    ## Construction proprement dite du champ
    txt.reponse <- paste0( "%", notes, "%", textes,
                           ifelse( is.na( commentaires ), "", paste0( "#", commentaires ) ) )
    txt.reponse <- paste0( txt.reponse, collapse = "~" )

    ## On renvoie le champ « réponses »
    txt.reponse
}

## ———————————————————————————————————
##
## Construction des questions de type « choix multiple, une seule réponse »
##  Types : MULTICHOICE   = menu déroulant
##          MULTICHOICE_S = menu déroulant (ordre aléatoire)
##          MULTICHOICE_V , MULTICHOICE_H  = boutons radio
##          MULTICHOICE_VS, MULTICHOICE_HS = boutons radio (ordre aléatoire)
##
## reponses = une liste avec deux éléments :
##   Textes  : les réponses autorisées ; numériques forcément
##   Correct : si les réponses sont correctes ou non
gr.qcu <- function( type, reponses, commentaires )
{
    ## On normalise le nom des réponses : « Textes »
    reponses <- normaliser.nom( reponses, 'Textes',
                                c( 'Texte', 'Reponses' ) )
    
    if ( is.null( names( reponses ) ) ) {
        names( reponses ) <- c( "Textes", "Correct" )
    }
    
    ## On récupère les réponses
    txt.reponses  <- reponses$Textes
    if ( length( txt.reponses ) < 2 ) {
        stop( "[CLOZE] QCU : une seule r\u00e9ponse propos\u00e9,",
              " il en faut au moins 2" )
    }
    
    ## On protège les textes des réponses
    ##   (Variante « menu » : entités et balises HTML non reconnues...)
    txt.reponses <- nettoyer.reponses( txt.reponses, 
                                       strict = ( type %in% c( 'MULTICHOICE', 'MULTICHOICE_S' ) ) )

    ## Si l'on n'a pas précisé quelle est la bonne réponse
    ##  on suppose que c'est la première
    if ( FALSE == hasName( reponses, 'Correct' ) ) {
        warning( "[CLOZE] QCU : \u00e9l\u00e9lement Correct absent des r\u00e9ponses" )
        reponses$Correct <- c( TRUE, rep( FALSE, length( reponses ) - 1 ) )
    }

    ## On récupère les réponses correctes
    txt.correctes <- reponses$Correct
    
    n.correctes <- length( which( txt.correctes == TRUE ) )
    if ( n.correctes == 0 ) {
        stop( "Aucune r\u00e9ponse correcte parmi celles indiqu\u00e9es !",
              "(R\u00e9ponses : ", paste0( "[", txt.reponses, "]", collapse = " // " ), ")" )
    }
    if ( ( n.correctes > 1 ) &
         !( type %in% c( 'MULTICHOICE', 'MULTICHOICE_S' ) ) ) {
        stop( "Moodle n'autorise pas plusieurs r\u00e9ponses correctes",
              " dans une question multiple cloze de type",
              " MULTICHOICE avec boutons radios (_H, _V, _HS, _VS)",
              " (l'import \ue00e9choue)\n",
              " [type = ", type , ", reponses = {", 
              paste0( reponses, collapse = ", " ), "}]" )
    }

    correct <- ifelse( txt.correctes, "=", "" )

    ## On construit le champ des réponses
    reponse <- paste0( correct, txt.reponses,
                       ifelse( is.na( commentaires ), "", paste0( "#", commentaires ) ) )
    reponse <- paste0( reponse, collapse = "~" )
    reponse
}

## ———————————————————————————————————
##
## Construction des questions de type « choix multiple » 
##  Types : MULTIRESPONSE
##
## reponses = une liste avec au moins deux éléments :
##   Textes : les réponses autorisées ; numériques forcément
##   Notes  : les notes associées (en pourcentages du point)
gr.qcm <- function( type, reponses, commentaires )
{
    ## On normalise le nom des réponses : « Textes »
    reponses <- normaliser.nom( reponses, 'Textes',
                                c( 'Texte', 'Reponses' ) )
    
    if ( is.null( names( reponses ) ) ) {
        names( reponses ) <- c( "Textes", "Correct" )
    }
    
    
    ## On récupère les réponses
    textes  <- reponses$Textes
    n.reponses  <- length( textes )
    if ( n.reponses < 2 ) {
        stop( "[CLOZE] QCM : une seule r\u00e9ponse propos\u00e9,",
              " il en faut au moins 2" )
    }
    
    ## On protège quelques caractères spéciaux
    textes <- nettoyer.reponses( textes, strict = FALSE )

    ## On récupère les notes
    notes <- reponses$Notes
    if( is.null( notes ) ) {
        if ( hasName( reponses, "Correct" ) ) {
            correct <- reponses$Correct
            if ( length( correct ) != n.reponses ) {
                stop( "Il y a ", n.reponses, " r\u00e9ponses",
                      " mais ", length( correct ), " indications de correct/incorrect." )
            }

            n.correctes <- length( which( correct ) )
            if ( n.correctes == 0 ) {
                stop( "Aucune r\u00e9ponse correcte parmi celles indiqu\u00e9es !",
                      "(R\u00e9ponses : ",
                      paste0( "[", textes, "]", collapse = " // " ),
                      ")" )
            }

            ## Même note pour toutes les correctes
            ## Même note négative pour toutes les incorrectes
            ## Attention, moodle le veux en % entier
            ##  (3 réponses -> 33 % et pas 33.33333 % ici ...)
            note.ok <- 100 / n.correctes
            if ( n.correctes < n.reponses ) {
                note.nok <- - 100 / ( n.reponses - n.correctes )
            } else {
                note.nok <- 0
            }
            notes <- ifelse( reponses$Correct, round( note.ok, 0 ), round( note.nok, 0 ) )
        } else {
            stop( "Ni note, ni indication de bonne r\u00e9ponse.",
                  " Impossible de construire la question." )
        }
    }
    ## if ( length( notes ) == 1 ) {
    ##     notes <- rep( notes, length( reponses ) )
    ## }
    if ( length( notes ) != n.reponses ) {
        stop( "Il y a ", n.reponses, " r\u00e9ponses",
              " mais ", length( notes ), " notes." )
    }

    ## On construit les champs de réponse
    txt.reponse <- paste0( "%", notes, "%", textes,
                           ifelse( is.na( commentaires ), "", paste0( "#", commentaires ) ) )
    txt.reponse <- paste0( txt.reponse, collapse = "~" )

    ## On renvoie le champ « réponses »
    txt.reponse
}

## ─────────────────────────────────────────────────────────────────
## 
## Question libre « cloze » — visible de l'utilisateur
## 
question_libre.moodle <- function( texte.intro, textes.avant, texte.final,
                                   reponses, notes = NULL, types = NULL, commentaires = NULL,
                                   titre = "Question libre",
                                   commentaire.global = NA, penalite = NA, note.question = NA,
                                   idnum = NA, temps,
                                   fichier.xml = get( "fichier.xml", envir = SARP.Moodle.env ) ) {
    ## Combien de questions au total ?
    if( length( textes.avant ) != length( reponses ) ) {
        print( textes.avant )
        stop( "Le nombre de r\u00e9ponses [", length( reponses ), "]",
              " et le nombre de champs [", length( textes.avant ), "]",
              " discordent..." )
    }
    n.questions <- length( reponses )

    ## On prépare les éléments
    if ( length( notes ) == 0 ) {
        notes <- rep( NA, n.questions )
    }
    if ( length( types ) == 0 ) {
        types <- rep( "NUMERICAL", n.questions )
    }
    if ( length( types ) != n.questions ) {
        stop( "Il faut autant de types que de champs r\u00e9ponses..." )
    }
    if ( length( notes ) != n.questions ) {
        stop( "Il faut autant de notes que de champs r\u00e9ponses..." )
    }
  
    if ( length( commentaires ) == 0 ) {
        commentaires <- as.list( rep( "", n.questions ) )
    }

    champs <- rep( "", n.questions )
    for(  i in 1:n.questions ) {
        champs[ i ] <- generer_question( note = notes[ i ], 
                                         type = types[ i ], 
                                         reponses = reponses[[ i ]],
                                         commentaires = commentaires[[ i ]] )
    }
    ## cat( "\nChamps : \n" )
    ## print( champs )
  
    texte <- paste0( texte.intro,
                     paste0( paste( textes.avant, champs ), collapse = "" ),
                     texte.final )

    ##    print( texte )
    if( length( texte ) != 1 ) {
        print( texte )
        stop( "Erreur dans la cr\u00e9ation de la question cloze" )
    }

    if ( missing( commentaire.global ) ) commentaire.global <- NA

    
    ## On ajoute l'indication de temps éventuelle
    texte <- paste0( texte, 
                     temps_necessaire.moodle( temps ) )

    ## On crée la question
    question.moodle( fichier.xml = fichier.xml, type = "cloze",
                     titre = titre, texte = texte, reponses = NULL,
                     penalite = penalite, note = note.question,
                     commentaire.global = commentaire.global, idnum = idnum )
}
