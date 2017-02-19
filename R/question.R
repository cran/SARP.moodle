## ─────────────────────────────────────────────────────────────────
## Création de XML Moodle avec R
## © Emmanuel Curis — mars 2015
##
## Fonctions permettant la création de questions génériques [dont « cloze »]
## ─────────────────────────────────────────────────────────────────
## Historique
##   22 juillet 2016 : mise en page adaptée
##                     avertissement en cas de notes automatiques
##
##   20 octobre 2016 : commentaire global (après réponse) géré
## ─────────────────────────────────────────────────────────────────

question.moodle <- function( type = "cloze",
                             titre = "Question...", texte, reponses = NULL,
                             penalite = NA, note = NA, commentaire.global = NA,
                             autres.codes = NULL,
                             fichier.xml = get( "fichier.xml", envir = SARP.Moodle.env ) ) {
    ## On démarre la question
    cat( file = fichier.xml, sep = "",
         "\n<question type=\"", type, "\">\n" )

    ## A-t-on indiqué un titre pour la question ?
    if ( length( titre ) > 0 ) {
        cat( file = fichier.xml, sep = "",
             " <name format=\"html\">\n",
             "  <text><![CDATA[", titre, "]]></text>\n",
             " </name>\n" )
    }

    ## OBLIGATOIRE : le texte de la question
    cat( file = fichier.xml, sep = "",
         " <questiontext format=\"html\">\n",
         "  <text><![CDATA[", texte, "]]></text>\n",
         " </questiontext>\n" )

    ## On indique le commentaire général fait à la question
    if ( all( is.na( commentaire.global ) == FALSE, nchar( commentaire.global ) > 0 ) ) {
        cat( file = fichier.xml, sep = "",
             " <generalfeedback>\n",
             "  <text><![CDATA[",
             commentaire.global,
             "]]></text>\n",
             " </generalfeedback>\n" )
    }

    ## On indique une note par défaut
    if ( is.finite( note ) ) {
        cat( file = fichier.xml, sep = "",
             " <defaultgrade>", note, "</defaultgrade>\n" )
    }
    
    ## On indique une pénalité
    if ( is.finite( penalite ) ) {
        cat( file = fichier.xml, sep = "",
             " <penalty>", penalite, "</penalty>\n" )
    }

    ## On a indiqué des réponses...
    n.reponses <- length( reponses )
    if ( n.reponses > 0 ) {
        ## A-t-on précisé la fraction des points pour chaque réponse ?
        fractions <- attr( reponses, "fractions" )
        n.fractions <- length( fractions )
        
        if ( n.fractions != n.reponses ) {
            ## Longueurs discordantes...
            if ( 0 == n.fractions ) {
                ## Aucune fraction indiquée : la première réponse est à 100 %, les autres à 0
                warning( "Aucune note n'est indiqu\u00e9e pour les r\u00e9ponses.",
                         " La premi\u00e8re r\u00e9ponse est, arbitrairement,",
                         " suppos\u00e9e la seule correcte.",
                         " Toutes les autres ont une note nulle." )
                fractions <- c( 100, rep( 0, n.reponses - 1 ) )
                n.fractions <- n.reponses
            } else {
                ## Pas d'automatisation, erreur...
                stop( "Discordance entre nombre de r\u00e9ponses et nombre de fractions..." )
            }
        }

        ## A-t-on précisé des commentaires pour chaque réponse ?
        commentaires <- attr( reponses, "commentaire" )
        n.commentaires <- length( commentaires )
        if ( n.commentaires != n.reponses ) {
            if ( 0 == n.commentaires ) {
                commentaires <- rep( NA, n.reponses )
            } else {
                ## Pas d'automatisation, erreur...
                stop( "Discordance entre nombre de r\u00e9ponses et nombre de commentaires..." )
            }
        }

        ## On ajoute chaque réponse (bonne, partielle ou fausse) au fichier
        for ( i in 1:n.reponses ) {
            cat( file = fichier.xml, sep = "", "\n",
                 " <answer fraction=\"", fractions[ i ], "\">\n",
                 "  <text>", reponses[ i ], "</text>\n" )
            if ( all( ( FALSE == is.na( commentaires[ i ] ) ),
                      nchar( commentaires[ i ] ) > 0 ) ) {
              cat( file = fichier.xml, sep = "", "\n",
                   "  <feedback>\n",
                   "   <text>\n",
                   "    <![CDATA[", commentaires[ i ], "]]>\n",
                   "   </text>\n",
                   "  </feedback>\n" )
            }
            cat( file = fichier.xml, sep = "\n",
                 " </answer>" )
        }
    }

    if ( length( autres.codes ) > 0 ) {
        nom.codes <- names( autres.codes )
        if ( is.null( nom.codes ) ) {
            stop( "Vous devez pr\u00e9ciser le nom des codes comme nom de la liste de leur valeur..." )
        }
        for ( i in 1:length( autres.codes ) ) {
            cat( file = fichier.xml, sep = "",
                 " <", nom.codes[ i ], ">", autres.codes[ i ], "</", nom.codes[ i ], ">\n" )
        }
    }

    ## Question finie...
    cat( file = fichier.xml, sep = "\n",
         "</question>" )
}
                             
