## ─────────────────────────────────────────────────────────────────
## Création de XML Moodle avec R
## © Emmanuel Curis — mars 2015
##
## Fonctions permettant la création d'un glossaire 
## ─────────────────────────────────────────────────────────────────
## Historique
##   31   juillet  2016 : création du fichier
##    1er novembre 2016 : les balises doivent être en majuscule…
##                        création depuis un fichier CSV
## ─────────────────────────────────────────────────────────────────

creer_glossaire.moodle <- function( nom.fichier, nom.glossaire, texte.intro,
                                    doublons = TRUE ) {
    ## On crée un fichier XML Moodle pour recevoir un glossaire
    fichier.xml <- debuter_xml.moodle( fichier.xml = nom.fichier, glossaire = TRUE )

    ## Les éléments de l'introduction
    ## Le nom du glossaire
    cat( file = fichier.xml, sep = "",
         "  <NAME><![CDATA[", nom.glossaire, "]]></NAME>\n" )

    ## Le texte de présentation (« intro »)
    cat( file = fichier.xml, sep = "",
         "  <INTRO><![CDATA[", texte.intro, "]]></INTRO>\n" )

    ## Quelques options diverses
    ##  Format de l'intro : ?
    cat( file = fichier.xml, sep = "",
         "  <INTROFORMAT>0</INTROFORMAT>\n" )

    ##  Entrées en double autorisées [terme ou définition ?]
    cat( file = fichier.xml, sep = "",
         "  <ALLOWDUPLICATEDENTRIES>", as.integer( doublons ), "</ALLOWDUPLICATEDENTRIES>\n" )

    cat( file = fichier.xml, sep = "",
         "  <DISPLAYFORMAT>entrylist</DISPLAYFORMAT>\n",
         "  <SHOWSPECIAL>0</SHOWSPECIAL>\n",
         "  <SHOWALPHABET>1</SHOWALPHABET>\n",
         "  <SHOWALL>1</SHOWALL>\n",
         "  <ALLOWCOMMENTS>0</ALLOWCOMMENTS>\n",
         "  <USEDYNALINK>1</USEDYNALINK>\n",
         "  <DEFAULTAPPROVAL>1</DEFAULTAPPROVAL>\n",
         "  <GLOBALGLOSSARY>0</GLOBALGLOSSARY>\n",
         "  <ENTBYPAGE>50</ENTBYPAGE>\n" )
    
    ## ON prépare pour pouvoir ajouter les entrées 
    cat( file = fichier.xml, sep = "",
         "  <ENTRIES>\n" )

    ## On renvoie le fichier XML…
    fichier.xml
}

entree_glossaire.moodle <- function( terme, definition,
                                     fichier.xml = get( "fichier.xml", envir = SARP.Moodle.env ) ) {
    cat( file = fichier.xml, sep = "",
         "   <ENTRY>\n" )
    
    ## Le terme à définir
    cat( file = fichier.xml, sep = "",
         "    <CONCEPT><![CDATA[", terme, "]]></CONCEPT>\n" )

    ## La définition
    cat( file = fichier.xml, sep = "",
         "    <DEFINITION><![CDATA[", definition, "]]></DEFINITION>\n" )

    ## Options diverses
    ##   (vues dans le fichier d'exemple ; rôle ?)
    cat( file = fichier.xml, sep = "",
         "    <FORMAT>1</FORMAT>\n",
         "    <USEDYNALINK>1</USEDYNALINK>\n",
         "    <CASESENSITIVE>1</CASESENSITIVE>\n",
         "    <FULLMATCH>1</FULLMATCH>\n",
         "    <TEACHERENTRY>1</TEACHERENTRY>\n"
        )
    
    cat( file = fichier.xml, sep = "",
         "   </ENTRY>\n" )
}

csv_glossaire.moodle <- function( fichier.csv,
                                  colonne.terme = 'Mot',
                                  colonne.definition = 'D\u00e9finition',
                                  fichier.xml = if ( TRUE == nv.fichier ) gsub( "\\.[Cc][Ss][Vv]$", ".xml", fichier.csv )
                                                else get( "fichier.xml", envir = SARP.Moodle.env ),
                                  nv.fichier = TRUE,
                                  embellir = TRUE, deja.HTML = FALSE,
                                  sep = ";", header = TRUE, quote = '"',
                                  ... ) {
    ## Vérification : fichier XML
    ##   [On ne peut pas utiliser « missing » pour les colonnes avec une valeur par défaut :
    ##                                        renvoie « TRUE » si non-spécifié sur la ligne de commande...]
    if ( all( FALSE == is.character( fichier.xml ),
              FALSE == ( "file" %in% ( fichier.xml ) ) ) ) {
        stop( "Le fichier XML doit \u00eatre un nom de fichier \u00e0 cr\u00e9er",
              " ou un fichier d\u00e9j\u00e0 ouvert par creer_glossaire.moodle" )
    }

    ## On harmonise les colonnes absentes : NA
    if ( any( is.na( colonne.terme ), is.null( colonne.terme ),
              ( is.integer( colonne.terme ) && ( colonne.terme < 1 ) ),
              ( is.character( colonne.terme ) && ( nchar( colonne.terme ) < 1 ) ) ) ) {
        stop( "Vous devez indiquer quelle colonne contient les mots \u00e0 d\u00e9finir" )
    }

    if ( any( is.na( colonne.definition ), is.null( colonne.definition ),
              ( is.integer( colonne.definition ) && ( colonne.definition < 1 ) ),
              ( is.character( colonne.definition ) && ( nchar( colonne.definition ) < 1 ) ) ) ) {
        stop( "Vous devez indiquer quelle colonne contient les d\u00e9finitions" )
    }

    ## Si demandé : on crée le fichier XML
    if ( TRUE == nv.fichier ) {
        if ( any( is.na( fichier.xml ), length( fichier.xml ) != 1,
                  FALSE == is.character( fichier.xml ), nchar( fichier.xml ) < 1 ) ) {
            stop( "Il faut indiquer un et un seul nom de fichier XML pour la sortie..." )
        }

        fichier.xml <- creer_glossaire.moodle( nom.fichier = fichier.xml,
                                               nom.glossaire = fichier.csv,
                                               texte.intro = "Conversion de CSV en glossaire XML" )
    }
    
    ## On charge les fichiers CSV indiqués à tour de rôle...
    l <- as.list( rep( "", length( fichier.csv ) ) )
    names( l ) <- fichier.csv
    for ( f in fichier.csv ) {
        cat( "*** Traitement du fichier \u00ab ", f, " \u00bb ***\n" )

        ## On fait le traitement réel...
        d <- csv_vers_glossaire( fichier.csv = f,
                                 colonne.terme   = colonne.terme,
                                 colonne.def = colonne.definition,
                                 fichier.xml     = fichier.xml, 
                                 embellir        = embellir   ,
                                 deja.HTML       = deja.HTML  ,
                                 sep = sep, header = header, quote = quote,
                                 ... )
        l[[ f ]] <- d
    }

    ## On a terminé
    if ( TRUE == nv.fichier ) {
        finir_xml.moodle( fichier.xml )
    }

    ## On renvoie les glossaires traités...
    ##  (mais sans l'afficher : ne sert à rien)
    invisible( l )
}

csv_vers_glossaire <- function( fichier.csv,
                                colonne.terme, colonne.def,
                                fichier.xml,
                                embellir, deja.HTML,
                                sep, header, quote,
                                ... ) {
    ## On ouvre le fichier dans une data.frame
    d <- read.table( file = fichier.csv,
                     stringsAsFactors = FALSE, # on ne convertit pas en facteur : superflu et complique...
                     sep = sep, header = header, quote = quote, ... )
    cat( "  ", nrow( d ), " lignes lues...\n", sep = "" )

    ## On récupère les noms des colonnes...
    noms.colonnes <- names( d )

    ## On essaye de détecter des colonnes facultatives…

    ## … et on repère celles indiquées, si ce ne sont pas des nombres.
    ##   (remarque : si absente, NA : is.integer vaut FALSE
    ##                                sauf si NA_integer_ mais alors noms.colonnes[ ] renvoie NA
    ##    ==> devrait fonctionner…)
    if ( is.integer( colonne.terme ) ) colonne.terme <- noms.colonnes[ colonne.terme ]
    if ( is.integer( colonne.def   ) ) colonne.def   <- noms.colonnes[ colonne.def   ]

    cat( "  Colonne des termes  : ", colonne.terme  , "\n" )
    cat( "  Colonne des d\u00e9finitions : ", colonne.def, "\n" )
    
    ## On vérifie qu'il n'y a pas de doublons
    if ( any( duplicated( na.omit( c( colonne.terme, colonne.def ) ) ) ) ) {
        stop( "Une m\u00eame colonne ne peut pas servir \u00e0 deux informations distinctes !" )
    }

    ## On remplace les textes vides par des NA, plus faciles à repérer
    if ( any( nchar( d[ , colonne.terme ] ) < 1 ) )
        d[ which( nchar( d[, colonne.terme ] ) < 1 ) , colonne.terme ] <- NA
    
    ## On travaille par terme à définir…
    apply( d, 1, function( l ) {
        entree_glossaire.moodle( terme = l[ colonne.terme ],
                                 definition = l[ colonne.def ] )
    } )
    
    ## On renvoie la base des questions...
    d
}
