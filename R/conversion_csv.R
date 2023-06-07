## ——————————————————————————————————————————————————————————————————————
## Création de XML Moodle avec R
## © Emmanuel Curis — mars 2015, mai 2019
##
## Conversion d'un fichier CSV de format standardisé
##   en un fichier XML importable par Moodle
##
## Sur une idée de Virginie LASSERRE, février 2016
## ——————————————————————————————————————————————————————————————————————
## Historique
##   9 février 2017 : début de l'historique (…)
##                    ajouté une option en ligne de commande : mélange des réponses…
##                    préparé pour une colonne de type de question (QCM/QCU)
##
##   1 avril   2017 : corrigé la sélection de type par la colonne Type / QCU
##                    (était ignoré avant, voire risque d'erreur « objet inexistant »)
##
##  15 mai     2019 : possibilité d'avoir une somme des points nulle sur un QCM
##                    (option somme.nulle = TRUE)
##
##  15 nov.    2019 : la colonne des commentaires globaux est prise en compte
##                     [erreur signalée par Céline Bugli ce jour...]
##
##  19 avril   2020 : premiers essais d'image et de formules dans le texte
##                     => les formules semblent fonctionner (avec @$, $@)
##                    détection automatique d'une colonne code de question
##                    correction de la détection des questions si colonne de code
##
##  20 avril   2020 : syntaxe pour contrôler la taille des images
##
##  21 avril   2020 : syntaxe pour générer des formules chimiques par code SMILE
##
##  26 avril   2020 : corrections mineures pour soumission au CRAN
##
##  27 avril   2020 : possibilité de préciser le nombre de décimales
##
##  30 avril   2020 : codage d'une description pour une image : |texte (en fin de code)
##                    corrigé la détection des réponses entières...
##
##  12 mai     2020 : possibilité de donner une note à la question [1 par défaut]
##                    possibilité de donner une pénalité à la question [aucune par défaut]
##
##   4 juin    2020 : dossier par défaut pour les images = celui du fichier
##
##  24 juin    2020 : préparation des textes des questions & réponses
##                     (suppression des blancs indus — sur une idée de Virginie)
##
##   1 juillet 2020 : si QCU demandé, on ne met pas de points négatif aux mauvaises réponses
##
##  31 octobre 2020 : corrigé une erreur qui bloquait l'utilisation de csv.moodle
##                     pour un fichier XML déjà existant...
##
##  26 janvier 2022 : ajouté le type « question rédactionnelle »
##
##  24 avril   2022 : correction, le titre était ignoré pour les questions « cloze »
##                    colonne pour le temps conseillé [aucune par défaut]
##
##  28 mai     2022 : correction, le nombre de réponses était faux
##                      en cas de texte final pour une question « cloze »
##                    avertissements si structure bizarre de question « cloze »
##
##  14 juin    2022 : détection automatique d'une colonne de titres, si présente
##                    correction de la détection des textes finaux, question « cloze »
##                    compte-rendu plus détaillé pour les questions « cloze »
##
##   1 juillet 2022 : gestion des QCM dans les questions cloze (types multiresponse*)
##                    code A possible pour ordre aléatoire des réponses / QCM cloze
##                    gestion des notes numériques dans les QCM cloze
##
##   3 juillet 2022 : temps nécessaire défini pour une catégorie
##
##   4 juillet 2022 : notes numériques (0 ou 1) possibles pour les MULTICHOICE
##                    corrigé la détection des champs pour réponse unique
##                    code pour les questions numériques à réponses multiples
##                    réaménagement du code : - pour autres formats de fichier
##                                            - fonctions distinctes 1 ligne/plusieurs lignes
##
##  30 mars    2023 : transfert de toute la partie « data.frame » dans conversion_df.R
##                      qui devient une fonction visible de l'extérieur
##                    simplification de csv.moodle qui passe ... à df.moodle
##                    possibilité d'indiquer une colonne pour le nombre de décimales
##
##   9 avril   2023 : vérifications préliminaires mises dans une fonction séparée
##                      => réutilisable pour d'autres formats de fichier...
##                    corrigé : quand on passe une liste de fichiers,
##                      convertit bien chacun à tour de rôle et pas X fois le premier...
##
##  15 mai   2023 : avertissement si extension inattendue
##                  message d'erreur si fichier inexistant
##
##  18 mai   2023 : conversion stop → erreur() et warning → avertissement()
## ——————————————————————————————————————————————————————————————————————

## La seule fonction visible de l'extérieur...
##
## fichier.csv = le nom du fichier csv à convertir [character]
## fichier.xml = le fichier contenant les questions formatées
##               soit le nom d'un fichier à créer,
##               soit un fichier XML en cours d'usage
## nv.fichier  = si TRUE, on crée un fichier XML en entier...
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
## 
## sep, header,... : options passées à read.table
## 
csv.moodle <- function( fichier.csv,
                        colonne.texte  = NA, colonne.reponse = NA,
                        colonne.note   = NA, colonne.note_question = NA,
                        colonne.titre  = NA, colonne.code = NA, colonne.type = NA,
                        colonne.retour = NA, colonne.global = NA, colonne.penalite = NA,
                        colonne.temps  = NA, colonne.decimale = NA,
                        fichier.xml = if ( TRUE == nv.fichier ) gsub( "\\.[Cc][Ss][Vv]$",
                                                                      ".xml",
                                                                      fichier.csv )
                                      else get( "fichier.xml", envir = SARP.Moodle.env ),
                        nv.fichier = TRUE,
                        creer.titre = TRUE, lg.titre = 30, embellir = TRUE, deja.HTML = FALSE,
                        forcer.multiple = TRUE, melanger.reponses = TRUE, somme.nulle = FALSE,
                        precision = 3,
                        categorie.base = "",
                        dossier.images = dirname( fichier.csv ),
                        sep.images = c( '@@', '@@' ), inserer.images = TRUE,
                        sep.formules = c( '@\\$', '\\$@' ),
                        sep.SMILES = c( '@\\{', '\\}@' ),
                        sep = if ( extension == "txt" ) "" else ";",
                        header = TRUE, quote = '"', 
                        ... ) {
    ## Vérifications initiales
    verification_conversions( fichier.xml = fichier.xml, nv.fichier = nv.fichier,
                              inserer.images = inserer.images,
                              dossier.images = dossier.images,
                              fonction = "csv.moodle" )

    
    ## Si demandé : on crée le fichier XML
    if ( TRUE == nv.fichier ) {
        if ( any( is.na( fichier.xml ), length( fichier.xml ) != 1,
                  FALSE == is.character( fichier.xml ), nchar( fichier.xml ) < 1 ) ) {
            erreur( 7, "csv.moodle",
                    "Il faut indiquer un et un seul nom de fichier XML pour la sortie..." )
        }

        fichier.xml <- debuter_xml.moodle( fichier.xml )
    }

    ## Catégorie de base
    if ( missing( categorie.base ) ) categorie.base <- ""
    if ( any( is.na( categorie.base ),
              is.null( categorie.base ) ) )
        categorie.base <- ""
    
    ## On charge les fichiers CSV indiqués à tour de rôle...
    l <- as.list( rep( "", length( fichier.csv ) ) )
    names( l ) <- fichier.csv
    for ( f in fichier.csv ) {
        cat( "*** Traitement du fichier \u00ab ", f, " \u00bb ***\n" )

        if ( FALSE == file.exists( f ) ) {
            erreur( 100, "csv.moodle",
                    "Fichier inexistant [", f, "]" )
        }
        
        ## Contrôle de l'extension
        extension <- strsplit( basename( f ), ".", fixed = TRUE )[[ 1 ]]
        extension <- extension[ length( extension ) ]
        extension <- tolower( extension )

        if ( !( extension %in% c( "csv", "txt", "asc" ) ) ) {
            avertissement( 101, "csv.moodle",
                           "Extension inhabituelle pour un fichier texte - ",
                           extension )
        }

        ## On ouvre le fichier dans une data.frame
        d <- read.table( file = f,
                         stringsAsFactors = FALSE, # on ne convertit pas en facteur : superflu et complique...
                         sep = sep, header = header, quote = quote, ... )
        cat( "  ", nrow( d ), " lignes lues...\n", sep = "" )

        ## On fait le traitement réel...
        d <- df.moodle( d = d,
                        colonne.texte    = colonne.texte,
                        colonne.reponse  = colonne.reponse,
                        colonne.note     = colonne.note,
                        colonne.note_question = colonne.note_question,
                        colonne.titre    = colonne.titre,
                        colonne.code     = colonne.code,
                        colonne.type     = colonne.type,
                        colonne.retour   = colonne.retour,
                        colonne.global   = colonne.global,
                        colonne.penalite = colonne.penalite,
                        colonne.temps    = colonne.temps,
                        colonne.decimale = colonne.decimale,
                        fichier.xml      = fichier.xml, 
                        creer.titre      = creer.titre,
                        lg.titre         = lg.titre   ,
                        embellir         = embellir   ,
                        deja.HTML        = deja.HTML  ,
                        forcer.multiple  = forcer.multiple,
                        melanger.reponses = melanger.reponses,
                        somme.nulle      = somme.nulle,
                        precision        = precision,
                        categorie.base   = categorie.base,
                        dossier.images   = dossier.images,
                        sep.images       = sep.images,
                        inserer.images   = inserer.images,
                        sep.formules     = sep.formules,
                        sep.SMILES       = sep.SMILES,
                        nom.fichier      = f )
        l[[ f ]] <- d
    }

    ## On a terminé
    if ( TRUE == nv.fichier ) {
        finir_xml.moodle( fichier.xml )
    }

    ## On renvoie les bases de questions traitées...
    ##  (mais sans l'afficher : ne sert à rien)
    invisible( l )
}

verification_conversions <- function( fichier.xml, nv.fichier,
                                      inserer.images, dossier.images,
                                      fonction ) {
        
    ## Vérification : fichier XML
    ##   [On ne peut pas utiliser « missing » pour les colonnes avec une valeur par défaut :
    ##                                        renvoie « TRUE » si non-spécifié sur la ligne de commande...]
    if ( all( FALSE == is.character( fichier.xml ),
              FALSE == ( "file" %in% class( fichier.xml ) ) ) ) {
        erreur( 3, fonction,
                "Le fichier XML doit \u00eatre un nom de fichier \u00e0 cr\u00e9er",
                " ou un fichier d\u00e9j\u00e0 ouvert par debuter_xml.moodle" )
    }
    
    ## On contrôle le dossier d'images
    if ( !is.logical( inserer.images ) ) {
        erreur( 4, fonction,
                "inserer.images doit valoir TRUE ou FALSE" )
    }
    if ( length( inserer.images ) != 1 ) {
        erreur( 5, fonction,
                "inserer.images ne doit avoir qu'une seule valeur, TRUE ou FALSE" )
    }

    if ( inserer.images ) {
        ## Contrôler le dossier d'images
        ##   n'a de sens que si l'on veut intégrer des images...
        if ( length( dossier.images ) > 1 ) {
            erreur( 6, fonction,
                    "Un seul dossier contenant les images",
                    " doit \u00eatre sp\u00e9cifi\u00e9" )
        }
        if ( length( dossier.images ) == 1 ) {
            if ( FALSE == dir.exists( dossier.images ) ) {
                erreur( 101, fonction,
                        "Le dossier d'images indiqu\u00e9 n'existe pas",
                        " [", dossier.images, "]." )
            }
        }
    }

    ## Vérifications passées, rien à renvoyer
}
