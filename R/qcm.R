## ─────────────────────────────────────────────────────────────────
## Création de XML Moodle avec R
## © Emmanuel Curis — mars 2015
##
## Fonctions permettant la création de questions de type QCM
## ─────────────────────────────────────────────────────────────────
## Historique
##   12 juillet 2016 : ajouté le choix du style de numérotation
##                     enjolivé les commentaires
##
##   22 juillet 2016 : avertissement s'il y a des NA dans les notes
##                     corrigé un s manquant quand il n'y a pas de fausse réponse…
##
##    9 février 2017 : [Vrai-Faux] si l'ordre des réponses est imposé,
##                                 on interdit à Moodle de le choisir aléatoirement
##
##    1 avril   2017 : [Vrai-Faux] on désactive la numérotation des réponses
##
##   13 mai     2020 : options globales (note, pénalité)
##
##   31 mai     2020 : ajout du temps conseillé pour répondre
##
##    1 juillet 2020 : si QCU, pas de point négatif par défaut pour les mauvaises réponses
##
##    1 janvier 2021 : prise en charge de l'identifiant numérique unique
##
##    3 juillet 2022 : adaptation pour utiliser le temps de catégorie
##
##   30 août    2022 : possibilité de ne pas afficher les instructions par défaut du QCM
##                       (balise <showstandardinstruction>)
##
##   18 mai     2023 : conversion stop → erreur et warning → avertissement
## ─────────────────────────────────────────────────────────────────

######################################################################
## 
## Question binaire
## 
vrai_faux.moodle <- function( texte, texte.vrai = "Vrai", texte.faux = "Faux",
                              commentaires = NULL,
                              titre = "Question vrai-faux...",
                              ordre = c( "aleatoire", NA, "random", "vrai premier", "faux premier" ),
                              melanger = FALSE,
                              fichier.xml = get( "fichier.xml", envir = SARP.Moodle.env ),
                              commentaire.global = NA, penalite = NA, note.question = NA,
                              idnum = NA,
                              temps )
{
    if( length( texte ) > 1 ) {
        avertissement( 340, "vrai_faux.moodle",
                       "Texte de longueur > 1 - Concat\u00e9nation" )
        texte <- paste0( texte, collapse = "" )
    }
    
    ## On ajoute l'indication de temps éventuelle
    texte <- paste0( texte, 
                     temps_necessaire.moodle( temps ) )

    ## On détermine l'ordre des réponses
    if ( any( is.null( ordre ), is.character( ordre ) ) ) {
        melanger = FALSE
        ordre <- match.arg( ordre )
        if ( tolower( ordre ) %in% c( "vrai premier" ) ) {
            rangs <- c( 1, 2 )
        } else if ( tolower( ordre ) %in% c( "faux premier" ) ) {
            rangs <- c( 2, 1 )
        } else {
            rangs <- sample( 1:2, size = 2, replace = FALSE )
        }
    }
    
    ## On prépare la réponse, dans l'ordre demandé
    reponses <- paste0( "<![CDATA[", c( texte.vrai, texte.faux )[ rangs ], "]]>" )
    attr( reponses, "fractions" ) <- c( 100, 0 )[ rangs ]

    if ( length( commentaires ) > 0 ) {
      attr( reponses, "commentaires" ) <- commentaires[ rangs ]
    }

    ## On crée la question
    ## Rq : on n'utilise pas le format truefalse, car il n'autorise pas de modifier les textes des vrai/faux...
    codes <- c( "single" = "true",     # Une seule réponse possible
                "shuffleanswers"  = if ( TRUE == melanger ) 1 else 0,
                "answernumbering" = "none" )

    question.moodle( fichier.xml = fichier.xml, type = "multichoice",
                     titre = titre, texte = texte, reponses = reponses,
                     penalite = penalite, note = note.question,
                     autres.codes = codes, commentaire.global = commentaire.global,
                     idnum = NA )
}

######################################################################
## 
## Question multiple
## 
qcm.moodle <- function( texte, bonnes.reponses, mauvaises.reponses,
                        commentaires = NULL, fractions = list( "Bonnes" = NULL, "Fausses" = NULL ),
                        unique = ( length( bonnes.reponses ) == 1 ), melanger = TRUE,
                        titre = "QCM...", numerotation = c( "none", "abc", "ABCD", "123" ),
                        fichier.xml = get( "fichier.xml", envir = SARP.Moodle.env ),
                        commentaire.global = NA, penalite = NA, note.question = NA, idnum = NA,
                        temps, instructions = TRUE )
{
    ## On prépare les réponses
    n.bonnes <- length( bonnes.reponses )
    if ( n.bonnes < 1 ) {
        ## Aucune bonne réponse ! On annule
        erreur( 350, "qcm.moodle",
                "Aucune bonne r\u00e9ponse indiqu\u00e9e..." )
    }

    ## Le pourcentage de points indiqués pour chaque bonne réponse
    n.fractions <- length( fractions$Bonnes )
    if ( 0 == n.fractions ) {
        fractions$Bonnes <- rep( 100 / n.bonnes, n.bonnes )
    } else if ( n.fractions > n.bonnes ) {
        erreur( 351, "qcm.moodle",
                "Plus de pourcentages de r\u00e9ponses que de bonnes r\u00e9ponses..." )
    } else {
        somme <- sum( fractions$Bonnes )
        fractions$Bonnes <- c( fractions$Bonnes,
                               rep( ( 100 - somme ) / ( n.bonnes - n.fractions ),
                                    n.bonnes - n.fractions ) )
    }
    somme <- sum( fractions$Bonnes )
    if ( somme != 100 ) {
        avertissement( 350, "qcm.moodle",
                       "La somme des points attribu\u00e9s ne vaut pas 100 % !" )
    }
    if ( any( is.na( fractions$Bonnes ) ) ) {
        avertissement( 351, "qcm.moodle",
                       "Certaines bonnes r\u00e9ponses n'ont pas de note associ\u00e9e !",
                       "On leur met la note 0 faute de mieux..." )
        fractions$Bonnes[ which( is.na( fractions$Bonnes ) ) ] <- 0
    }
    if ( any( fractions$Bonnes <= 0 ) ) {
        avertissement( 352, "qcm.moodle",
                       "Certaines bonnes r\u00e9ponses sont affect\u00e9es",
                       " d'une fraction n\u00e9gative !" )
    }

  ## Le pourcentage de points indiqué pour chaque mauvaise réponse
  n.mauvaises <- length( mauvaises.reponses )
  if ( n.mauvaises > 0 ) {
    n.fractions <- length( fractions$Fausses )
    if ( 0 == n.fractions ) {
        if ( unique ) {
            fractions$Fausses <- rep( 0, n.mauvaises )
        } else {
            fractions$Fausses <- - rep( 100 / n.mauvaises, n.mauvaises )
        }
    } else if ( n.fractions > n.mauvaises ) {
        erreur( 352, "qcm.moodle",
                "Plus de pourcentage de p\u00e9nalit\u00e9 que de mauvaise r\u00e9ponses..." )
    } else {
        if ( any( is.na( fractions$Fausses ) ) ) {
            fractions$Fausses[ which( is.na( fractions$Fausses ) ) ] <- 0
        }
        if ( all( fractions$Fausses >= 0 ) ) {
            fractions$Fausses <- -fractions$Fausses
        }
      somme <- sum( fractions$Fausses )
      fractions$Fausses <- c( fractions$Fausses,
                              - rep( ( 100 - somme ) / ( n.mauvaises - n.fractions ),
                                     n.mauvaises - n.fractions ) )
    }
    if ( any( is.na( fractions$Fausses ) ) ) {
        avertissement( 353, "qcm.moodle",
                       "Certaines fausses r\u00e9ponses n'ont pas de fraction associ\u00e9e [NA].",
                       " On leur attribue la note 0." )
        fractions$Fausses[ which( is.na( fractions$Fausses ) ) ] <- 0
    }
    if ( any( fractions$Fausses > 0 ) ) {
        avertissement( 354, "qcm.moodle",
                       "Certaines fausses r\u00e9ponses sont affect\u00e9es d'une fraction positive !" )
    }
  } else {
      avertissement( 355, "qcm.moodle",
                     "Il n'y a aucune mauvaise r\u00e9ponse dans ce Q. C. M. !" )
      fractions$Fausses <- NULL
  }

  ## On assemble le tout
  ##  (en protégeant les textes, au cas où…)
  reponses <- paste0( "<![CDATA[", c( bonnes.reponses, mauvaises.reponses ), "]]>" )
  attr( reponses, "fractions" ) <- c( fractions$Bonnes, fractions$Fausses )
  if ( length( commentaires ) > 0 ) {
    attr( reponses, "commentaires" ) <- commentaires
  }

  ## Compléments
  codes <- c( "single"         = if ( TRUE == unique   ) "true" else "false",
              "shuffleanswers" = if ( TRUE == melanger ) 1 else 0,
              "answernumbering"= match.arg( numerotation ),
              "showstandardinstruction" = if ( TRUE == instructions ) 1 else 0 )

  ## On ajoute l'indication de temps éventuelle
  texte <- paste0( texte, 
                   temps_necessaire.moodle( temps ) )

  ## On fait la question
  question.moodle( fichier.xml = fichier.xml, type = "multichoice",
                   titre = titre, texte = texte, reponses = reponses,
                   penalite = penalite, note = note.question,
                   autres.codes = codes, commentaire.global = commentaire.global,
                   idnum = NA )
}

######################################################################
## 
## Question rapide ouverte et courte
## 
qroc.moodle <- function( texte, reponses, notes = rep( 100, length( reponses ) ),
                         commentaires = NULL, casse = TRUE,
                         titre = "QROC...",
                         fichier.xml = get( "fichier.xml", envir = SARP.Moodle.env ),
                         commentaire.global = NA, penalite = NA, note.question = NA, 
                         idnum = NA,
                         temps )
{
    ## On construit les réponses
    reponses <- paste0( "<![CDATA[", reponses, "]]>" )
    attr( reponses, "fractions" ) <- notes
    if ( length( commentaires ) > 0 ) {
        attr( reponses, "commentaires" ) <- commentaires
    }

    ## Compléments
    codes <- c( "usecase"         = if ( TRUE == casse ) 1 else 0 )

    ## On ajoute l'indication de temps éventuelle
    texte <- paste0( texte, 
                     temps_necessaire.moodle( temps ) )

    ## On fait la question
    question.moodle( fichier.xml = fichier.xml, type = "shortanswer",
                     titre = titre, texte = texte, reponses = reponses,
                     penalite = penalite, note = note.question,
                     autres.codes = codes,
                     commentaire.global = commentaire.global, idnum = idnum )
}
