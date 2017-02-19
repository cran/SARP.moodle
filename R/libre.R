## ─────────────────────────────────────────────────────────────────
## Création de XML Moodle avec R
## © Emmanuel Curis — mars 2015
##
## Fonctions permettant la création de questions « libres »
## ─────────────────────────────────────────────────────────────────
## Historique
##   12 juillet 2016 : supprimé les accents directs restants
##                     enjolivé les commentaires
## ─────────────────────────────────────────────────────────────────

######################################################################
##                                                                  ## 
##                         Question ouverte                         ##
##                                                                  ##
######################################################################
question_ouverte.moodle <- function( texte,
                                     titre = "Question r&eacute;dactionnelle ouverte",
                                     fichier.xml = get( "fichier.xml", envir = SARP.Moodle.env ) ) {
  ## On prépare la réponse
  reponses <- ""
  attr( reponses, "fractions" ) <- 0

  ## On crée la question
  question.moodle( fichier.xml = fichier.xml, type = "essay",
                   titre = titre, texte = texte, reponses = reponses )
}


######################################################################
##                                                                  ## 
##                        Question « cloze »                        ##
##                                                                  ##
######################################################################

######################################################################
## 
## Les questions avec plusieurs réponses, complètement libres
##  (attention, fonction non-exportée)
##
generer_question <- function( note = NA, type, reponses, commentaires ) {
  ## Préparation des réponses : le plus long
  
  if ( type == "NUMERICAL" ) {
    if ( length( reponses ) != 1 ) {
        if ( is.list( reponses ) ) {
        } else {
            ## Plusieurs variantes de réponses correctes
            reponse <- paste0( "=", reponses, "#", commentaires )
            reponse <- paste0( reponse, collapse = "~" )
        }
    } else {
        reponse <- paste0( "=", reponses )
        if ( nchar( commentaires[ 1 ] ) > 0 ) reponse <- paste0( reponses, "#", commentaires )
    }
  }
  if ( type %in% c( "MULTICHOICE", "MULTICHOICE_V", "MULTICHOICE_H",
                    "SHORTANSWER", "SHORTANSWER_C" ) ) {
    txt.reponses  <- reponses$Textes
    txt.correctes <- reponses$Correct

    n.correctes <- length( which( txt.correctes == TRUE ) )
    if ( n.correctes == 0 ) {
      warning( "Aucune r\u00e9ponse correcte parmi celles indiqu\u00e9es !",
               "(R\u00e9ponses : ", paste0( "[", txt.reponses, "]", collapse = " // " ), ")" )
      points <- 0
      correct <- ""
    } else {
      if ( 1 == n.correctes ) {
        correct <- ifelse( txt.correctes, "=", "" )
      } else {
        points <- round( 100 / n.correctes, 3 )
        correct <- paste0( "%", ifelse( txt.correctes, points, 0 ), "%" )
      }
    }

    reponse <- paste0( correct, reponses$Textes,
                       ifelse( is.na( commentaires ), "", paste0( "#", commentaires ) ) )
    reponse <- paste0( reponse, collapse = "~" )
  }
  
  question <- paste0( "{", if ( is.finite( note ) ) note,
                      ":", type, ":",
                      reponse,
                      "}" )
  question
}

######################################################################
## 
## Question libre « cloze »
## 
question_libre.moodle <- function( texte.intro, textes.avant, texte.final,
                                   reponses, notes = NULL, types = NULL, commentaires = NULL,
                                   titre = "Question libre",
                                   commentaire.global = NA,
                                   fichier.xml = get( "fichier.xml", envir = SARP.Moodle.env ) ) {
  # Combien de questions au total ?
  n.questions <- length( reponses )

  # On prépare les éléments
  if ( length( notes ) == 0 ) {
    notes <- rep( NA, n.questions )
  }
  if ( length( types ) == 0 ) {
    types <- rep( "NUMERICAL", n.questions )
  }
  if ( length( types ) != n.questions ) {
      stop( "Il faut autant de types que de champs r\u00e9ponses..." )
  }
  
  if ( length( commentaires ) == 0 ) {
    commentaires <- as.list( rep( "", n.questions ) )
  }

  champs <- rep( "", n.questions )
  for(  i in 1:n.questions ) {
    champs[ i ] <- generer_question( note = notes[ i ], type = types[ i ], reponses = reponses[[ i ]],
                                     commentaires = commentaires[[ i ]] )
  }
  
  texte <- paste0( texte.intro,
                   paste0( paste( textes.avant, champs ), collapse = "" ),
                   texte.final )

  if ( missing( commentaire.global ) ) commentaire.global <- NA

  ## On crée la question
  question.moodle( fichier.xml = fichier.xml, type = "cloze",
                   titre = titre, texte = texte, reponses = NULL ,
                   commentaire.global = commentaire.global )
}
