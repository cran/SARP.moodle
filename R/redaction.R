## ─────────────────────────────────────────────────────────────────
## Création de XML Moodle avec R
## © Emmanuel Curis — mars 2015, juin 2020
##
## Fonctions permettant la création de questions rédactionnelles
##  [type Moodle : essay]
## ─────────────────────────────────────────────────────────────────
## HISTORIQUE
##  25 juin 2020 : création du fichier
##                   (repris de libre.R)
##                 options de contrôle des réponses
##
##   1 jui. 2020 : correction de deux coquilles
##
##   3 jui. 2022 : adaptation pour utiliser le temps de catégorie
##
##  18 mai  2023 : conversion stop → erreur
## ─────────────────────────────────────────────────────────────────

## ─────────────────────────────────────────────────────────────────
##
##                         Question ouverte
##
## ─────────────────────────────────────────────────────────────────

question_ouverte.moodle <- function( texte,
                                     titre = "Question r&eacute;dactionnelle ouverte",
                                     editeur = c( 'WYSIWIG', 'WYSIWIG+', 'Texte', 'Chasse fixe', 'Aucun' ),
                                     avec.texte = TRUE, n.lignes = 15, 
                                     n.annexes = 0, n.optionnelles = min( n.annexes, 3 ), types = 'PDF',
                                     modele = NULL, informations = NULL,
                                     commentaire.global = NA, penalite = NA, note.question = NA, idnum = NA,
                                     temps,
                                     fichier.xml = get( "fichier.xml", envir = SARP.Moodle.env ) )
{
    ## On ajoute l'indication de temps éventuelle
    texte <- paste0( texte, 
                     temps_necessaire.moodle( temps ) )
    
    ## On crée la question
    debut_question.moodle( type = "essay",
                           titre = titre, texte = texte,
                           penalite = penalite, note = note.question,
                           commentaire.global = commentaire.global,
                           idnum = idnum,
                           fichier.xml = fichier.xml )

    ## L'éditeur demandé
    editeur <- match.arg( editeur )
    editeur <- switch( editeur,
                       'WYSIWIG'  = "editor",
                       'WYSIWIG+' = "editorfilepicker",
                       'Texte' = "plain",
                       'Chasse fixe' = "monospaced",
                       'Aucun' = "noinline",
                       erreur( 400, "question_ouverte.moodle",
                               "Type d'\u00e9diteur de texte inconnu" ) )
    cat( file = fichier.xml, sep = "",
         "   <responseformat>", editeur, "</responseformat>\n" )

    ## Veut-on une réponse ?
    ## Et combien de lignes pour l'éditeur de texte ?
    cat( file = fichier.xml, sep = "",
         "  <responserequired>", if ( avec.texte ) 1 else 0, "</responserequired>\n",
         "  <responsefieldlines>", n.lignes, "</responsefieldlines>\n" )
        
    ## Les fichiers attachés
    if ( missing( n.annexes ) ) n.annexes <- 0
    if ( any( is.null( n.annexes ), 
              is.na( n.annexes ) ) ) n.annexes <- 0
    n.annexes <- as.integer( n.annexes )    
    if ( n.annexes < 0 ) n.annexes <- 0
    
    ## Nombre de fichiers demandés
    cat( file = fichier.xml, sep = "",
         "  <attachments>", n.annexes, "</attachments>\n",
         "  <attachmentsrequired>", n.annexes - n.optionnelles, "</attachmentsrequired>\n" )

    ## Indications pour le correcteur
    if ( missing( informations ) ) informations <- NULL
    if ( all( !is.null( informations ),
              !is.na( informations ),
              nchar( informations ) > 0 ) ) {
        cat( file = fichier.xml, sep = "",
             "  <graderinfo format=\"html\">",
             coder.texte( informations,
                          indentation = 3, fichier.xml = fichier.xml ),
             "  </graderinfo>\n" )
    }

    ## Modèle de réponse
    if ( missing( modele ) ) modele <- NULL
    if ( all( !is.null( modele ),
              !is.na( modele ),
              nchar( modele ) > 0 ) ) {
        cat( file = fichier.xml, sep = "",
             "  <responsetemplate format=\"html\">",
             coder.texte( modele, indentation = 3, fichier.xml = fichier.xml ),
             "  </responsetemplate>\n" )
    }
    
    ## On a fini avec cette question
    fin_question.moodle( fichier.xml )
}
