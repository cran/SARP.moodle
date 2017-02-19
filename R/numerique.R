## ─────────────────────────────────────────────────────────────────
## Création de XML Moodle avec R
## © Emmanuel Curis — mars 2015
##
## Fonctions permettant la création de questions numériques « simples »
## ─────────────────────────────────────────────────────────────────
## Historique
##   24 octobre 2016 : mise en page adaptée
## ─────────────────────────────────────────────────────────────────

numerique.moodle <- function( texte, bonne.reponse, etendue = NULL,
                              n.decimales = get( "nombre.chiffres", envir = SARP.Moodle.env ),
                              titre = "Question num&eacute;rique...",
                              fichier.xml = get( "fichier.xml", envir = SARP.Moodle.env ),
                              commentaire.global = NA ) {
    if ( FALSE == is.numeric( bonne.reponse ) ) {
        stop( "Pour une question num\u00e9rique, la bonne r\u00e9ponse doit \u00eatre un nombre" )
    }
    
    ## On prépare la réponse
    if ( all( is.finite( n.decimales ) ) ) {
        reponse <- round( bonne.reponse, n.decimales )
        texte <- paste0( texte,
                         "<br />",
                         "<i>Vous donnerez la r&eacute;ponse avec ", n.decimales, "&nbsp;chiffres",
                         " apr&egrave;s la virgule.</i>" )
    } else {
        reponse <- bonne.reponse
    }
    reponse <- as.character( reponse )
    attr( reponse, "fractions" ) <- 100
    
    if ( missing( commentaire.global ) ) commentaire.global <- NA

    ## On crée la question
    question.moodle( fichier.xml = fichier.xml, type = "numerical",
                     titre = titre, texte = texte, reponses = reponse,
                     commentaire.global = commentaire.global )
}
