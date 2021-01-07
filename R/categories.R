## ─────────────────────────────────────────────────────────────────
## Création de XML Moodle avec R
## © Emmanuel Curis — mars 2015
##
## Fonctions permettant la création de catégories
## ─────────────────────────────────────────────────────────────────
## Historique
##   22 juillet 2016 : supprime les / terminaux
##                     corrigé le remplacement des $…
##
##    1 janvier 2021 : on maintient une liste d'identifiants numériques
## ─────────────────────────────────────────────────────────────────

categorie.moodle <- function( nom.categorie, autoriser.dollar = FALSE,
                              fichier.xml = get( "fichier.xml", envir = SARP.Moodle.env ) ) {
    ## On supprime les $, sauf si explicitement demandé de les garder
    if ( FALSE == autoriser.dollar ) {
        nom.categorie <- gsub( '$', '_', fixed = TRUE, nom.categorie )
    }

    ## On supprime les // pour ne pas avoir de catégorie intermédiaire sans nom…
    nom.categorie <- gsub( "/{2,}", "/", nom.categorie )
    
    ## On supprime les / terminaux, sinon on a une sous-catégorie sans nom…
    nom.categorie <- gsub( "/+$", "", nom.categorie )

    ## On crée la catégorie dans le fichier XML produit
    cat( file = fichier.xml, sep = "", "\n",
         "<question type=\"category\">\n",
         "  <category>\n",
         "    <text><![CDATA[$course$/", nom.categorie, "]]></text>\n",
         "  </category>\n",
         "</question>\n" )

    ## On remet à 0 la liste des identifiants
    assign( "liste.ids", integer(), envir = SARP.Moodle.env )
}
