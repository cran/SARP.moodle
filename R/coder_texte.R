## —————————————————————————————————————————————————————————————————
## Création de XML Moodle avec R
## Emmanuel Curis — juin 2015
##
## Fonctions permettant de coder un texte (en extrayant des infos)
## —————————————————————————————————————————————————————————————————
## HISTORIQUE
##   29 janvier 2020 : création du fichier
##
##   24 mars    2020 : essai de protection contre le double codage
##                      de la même image interne dans un texte donné
## —————————————————————————————————————————————————————————————————

coder.texte <- function( texte, ajouter.images = TRUE, proteger.balises = FALSE,
                         balise.CDATA = TRUE,
                         indentation = 2, fichier.xml ) {
    if ( length( texte ) > 1 ) {
        stop( "Le texte pass\u00e9 n'est pas unique.",
              " Probl\u00e8me de codage ant\u00e9rieur !\n",
              texte )
    }
    blanc <- paste0( rep( " ", indentation ), collapse = "" )
    ## Le préambule
    cat( file = fichier.xml, sep = "",
         blanc, "<text>",
         if ( TRUE == balise.CDATA ) "<![CDATA[" )

    ## Le texte proprement dit
    cat( file = fichier.xml, sep = "",
         texte )

    ## La balise finale
    cat( file = fichier.xml, sep = "",
         if ( TRUE == balise.CDATA ) "]]>",
         "</text>\n" )
        
    ## On cherche les images pour les ajouter (si demandé)
    if ( TRUE == ajouter.images ) {
        av.images <- grepl( "@@PLUGINFILE@@", fixed = TRUE, texte )
        if ( av.images ) {
            ## On initialise la liste : éviter au moins les doublons internes...
            assign( "liste.images", character() , SARP.Moodle.env )

            pos.images <- gregexpr( "\"@@PLUGINFILE@@/.*?\"", fixed = FALSE, texte )[[ 1 ]]
            lg <- attr( pos.images, "match.length" )

            lapply( 1:length( lg ),
                   function( i ) {
                       ## On récupère le lien de l'image
                       img <- substr( texte, start = pos.images[ i ], stop = pos.images[ i ] + lg[ i ] - 1 )

                       ## On fait sauter le début et le guillemet final
                       img <- gsub( ".*/", '', img )
                       img <- gsub( "\"$", '', img )

                       ## On convertit l'image...
#                       print( texte )
                       cat( file = fichier.xml, sep = "",
                            blanc,
                            coder_image.moodle( img ),
                            "\n" )
                   } )
        }
    }
}
