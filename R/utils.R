## ——————————————————————————————————————————————————————————————————————
## Création de XML Moodle avec R
## © Emmanuel Curis — mars 2015, mars 2023
##
## Fonctions utilitaires diverses
## ——————————————————————————————————————————————————————————————————————
## Historique
##  30 mars 2023 : création du fichier
##
##  18 mai  2023 : conversion stop → erreur
##
##   1 juin 2023 : on cherche aussi les pluriels « classiques » (+s)
## ——————————————————————————————————————————————————————————————————————

## ——————————————————————————————————————————————————————————————————————
##
##               Trouver une colonne dans une data.frame
##
## colonne     : la colonne cherchée (numéro ou nom)
##                  si NA, "", NULL, <0 : on essaye de la trouver automatiquement
## variantes   : vecteur de chaînes = les noms par défaut
## type        : le type de colonne cherchée
## obligatoire : si TRUE, erreur si l'on ne trouve pas la colonne
## message.erreur : le message à afficher si l'on ne trouve pas une colonne obligatoire
## ——————————————————————————————————————————————————————————————————————
trouver_colonne <- function( colonne, variantes, noms.colonnes, type,
                             obligatoire = FALSE, message.erreur ) {
    ## Harmonisation : nom manquant…
    if ( is.double( colonne ) ) colonne <- as.integer( colonne )
    if ( any( is.null( colonne ),
              ( is.integer( colonne ) && ( colonne < 1 ) ),
              ( is.character( colonne ) && ( nchar( colonne ) < 1 ) ) ) ) {
        colonne <- NA
    }

    if ( is.na( colonne ) ) {
        variantes <- tolower( variantes )
        variantes <- c( variantes, 
                        oter_accents( variantes ) )
        variantes <- c( variantes, paste0( variantes, "s" ) ) # Pluriel
        if ( any( variantes %in% tolower( noms.colonnes ) ) ) {
            colonne <- which( tolower( noms.colonnes ) %in% variantes )[ 1 ]
            cat( "  Colonne de ", type, " d\u00e9tect\u00e9e   : colonne ", colonne,
                 " [", noms.colonnes[ colonne ], "]\n", sep = "" )
            return( colonne )
        }

        if ( obligatoire ) {
            erreur( 1014, "trouver_colonne",
                    "Vous devez indiquer quelle colonne contient ",
                    message.erreur )
        }
        return( NA )                    # Colonne non trouvée
    }

    if ( is.integer( colonne ) ) {
        if( colonne > length( noms.colonnes ) ) {
            erreur( 1015, "trouver_colonne",
                    "Num\u00e9ro de colonne incorrect [", colonne, 
                    " mais seulement ", length( noms.colonnes ), " colonnes]" )
        }

        return( colonne )
    }

    if ( is.character( colonne ) ) {
        if ( colonne %in% noms.colonnes ) {
            return( which( noms.colonnes == colonne )[ 1 ] )
        }
        if ( tolower( colonne ) %in% tolower( noms.colonnes ) ) {
            avertissement( 1007, "trouver_colonne",
                           "Attention, colonne ", colonne,
                           " trouv\u00e9e en n\u00e9gligeant la casse" )
            return( which( tolower( noms.colonnes ) == tolower( colonne ) )[ 1 ] )
        }

        erreur( 1016, "trouver_colonne",
                "Colonne demand\u00e9e non trouv\u00e9e : ", colonne )
    }

    erreur( 1017, "trouver_colonne",
            "Type d'identifiant de colonne non g\u00e9r\u00e9 [", colonne, "]" )
}

## ——————————————————————————————————————————————————————————————————————
##
##                      Ôter les accents d'un texte
##
## ——————————————————————————————————————————————————————————————————————
oter_accents <- function( texte ) {
    texte <- gsub( "[\u00e0\u00e1\u00e2\u00e3\u00e4\u00e5]", "a", texte )
    texte <- gsub( "\u00e7",                                 "c", texte )
    texte <- gsub( "[\u00e8\u00e9\u00ea\u00eb]",             "e", texte )
    texte <- gsub( "[\u00ec\u00ed\u00ee\u00ef]",             "i", texte )
    texte <- gsub( "\u00f1",                                 "n", texte )
    texte <- gsub( "[\u00f2\u00f3\u00f4\u00f5\u00f6\u00f8]", "o", texte )
    texte <- gsub( "[\u00f9\u00fa\u00fb\u00fc]",             "u", texte )
    texte <- gsub( "[\u00fd\u00ff]",                         "y", texte )

    texte
}
