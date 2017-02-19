# -----------------------------------------------------------------
# Création de XML Moodle avec R
# Emmanuel Curis --- mars 2015
#
# Fonctions facilitant les affichages
# -----------------------------------------------------------------

######################################################################
# Affichage d'un nombre
#
afficher_nombre.moodle <- function( x,
                                    dec = get( "decimal", envir = SARP.Moodle.env ),
                                    n.chiffres = get( "nombre.chiffres", envir = SARP.Moodle.env ) ) {
  # Y a-t-il bien une et une seule valeur ?
  if ( is.null( x ) || ( 0 == length( x ) ) ) {
    return( paste0( "<span style=\"", get( "styles", envir = SARP.Moodle.env )$erreur, "\">&empty;</span>" ) )
  }
  if ( length( x ) > 1 ) {
    warning( "Attention, seule la premi\u00e8re valeur sera convertie, les autres seront ignor\u00e9es..." )
    x <- x[ 1 ]
  }

  # Valeur manquante ?
  if ( is.na( x ) ) {
    return( paste0( "<span style=\"", get( "styles", envir = SARP.Moodle.env )$erreur, "\">NA</span>" ) )
  }

  # Valeur non-numérique ?
  if ( is.nan( x ) ) {
    return( "<span style=\"", get( "styles", envir = SARP.Moodle.env )$erreur, "\">NaN</span>" )
  }

  # Valeur infinie ?
  if ( is.infinite( x ) ) {
    if ( x > 0 )
      return( "&infin;" )
    else
      return( "&minus;&infin;" )
  }

  # Si c'est une chaîne, on l'affiche telle qu'elle...
  if ( is.character( x ) ) {
      return( x )
  }

  # Valeur numérique
  #  1) conversion
  txt <- paste0( if ( log( abs( x ), 10 ) > -n.chiffres ) round( x, n.chiffres ) else signif( x, n.chiffres ) )
  #  2) nettoyage : le signe -
  txt <- gsub( "-", "&minus;", fixed = TRUE, txt )
  #                 le séparateur décimal
  txt <- gsub( ".", dec, fixed = TRUE, txt )
  #                 les puissances de 10
  if ( any( grepl( "e", fixed = TRUE, txt ) ) ) {
    # Recodage de la partie puissance de 10
    txt <- paste0( gsub( "e", "&thinsp;&times;&thinsp;10<sup>", txt ), "</sup>" )
    # On ôte le signe de l'exposant, si c'est un plus, et les 0 initiaux éventuels
    txt <- gsub( "p>[+]0*", "p>", txt )
    txt <- gsub( ";0*", ";", txt )
  }

  return( txt )
}

######################################################################
# Affichage d'un échantillon de valeurs
#  (dans un tableau idéalement)
afficher_echantillon.moodle <- function( x, tableau = TRUE, n.lignes = 1, trier = FALSE ) {
  # Veut-on trier les valeurs ?
  if ( TRUE == trier ) {
    x <- sort( x )
  }

  # Les valeurs converties en un vecteur de texte
  txt <- unlist( lapply( x, afficher_nombre.moodle ) )
  
  # Cas simple : échantillon de valeurs entre parenthèses
  if ( FALSE == tableau ) {
    return( paste0( "(", txt, ")", collapse = "&thinsp;; " ) )
  }

  ## Ici : on veut faire un tableau
  ##   (on met des espaces dans les cellules
  ##    parce que certaines versions de moodle ne mettent aucun blanc entre les colonnes...)
  txt <- paste0( "<table style=\"border-collapse: collapse;\">",
                 " <tr style=\"border-style: solid none solid none; border-color: Black;\">",
                 paste0( "<td style=\"padding-left: 10px; padding-right: 10px;\"> ", txt, " </td>", collapse = " " ),
                 " </tr>",
                 "</table>" )
  # On a fini...
  return( txt )
}
