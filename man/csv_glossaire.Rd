\name{csv_glossaire.moodle}
\alias{csv_glossaire.moodle}
\title{
  Convertir un fichier CSV en glossaire Moodle
}
\description{
  Cette fonction permet de convertir un fichier CSV, convenablement
  construit, en un fichier XML de glossaire pour Moodle
}
\usage{
csv_glossaire.moodle( fichier.csv,
                      colonne.terme = 'Mot',
                      colonne.definition = 'D\u00e9finition',
                      fichier.xml = if ( TRUE == nv.fichier ) gsub(
                            "\\\\.[Cc][Ss][Vv]$",
                            ".xml", fichier.csv )
                                    else get( "fichier.xml",
                                              envir = SARP.Moodle.env ),
                      nv.fichier = TRUE,
                      embellir = TRUE, deja.HTML = FALSE,
                      sep = ";", header = TRUE, quote = '\"',
                      ... )
}
\arguments{
  \item{fichier.csv}{
    Un vecteur de chaînes de caractères contenant les noms des fichiers
    à convertir. Chacun des fichiers est traité indépendamment, mais
    avec les mêmes valeurs des options qui suivent.
  }
  \item{colonne.terme}{
    Le nom ou le numéro de la colonne qui contient les termes à définir
    dans le glossaire.
  }
  \item{colonne.definition}{
    Le nom ou le numéro de la colonne qui contient les définitions des
    termes du glossaire.
  }
  \item{fichier.xml}{
    Le nom du fichier XML à créer ou un fichier XML déjà créé avec
    \code{\link{creer_glossaire.moodle}}. Par
    défaut, la fonction crée un fichier de même nom que le premier
    fichier fourni, en remplaçant l'extension \code{.csv} par
    \code{.xml}.
  }
  
  \item{nv.fichier}{
    Une valeur logique indiquant si l'on veut créer un nouveau fichier
    (\code{TRUE}) ou utiliser un fichier XML déjà ouvert avec
    \code{\link{creer_glossaire.moodle}} (\code{FALSE})
  }
  \item{embellir,deja.HTML}{
    Actuellement inutilisés, prévus pour une version future
  }
  \item{sep, header, quote}{
    Options pour \code{\link{read.table}}, correspondant au format d'un
    fichier CSV par défaut lorsqu'il est créé par Libre office ou Excel.
  }
  \item{...}{
    Autres options pour \code{\link{read.table}}, si nécessaire.
  }
}
\details{
  Cette fonction réalise la conversion d'un ou plusieurs fichiers CSV
  de définitions d'un glossaire pour Moodle en un fichier XML.

  Ce fichier doit comporter au moins deux colonnes : l'une avec les
  termes à définir et l'autre avec les définitions. Chaque ligne
  correspond à une définition.
}
\value{
  De façon invisible, une liste des data.frames des définitions après
  traitement de chaque fichier.  Elle n'a guère d'autre intérêt que pour
  détecter l'origine d'un problème de conversion.
}
\seealso{
  Les fonctions de base de création de glossaire pour plus de souplesse.
  }
\author{
  Emmanuel Curis \email{emmanuel.curis@parisdescartes.fr}
}
\examples{
  # Conversion du fichier d'exemple fourni
  #  (à placer dans le répertoire de travail)
\dontrun{
  csv_questionnaire.moodle( "exemple_glossaire.csv" )
}
}