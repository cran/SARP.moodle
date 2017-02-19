\name{glossaire.moodle}
\alias{creer_glossaire.moodle}
\alias{entree_glossaire.moodle}
\title{
  Commencer et terminer la génération de glossaires Moodle
}
\description{
  Ces fonctions permettent de préparer et de remplir un glossaire Moodle au format XML.
}
\usage{
creer_glossaire.moodle( nom.fichier, nom.glossaire, texte.intro,
                        doublons = TRUE )

entree_glossaire.moodle( terme, definition,
                         fichier.xml = get( "fichier.xml", envir = SARP.Moodle.env ) )
}
\arguments{
  \item{nom.fichier}{Une chaîne de caractères indiquant le
    nom du fichier XML, contenant le glossaire, à créer.}

  \item{nom.glossaire}{Une chaîne de caractères indiquant le
    nom du glossaire.}

  \item{texte.intro}{Une chaîne de caractères indiquant le
    texte décrivant le glossaire.}

  \item{doublons}{Une valeur logique indiquant si les doublons sont
  autorisés.}

  \item{terme}{Une chaîne de caractères indiquant le
    terme à insérer dans le glossaire.}
  
  \item{definition}{Une chaîne de caractères indiquant la définition du
    terme à insérer dans le glossaire.}
  
  \item{fichier.xml}{ Le fichier correspondant au glossaire qui
    contiendra cette entrée (par défaut, le dernier créé).  }
}

\details{ Ces fonctions permettent de préparer un glossaire Moodle au
  format XML.
}
\section{Attention}{Le support des glossaires au format XML est
  expérimental, en l'absence de documentation officielle.}

\value{
Aucune valeur n'est renvoyée par cette fonction.
}
\author{
  Emmanuel Curis \email{emmanuel.curis@parisdescartes.fr}
}
% \seealso{}
% \examples{
%}