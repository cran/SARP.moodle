\name{messages.moodle}
\alias{messages.moodle}
\alias{temps_necessaire.moodle}
\alias{arrondi.moodle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Aide aux affichages dans les textes des questions
}
\description{
  Ces fonctions permettent de créer une chaîne de caractère au format
  HTML, pour des messages courant d'indication dans les questions.
}
\usage{
temps_necessaire.moodle( temps, couleur = "Blue",
                         nv.ligne = TRUE )
}
\arguments{
  \item{temps}{Le temps conseillé pour la question. Ce peut être un
    nombre, le temps est alors supposé exprimé en minutes, ou une chaîne
    de caractères à un format de temps classique (par exemple 45s,
    2'30...).}

  \item{couleur}{La couleur à utiliser pour le message. N'importe quelle
    couleur reconnue en CSS peut être utilisée.}

  \item{nv.ligne}{Si \code{TRUE}, le message est précédé d'un retour à
    la ligne (balise HTML \code{<br />}). }
}

\details{
  Ces fonctions sont utilisées par les fonctions générant les questions
  classiques pour ajouter des consignes en fin d'énoncé. Elles peuvent
  être utilisées directement pour davantage de souplesse.

  Le message créé par \code{temps_necessaire.moodle} est \dQuote{Temps
    conseillé pour cette question : x min y s}, où x et y sont
  construits à partir du temps indiqué en minutes (par exemple, si
  \code{temps=1.5}, x=1 et y=30). Pour les questions classiques, il
  correspond à l'option \code{temps}.
}

\value{ Ces fonctions renvoient une unique chaîne de caractères, en
  HTML, prête à être intégrée dans un énoncé.
}

\author{
  Emmanuel Curis \email{emmanuel.curis@parisdescartes.fr}
}

\examples{
  # Indication d'un temps conseillé de 2 min 30
  temps_necessaire.moodle( 2.5 )

  # Indication d'un temps conseillé de 10 secondes
  temps_necessaire.moodle( "10s" )
}
