\name{sortie_R.moodle}
\alias{sortie_R.moodle}
\alias{sortie_R.moodle.call}
\alias{sortie_R.moodle.default}
\alias{sortie_R.moodle.htest}
\alias{sortie_R.moodle.numeric}
\title{
  Insérer une sortie de R dans une question
}
\description{
  Ces fonctions permettent de créer du code HTML à partir du résultat
  d'une fonction R, avec une mise en forme spécifique, pour insertion
  dans une question Moodle.
}
\usage{
sortie_R.moodle( objet.R, ... )

\method{sortie_R.moodle}{htest}( objet.R, precision = 4, ... )
\method{sortie_R.moodle}{call}( objet.R, precision = 4, ... )
\method{sortie_R.moodle}{numeric}( objet.R, precision = 4, ... )
}
\arguments{

  \item{objet.R}{L'objet R à convertir}

  \item{precision}{Nombre de décimales à conserver pour l'affichage des
    nombres (arrondis avec \code{round}).
  }

  \item{\dots}{Paramètres de mise en forme additionnels, en particulier
    indiquant la commande à afficher (\code{commande}), sa couleur
    (\code{couleur.commande}, nom de couleur HTML/CSS et non R, par
    défaut du bleu), s'il faut un cadre (\code{cadre}, présent par
    défaut) et dans ce cas son épaisseur (\code{largeur.cadre}, 2 pixels
    par défaut) et sa couleur (\code{couleur.cadre}, noir par défaut).
  }
}

\value{
  Une chaîne de caractère contenant le code HTML approprié.
}
\author{
  Emmanuel Curis \email{emmanuel.curis@parisdescartes.fr}
}


  