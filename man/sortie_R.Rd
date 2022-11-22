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
\method{sortie_R.moodle}{numeric}( objet.R, precision = 4,
            enjoliver = !cadre, cadre = TRUE,
            couleur.trait = "Black", lg.trait = "2",
            marge = if ( enjoliver ) c( 10, 10, 1, 1 ) else c( 25, 25, 1, 1 ),
            pre.X = NA, pre.X.nom = NA, noms.gras = TRUE, ... )
}
\arguments{

  \item{objet.R}{L'objet R à convertir}

  \item{precision}{Nombre de décimales à conserver pour l'affichage des
    nombres (arrondis avec \code{round}). }

  \item{enjoliver}{Si \code{TRUE}, la sortie est mise en forme en
    utilisant des balises HTML appropriées. Sinon, elle est faite pour
    reproduire le plus possible l'affichage dans la console R. }

  \item{cadre}{Si \code{TRUE}, un cadre est dessiné (via les styles CSS
    appropriées) tout autour de la sortie. }

  Options pour l'affichage des vecteurs numériques
  
  \item{couleur.trait, lg.trait}{Couleur et épaisseur des filets des
    tableaux, pour la mise en forme enjolivée. La couleur peut être
    n'importe quelle couleur autorisée en CSS ; l'épaisseur des filets
    est exprimée en pixels. }

  \item{marge}{Les marges à laisser au sein de chaque case du tableau
    affichant les résultats. Ce doit être un vecteur de quatre valeurs
    entières, correspondant dans l'ordre aux marges gauche, droite,
    haute et basse, exprimées en pixels. }

  \item{pre.X, pre.X.nom}{Dans la version enjolivée, textes à afficher
    en première colonne du tableau, avant les premières valeurs.}

  \item{noms.gras}{Si \code{TRUE}, les noms des éléments du vecteur sont
  affichés en gras dans la première ligne du tableau.}
  
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
\seealso{\code{\link{afficher_echantillon.moodle}} pour afficher un
  vecteur de valeurs sous forme d'un tableau.
}
\author{
  Emmanuel Curis \email{emmanuel.curis@parisdescartes.fr}
}


  