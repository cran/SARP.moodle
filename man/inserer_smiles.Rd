\name{inserer_SMILES.moodle}
\alias{inserer_SMILES.moodle}
\title{
  Insérer une formule chimique, comme image, grâce à openbabel
}
\description{
  Cette fonction permet de convertir une formule chimique définie par un
  code SMILES en une image prête à insérer dans un texte.
}
\usage{
inserer_SMILES.moodle( code.SMILES, nom.molecule = code.SMILES,
                       largeur = 300, hauteur = 300,
                       couleur.atomes = TRUE,
                       couleur.fond = NA,
                       couleur.liaisons = NA,
                       double.liaisons.asymetrique = FALSE,
                       masquer.terminaux = TRUE,
                       dessiner.CH = FALSE,
                       marges = FALSE,
                       cmd.obabel = "obabel " )

}
\arguments{
  \item{code.SMILES}{Une chaîne de caractères contenant le code SMILES à
    convertir.  }

  \item{nom.molecule}{Une chaîne de caractère qui sera utilisée comme
    description de l'image, dans le code HTML produit pour Moodle.  }

  \item{largeur, hauteur}{Les dimensions de l'image, en pixels}

  \item{couleur.atomes}{Faut-il utiliser le code couleur habituel pour
    indiquer les atomes? (Option \dQuote{-xu} d'open babel.)}

  \item{couleur.fond}{Couleur à utiliser pour le fond de l'image
    (Option \dQuote{-xb} d'open babel; \code{NA}: fond par défaut.)}

  \item{couleur.liaisons}{Couleur à utiliser pour les liaisons chimiques
    (\code{NA}: fond par défaut d'open babel)}

  \item{double.liaisons.asymetrique}{Option \dQuote{-xs} d'open babel.}

  \item{masquer.terminaux}{Faut-il afficher en toutes lettres les
    méthyles terminaux? (Option \dQuote{-xC} d'open babel.)}

  \item{dessiner.CH}{Faut-il afficher en toutes lettres les
    atomes de carbone? (Option \dQuote{-xa} d'open babel.)}

  \item{marges}{Si \code{FALSE}, les marges autour de la formule sont
    éliminées (option \dQuote{-xm} d'open babel).  Comme les marges
    dépendent aussi des dimensions d'image demandées et de la formule
    créée, open babel peut laisser des marges assez grandes malgré tout,
    aussi l'image est rognée grâce à l'utilitaire \dQuote{convert}
    (option \dQuote{-trim}), qui doit donc aussi être installé et
    exécutable par R.}

  \item{cmd.obabel}{Une chaîne de caractères indiquant la commande à
    utiliser pour lancer open babel}  
}

\details{ Cette fonction permet de créer une image d'une formule
  chimique semi-développée, à partir du code SMILES correspondant.

  La conversion est faite avec open babel, qui doit être installé et
  exécutable par R pour que la conversion puisse avoir lieu. Par défaut,
  pour ne pas avoir de marges, l'utilitaire convert est aussi utilisé.
}

\value{
  Une chaîne de caractère contenant le code XML nécessaire pour inclure
  une image (contenant la formule créée), et qu'elle soit incluse au
  fichier XML au moment de la création de la question.
}

\seealso{
  \code{\link{inserer_formule.moodle}} pour convertir un code latex en
  une formule mathématique et l'insérer dans une question Moodle.

  \code{\link{lier_image.moodle}} pour insérer une image quelconque.
}

\author{
  Emmanuel Curis \email{emmanuel.curis@parisdescartes.fr}
}
\examples{
  # Création d'une question demandant quelle est la formule du benzène.
  #  (Non exécuté car open babel et convert non nécessairement installés)
\dontrun{
  # Commencer un nouveau questionnaire
  q <- debuter_xml.moodle( "benzene" )

  # Catégorie : exemples
  categorie.moodle( "Exemples SARP/SMILES" )

  # Le format par défaut, avec deux bonnes réponses et trois mauvaises,
  qcm.moodle( "Quelle est la formule du benzène&thinsp;?",
              inserer_SMILES.moodle( "c1ccccc1" ),
              inserer_SMILES.moodle( "C1CCCCC1" ) )

  # On termine le questionnaire
  finir_xml.moodle( )

  # Importez le fichier benzene.xml dans Moodle pour voir le résultat...
  print( "Importez le fichier benzene.xml dans Moodle pour voir le résultat..." )
}
}
