\name{ouverte.moodle}
\alias{ouverte.moodle}
\alias{question_ouverte.moodle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Créer une question à réponse ouverte (rédactionnelle)
}
\description{
  Cette fonction permet de créer une question dont la réponse est libre (rédactionnelle).
}
\usage{
question_ouverte.moodle( texte, 
                         titre = "Question r&eacute;dactionnelle ouverte",
                         fichier.xml = get( "fichier.xml", envir = SARP.Moodle.env ) )
}
\arguments{
  \item{texte}{ L'énoncé de la question (une chaîne de caractères). Cet
  énoncé peut contenir n'importe quel caractère (codage en UTF-8) et
  n'importe quelles balises HTML.  }

  \item{titre}{ L'intitulé de la question, en HTML. }

  \item{fichier.xml}{ Le fichier correspondant au questionnaire qui
    contiendra cette question (par défaut, le dernier créé).  }
}
\details{ 
Cette fonction est une interface simplifiée pour la
fonction \code{\link{question.moodle}}.
}
\value{
Aucune valeur n'est renvoyée par cette fonction.
}
\author{
  Emmanuel Curis \email{emmanuel.curis@parisdescartes.fr}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{\code{\link{numerique.moodle}}, \code{\link{qcm.moodle}},
  \code{\link{qroc.moodle}}, \code{\link{vrai_faux.moodle}}
}
\examples{
\dontrun{
# Commencer un nouveau questionnaire
q <- debuter_xml.moodle( "exemple.ouverte" )

# Catégorie : exemples
categorie.moodle( "Exemples SARP/Question ouverte" )

# Une question Moodle ouverte
question_ouverte.moodle( "Racontez votre journ&eacute;e." )

# On termine le questionnaire
finir_xml.moodle( )

# Importez le fichier exemple.ouverte.xml dans Moodle pour voir le résultat...
print(
"Importez le fichier exemple.ouverte.xml dans Moodle pour voir le résultat..." )
}
}
