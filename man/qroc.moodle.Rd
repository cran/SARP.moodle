\name{qroc.moodle}
\alias{qroc.moodle}
\alias{qroc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Créer une question à réponse ouverte, courte
}
\description{
  Cette fonction permet de créer une question dont la réponse est un
  court texte (« QROC »)
}
\usage{
qroc.moodle( texte, reponses, notes = rep( 100, length( reponses ) ),
             commentaires = NULL, casse = TRUE,
             titre = "QROC...",
             fichier.xml = get( "fichier.xml", envir = SARP.Moodle.env ),
             commentaire.global = NA, penalite = NA, note.question = NA,
             idnum = NA,
             temps )
}
\arguments{
  \item{texte}{ L'énoncé de la question (une chaîne de caractères). Cet
  énoncé peut contenir n'importe quel caractère (codage en UTF-8) et
  n'importe quelles balises HTML.  }

  \item{reponses}{ Un vecteur de chaînes de caractères, dont chaque
  élément correspond à une réponse correcte possible. }

  \item{notes}{ Un vecteur de nombres, correspondant aux pourcentages de
  la note totale associés à chaque réponse. Par défaut, toutes les
  réponses sont supposées parfaitement correctes.}

  \item{commentaires}{ Un vecteur de chaînes de caractères, correspondant
  au retour à afficher pour chacune des réponses entrées (dans le même
  ordre que les réponses). Le vecteur doit avoir la même longueur ; si
  certaines réponses n'appellent pas de commentaire, utilisez \code{NA}.}

  \item{casse}{Si \code{TRUE}, on distingue entre majuscules et
  minuscules pour savoir si la réponse est correcte. Sinon, cette
  distinction n'est pas faite.}

  \item{titre}{ L'intitulé de la question, en HTML. }

  \item{fichier.xml}{ Le fichier correspondant au questionnaire qui
    contiendra cette question (par défaut, le dernier créé).  }

  \item{commentaire.global, penalite, note.question, idnum}{Options
    communes à toutes les questions Moodle, voyez
    \code{\link{debut_question.moodle}}.}

  \item{temps}{ Le temps conseillé pour répondre à la question, en
    minutes. }
}
\details{ 
Cette fonction est une interface simplifiée, et enrichie, pour la
fonction \code{\link{question.moodle}}.
}
\value{
Aucune valeur n'est renvoyée par cette fonction.
}
\author{
  Emmanuel Curis \email{emmanuel.curis@parisdescartes.fr}
}

\seealso{\code{\link{numerique.moodle}}, \code{\link{qcm.moodle}},
  \code{\link{question_ouverte.moodle}}, \code{\link{vrai_faux.moodle}}
}
\examples{
\dontrun{
# Commencer un nouveau questionnaire
q <- debuter_xml.moodle( "exemple.qroc" )

# Catégorie : exemples
categorie.moodle( "Exemples SARP/QROC" )

# Une question Moodle en QROC, avec une seule bonne réponse
qroc.moodle( "Quel est le prénom de Victor&nbsp;H<small>UGO</small>&thinsp;?",
             "Victor" )

# La même, en indiquant que l'initiale doit être en majuscule
qroc.moodle( "Quel est le prénom de Victor&nbsp;H<small>UGO</small>&thinsp;?",
             c( "Victor", "victor" ),
             notes = c( 100, 50 ),
             commentaires = c( NA, "N'oubliez pas la majuscule aux noms propres&thinsp;!" ) )

# On termine le questionnaire
finir_xml.moodle( )

# Importez le fichier exemple.numerique.xml dans Moodle pour voir le résultat...
print(
"Importez le fichier exemple.numerique.xml dans Moodle pour voir le résultat..." )
}
}
