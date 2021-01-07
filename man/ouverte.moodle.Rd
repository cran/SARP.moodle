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
                         editeur = c( 'WYSIWIG', 'WYSIWIG+',
                                      'Texte', 'Chasse fixe', 'Aucun' ),
                         avec.texte = TRUE, n.lignes = 15, 
                         n.annexes = 0, n.optionnelles = min( n.annexes, 3 ),
                         types = 'PDF',
                         modele = NULL, informations = NULL,
                         commentaire.global = NA, penalite = NA, note.question = NA,
                         idnum = NA,
                         temps,
                         fichier.xml = get( "fichier.xml", envir = SARP.Moodle.env ) )
}
\arguments{
  \item{texte}{ L'énoncé de la question (une chaîne de caractères). Cet
  énoncé peut contenir n'importe quel caractère (codage en UTF-8) et
  n'importe quelles balises HTML.  }

  \item{titre}{ L'intitulé de la question, en HTML. }

  \item{editeur}{ Le type d'éditeur de texte à offrir à l'étudiant pour
    qu'il puisse rédiger sa réponse. \dQuote{WYSIWIG+} correspond à un
    éditeur offrant la possibilité de choisir des fichiers. }

  \item{avec.texte}{ Si \code{TRUE}, il est obligatoire de répondre à la
    question en mettant au moins quelques mots. Sinon, il est possible
    de répondre en ne remplissant rien. }

  \item{n.lignes}{ Le nombre de lignes de texte que doit afficher
    l'éditeur, par défaut. }

  \item{n.annexes}{ Le nombre maximal de fichiers que l'étudiant peut
    téléverser pour compléter sa réponse. }

  \item{n.optionnelles}{ Parmi ces fichiers, combien sont-ils
    obligatoires? }

  \item{types}{ Les formats de fichiers autorisés, pour le dépôt. Pour
    l'instant ignoré, car il semble que cette information ne soit pas
    exportée dans le fichier XML... }

  \item{modele}{ Un modèle de réponse, à préafficher dans l'éditeur. }

  \item{informations}{ Des informations qui seront affichées pour le
    relecteur, afin de le guider dans la correction par exemple. }

  \item{commentaire.global, penalite, note.question, idnum}{Options
    communes à toutes les questions Moodle, voyez
    \code{\link{debut_question.moodle}}.}

  \item{temps}{ Le temps conseillé pour répondre à la question, en
    minutes. }

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
