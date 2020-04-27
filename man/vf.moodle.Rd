\name{vf.moodle}
\alias{vrai_faux.moodle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Créer une question à deux réponses possibles, dont une seule vraie
}
\description{
  Cette fonction permet de créer une question de type vrai ou faux.
}
\usage{
vrai_faux.moodle( texte,  texte.vrai = "Vrai", texte.faux = "Faux",
                  commentaires = NULL,
                  titre = "Question vrai-faux...",
                  ordre = c( "aleatoire", NA, "random", "vrai premier", "faux premier" ),
                  melanger = FALSE,
                  fichier.xml = get( "fichier.xml", envir = SARP.Moodle.env ),
                  commentaire.global = NA )
}
\arguments{
  \item{texte}{ L'énoncé de la question (une chaîne de caractères). Cet
  énoncé peut contenir n'importe quel caractère (codage en UTF-8) et
  n'importe quelles balises HTML.  }

  \item{texte.vrai}{ Le texte à utiliser pour la bonne réponse. }
  \item{texte.faux}{ Le texte à utiliser pour la mauvaise réponse. }

  \item{commentaires}{ Un vecteur de deux chaînes de caractère,
  correspondant au retour à indiquer pour la bonne réponse (premier
  élément) et pour la mauvaise réponse (second élément). Utilisez
  \code{NA} pour laisser un commentaire vide. }

  \item{titre}{ L'intitulé de la question, en HTML. }

  \item{ordre}{ L'ordre dans lequel les deux réponses possibles doivent
    apparaître. Les trois premières possibilités décident d'un ordre
    aléatoire (fixe). Cette option n'a d'intérêt que si
    \code{melanger=FALSE}...}

  \item{melanger}{ Si \code{TRUE}, indique que Moodle devra tirer au
    sort l'ordre d'affichage des réponses (qui changera donc à chaque fois
    que la question sera posée). Sinon, l'ordre sera toujours celui
    indiqué lors de la création de la question.}

  \item{fichier.xml}{ Le fichier correspondant au questionnaire qui
    contiendra cette question (par défaut, le dernier créé).  }
  
  \item{commentaire.global}{ Un vecteur de chaînes de caractère, de longueur
    1, indiquant le commentaire global à afficher après que l'étudiant a
    répondu. }
}
\details{ Bien qu'un type particulier de question existe dans Moodle
    pour les questions « vrai ou faux » (type \code{truefalse}), la
    question générée avec cette fonction est une question de type
    \code{multichoice}, afin d'avoir la liberté des intitulés des bonnes
    et mauvaises réponses.
    
    Cette fonction est une interface simplifiée pour la
    fonction \code{\link{question.moodle}}.
}
\value{
Aucune valeur n'est renvoyée par cette fonction.
}
\author{
  Emmanuel Curis \email{emmanuel.curis@parisdescartes.fr}
}
\seealso{\code{\link{numerique.moodle}}, \code{\link{qcm.moodle}},
  \code{\link{qroc.moodle}}, \code{\link{question_ouverte.moodle}}.
}
\examples{
\dontrun{
# Commencer un nouveau questionnaire
q <- debuter_xml.moodle( "exemple.VF" )

# Catégorie : exemples
categorie.moodle( "Exemples SARP/Vrai ou Faux" )

# Le format par défaut
vrai_faux.moodle( "Une question « vrai ou faux » offre le choix entre deux réponses" )

# et si la bonne réponse est « faux »
vrai_faux.moodle( "Le Soleil tourne autour de la Terre", "Faux", "Vrai" )

# avec des intitulés libres
vrai_faux.moodle( "La bataille de Marignan a eu lieu en...", "1515", "1914" )

# On termine le questionnaire
finir_xml.moodle( )

# Importez le fichier exemple.VF.xml dans Moodle pour voir le résultat...
print(
"Importez le fichier exemple.VF.xml dans Moodle pour voir le résultat..." )
}
}
