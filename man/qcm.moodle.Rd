\name{qcm.moodle}
\alias{qcm}
\alias{qcm.moodle}
\title{
  Créer une question à choix (réponse unique ou réponses multiples)
}
\description{
  Cette fonction permet de créer une question de type QCM ou QCU, dans
  laquelle la ou les bonnes réponses sont à cocher dans une liste de
  réponses prédéfinie.
}
\usage{
qcm.moodle( texte, bonnes.reponses, mauvaises.reponses,
            commentaires = NULL, fractions = list( "Bonnes" = NULL, "Fausses" = NULL ),
            unique = ( length( bonnes.reponses ) == 1 ), melanger = TRUE,
            titre = "QCM...",
            numerotation = c( "none", "abc", "ABCD", "123" ),
            fichier.xml = get( "fichier.xml", envir = SARP.Moodle.env ),
            commentaire.global = NA )
}
\arguments{
  \item{texte}{ L'énoncé de la question (une chaîne de caractères). Cet
  énoncé peut contenir n'importe quel caractère (codage en UTF-8) et
  n'importe quelles balises HTML.  }

  \item{bonnes.reponses}{ Un vecteur de chaînes de caractères
  correspondant aux intitulés des bonnes réponses. Il doit y avoir au
  moins une bonne réponse. }

  \item{mauvaises.reponses}{ Un vecteur de chaînes de caractères
  correspondant aux intitulés des mauvaises réponses. }

  \item{commentaires}{ Un vecteur de chaînes de caractère, correspondant
  au retour à indiquer pour chacune des réponses, dans l'ordre dans
  lequel elles sont passées, en commençant par les bonnes réponses.
  Utilisez \code{NA} pour laisser un commentaire vide. }

  \item{fractions}{ Une liste de deux éléments, contenant les
  pourcentage de note associés à chaque réponse. Ces deux éléments
  doivent être intitulés \code{Bonnes} (pour les bonnes réponses) et
  \code{Fausses} (pour les mauvaises réponses) ; s'ils sont indiqués, ce
  doivent être des vecteurs numériques ayant le même nombre d'éléments
  que la liste des réponses.  Par défaut (si l'élément vaut
  \code{NULL}), chaque bonne réponse est affectée de la même fraction de
  note, de sorte que l'on atteigne 100 \% en cochant toutes les bonnes
  réponses. De la même façon, les mauvaises réponses sont affectées
  d'une fraction négative de sorte que cocher toutes les mauvaises
  réponses annule le fait de cocher toutes les bonnes réponses, sans
  donner une note négative.

  Les fractions doivent être comprises entre 0 (exclus) et 100 pour les
  bonnes réponses ; elles doivent être négatives pour les mauvaises
  réponses. Si ce n'est pas le cas, ou si la somme des fractions des
  bonnes réponse ne vaut pas 100, un avertissement est donné, mais la
  question est tout de même créée. Toutefois, si toutes les fractions
  des mauvaises réponses sont positives, elles sont considérées
  négatives sans avertissement.}

  \item{unique}{ Si \code{TRUE}, indique que l'on ne peut cocher qu'une seule bonne réponse. }

  \item{melanger}{ Si \code{TRUE}, indique que l'ordre des réponses doit
    être choisi aléatoirement à chaque affichage de la question. Sinon,
    l'ordre est celui défini dans le fichier. Actuellement, cela
    correspond à l'ensemble des bonnes réponses dans l'ordre indiqué,
    suivi de l'ensemble des mauvaises réponses dans l'ordre indiqué.}

  \item{titre}{ L'intitulé de la question, en HTML. }

  \item{numerotation}{ Le type de numérotation à utiliser pour les
    différentes réponses. Ces numéros sont ajoutés par Moodle lors de
    l'affichage de la question. Seules les quatre valeurs indiquées sont
    reconnues par Moodle. Vous pouvez abréger en n'utilisant que la
    première lettre. }

  \item{fichier.xml}{ Le fichier correspondant au questionnaire qui
    contiendra cette question (par défaut, le dernier créé).  }
  
  \item{commentaire.global}{ Un vecteur de chaînes de caractère, de longueur
    1, indiquant le commentaire global à afficher après que l'étudiant a
    répondu. }
}

\details{ La question générée avec cette fonction est une question de
    type \code{multichoice}.
    
    Cette fonction est une interface simplifiée pour la
    fonction \code{\link{question.moodle}}.
}
\value{
  Aucune valeur n'est renvoyée par cette fonction.
}
\author{
  Emmanuel Curis \email{emmanuel.curis@parisdescartes.fr}
}

\seealso{\code{\link{numerique.moodle}},
  \code{\link{qroc.moodle}}, \code{\link{question_ouverte.moodle}}, \code{\link{vrai_faux.moodle}}
}
\examples{
\dontrun{
# Commencer un nouveau questionnaire
q <- debuter_xml.moodle( "exemple.QCM" )

# Catégorie : exemples
categorie.moodle( "Exemples SARP/QCM" )

# Le format par défaut, avec deux bonnes réponses et trois mauvaises,
qcm.moodle( "Quels sont les fruits de la liste ci-dessous&thinsp;?",
            c( "Orange", "Tomate" ), c( "Pomme de terre", "Carotte", "Navet" ) )

# Deux bonnes réponses, deux mauvaises réponses, avec commentaires
#   et fractions inégales
qcm.moodle( "Indiquez les romanciers de la liste ci-dessous.",
            c( "Eugène&nbsp;S<small>UE</small>", "Alexandre&nbsp;D<small>UMAS</small>" ),
            c( "Labiche", "Ronsard" ),
            commentaires = c( "Bravo&thinsp;!", NA,
                 "C'est un auteur de pièces de théâtre", "C'est un poète" ),
            fractions = list( "Bonnes"  = c( 75, 25 ),
                              "Fausses" = c( -50, -100 ) ) )

# On termine le questionnaire
finir_xml.moodle( )

# Importez le fichier exemple.QCM.xml dans Moodle pour voir le résultat...
print(
"Importez le fichier exemple.QCM.xml dans Moodle pour voir le résultat..." )
}
}
