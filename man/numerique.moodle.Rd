\name{numerique.moodle}
\alias{numerique.moodle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Créer une question à réponse numérique simple
}
\description{
  Cette fonction permet de créer une question dont la réponse est une
  valeur numérique unique
}
\usage{
numerique.moodle( texte, bonne.reponse, etendue = NULL,
                  n.decimales = get( "nombre.chiffres", envir = SARP.Moodle.env ),
                  titre = "Question num&eacute;rique...",
                  fichier.xml = get( "fichier.xml", envir = SARP.Moodle.env ),
                  commentaire.global = NA )
}
\arguments{
  \item{texte}{ L'énoncé de la question (une chaîne de caractères). Cet
  énoncé peut contenir n'importe quel caractère (codage en UTF-8) et
  n'importe quelles balises HTML.  }

  \item{bonne.reponse}{ La valeur numérique correspondant à la bonne
  réponse. }

  \item{etendue}{Actuellement inutilisé}

  \item{n.decimales}{ Le nombre de décimales à conserver pour la bonne
  réponse (utiliser \code{NA} pour ne pas arrondir); par défaut, la
  valeur indiquée à la création du questionnaire est utilisée. }

  \item{titre}{ L'intitulé de la question, en HTML. }

  \item{fichier.xml}{ Le fichier correspondant au questionnaire qui
    contiendra cette question (par défaut, le dernier créé).  }

  \item{commentaire.global}{Le commentaire global affiché après avoir
  répondu à la question. Il n'y en aura pas s'il vaut \code{NA},
  \code{NULL} ou est manquant.}
}
\details{ Dans la version actuelle, la tolérance n'est pas prise en
compte. La bonne réponse doit donc être donnée exactement: n'oubliez pas
d'arrondir la valeur au bon nombre de décimales, soit en utilisant
l'option \code{n.decimales}, soit avant d'appeler la fonction.

L'arrondi est fait avec la fonction \code{\link{round}} de R. Dans ce
cas, un message est automatiquement ajouté à la fin du texte de la
question, précisant le nombre de décimales attendu. Ce texte est sur sa
propre ligne (précédé d'une balise HTML \code{<br />}) et en italique
(balises HTML \code{<i>} et \code{<i/>}).

Cette fonction est une interface simplifiée, et enrichie, pour la
fonction \code{\link{question.moodle}}.
}
\value{
Aucune valeur n'est renvoyée par cette fonction.
}
\author{
  Emmanuel Curis \email{emmanuel.curis@parisdescartes.fr}
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{ \code{\link{qcm.moodle}}, \code{\link{qroc.moodle}},
\code{\link{question_ouverte.moodle}}, \code{\link{vrai_faux.moodle}} }
\examples{
# Commencer un nouveau questionnaire
q <- debuter_xml.moodle( "exemple.numerique" )

# Catégorie : exemples
categorie.moodle( "Exemples SARP/Numérique" )

# Une question Moodle numérique, avec 3 décimales
numerique.moodle( "Donnez la valeur de &pi;", pi, n.decimales = 3 )

# On termine le questionnaire
finir_xml.moodle( )

# Importez le fichier exemple.numerique.xml dans Moodle pour voir le résultat...
print( "Importez le fichier exemple.numerique.xml dans Moodle pour voir le résultat..." )
}
