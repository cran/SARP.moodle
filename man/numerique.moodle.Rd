\name{numerique.moodle}
\alias{numerique.moodle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Créer une question à réponse numérique simple
}
\description{
  Cette fonction permet de créer une question dont la réponse est une
  valeur numérique unique.
}
\usage{
numerique.moodle( texte, bonne.reponse, notes = 100,
                  n.decimales = get( "nombre.chiffres", envir = SARP.Moodle.env ),
                  n.significatifs = NA,
                  titre = "Question num&eacute;rique...",
                  commentaire.global = NA, penalite = NA, note.question = NA,
                  tolerance.type = 2, tolerance = "auto",
                  unites = NULL, unite.avant = FALSE,
                  unite.penalite = 0.1, unite.visible = FALSE,
                  commentaires = NULL, couleur.consigne = "Orange",
                  fichier.xml = get( "fichier.xml", envir = SARP.Moodle.env ) )
}
\arguments{
  \item{texte}{ L'énoncé de la question (une chaîne de caractères). Cet
    énoncé peut contenir n'importe quel caractère (codage en UTF-8) et
    n'importe quelles balises HTML.
  }

  \item{bonne.reponse}{ La valeur numérique correspondant à la bonne
    réponse. Depuis Moodle 3, plusieurs valeurs peuvent être
    proposées. }

  \item{notes}{Lorsque plusieurs valeurs peuvent être acceptées comme
    réponse, la note (en fraction de la note totale de la question)
    associée à chaque réponse.}

  \item{n.decimales}{ Le nombre de décimales à conserver pour la bonne
  réponse (utiliser \code{NA} pour ne pas arrondir); par défaut, la
  valeur indiquée à la création du questionnaire est utilisée. }

  \item{n.significatifs}{ Le nombre de chiffres significatifs à
  conserver pour la bonne réponse (utiliser \code{NA} pour ne pas
  arrondir); par défaut, la valeur indiquée à la création du
  questionnaire est utilisée. }

  \item{titre}{ L'intitulé de la question, en HTML. }

  \item{fichier.xml}{ Le fichier correspondant au questionnaire qui
    contiendra cette question (par défaut, le dernier créé).  }

  \item{commentaire.global, penalite, note.question}{Options communes à
    toutes les questions Moodle, voyez
    \code{\link{debut_question.moodle}}.}

  \item{tolerance.type}{La façon dont est exprimée la tolérance. Les
    valeurs possibles sont 1 (tolérance relative: la valeur de la
    tolérance est un pourcentage de la réponse), 2 (tolérance absolue)
    et 3 (tolérance « géométrique »).}

  \item{tolerance}{La tolérance associée à chaque réponse. Si une seule
    valeur est fournie, elle sera utilisée pour toutes les réponses. La
    valeur spéciale \code{"auto"} calcule une tolérance de
    \eqn{1{,}1\times10^{-n}}{1,1×10^-n}, où \eqn{n} est le nombre de
    décimales demandé pour l'arrondi si \code{n.decimales > 0}, et 0
    sinon.}

  \item{unites}{Un vecteur de multiplicateurs indiquant les conversions
    possible d'unités pour la réponse. Les noms de ce vecteurs
    définissent les unités. }

  \item{unite.avant}{Si \code{TRUE}, l'unité sera affichée et attendue
    avant la valeur numérique (pour les unités monétaires anglosaxonnes,
    typiquement). Sinon, elle sera affichée et attendue après la valeur
    numérique.}

  \item{unite.penalite}{La pénalité en cas d'erreur sur l'unité.}

  \item{unite.visible}{Si \code{TRUE}, l'unité peut être choisie dans un
  menu déroulant. Sinon, elle doit être entrée dans le champ de la
  réponse, comme un texte.}

  \item{commentaires}{Le commentaire à affiche selon la réponse
  proposée. Il doit y avoir autant de valeurs que de bonne réponses
  (certaines pouvant être \code{NA}, pour ne pas avoir de commentaire
  pour la réponse associée).}

  \item{couleur.consigne}{La couleur à utiliser pour afficher le texte
  de la consigne générée, lorsque l'on précise l'arrondi. Doit être une
  chaîne de caractères décrivant une couleur reconnaissable en CSS.}

}

\details{ En l'absence de tolérance, la bonne réponse doit être donnée
exactement: n'oubliez pas d'arrondir la valeur au bon nombre de
décimales, soit en utilisant l'option \code{n.decimales} ou
\code{n.significatifs}, soit avant d'appeler la fonction.

Si \code{n.decimales} est précisé et strictement positif, l'arrondi est
fait avec la fonction \code{\link{round}} de R. Dans ce cas, un message
est automatiquement ajouté à la fin du texte de la question, précisant
le nombre de décimales attendu. Ce texte est sur sa propre ligne
(précédé d'une balise HTML \code{<br />}), en italique (balises HTML
\code{<i>} et \code{<i/>}) et avec la couleur indiquée par
\code{couleur.consigne}.

Si \code{n.significatif} est précisé et strictement positif, l'arrondi
est fait avec la fonction \code{\link{signif}} de R. Dans ce cas, un
message est automatiquement ajouté à la fin du texte de la question,
précisant le nombre de chiffres significatifs attendu. Ce texte est sur
sa propre ligne (précédé d'une balise HTML \code{<br />}), en italique
(balises HTML \code{<i>} et \code{<i/>}) et avec la couleur indiquée par
\code{couleur.consigne}.

Lorsque vous attendez une réponse entière, pour ne pas avoir d'arrondi,
de tolérance ni de consigne associée, forcez le type de
\code{bonne.reponse} à être un entier, et pas un double (fonction
\code{\link{as.integer}} de R).

La tolérance \eqn{\tau}{t} permet d'accepter toutes les valeurs dans un
intervalle \eqn{[a,b]}, calculé à partir de la réponse \eqn{r}. Le
calcul dépend du type de tolérance demandé: pour le type 1,
\eqn{a=r-\delta}{a=r-d} et \eqn{b=r+\delta}{b=r+d}, avec
\eqn{\delta=\tau\,r}{d=tr}. Pour le type 2, \eqn{\delta=\tau}{d=t}.
Pour le type 3, \eqn{a=r/(1+\tau)}{a=r/(1+t)} et
\eqn{b=r(1+\tau)}{a=r(1+t)}.
}
\value{
Aucune valeur n'est renvoyée par cette fonction.
}
\author{
  Emmanuel Curis \email{emmanuel.curis@parisdescartes.fr}
}

\seealso{ \code{\link{qcm.moodle}}, \code{\link{qroc.moodle}},
\code{\link{question_ouverte.moodle}}, \code{\link{vrai_faux.moodle}} }

\examples{
\dontrun{
# Commencer un nouveau questionnaire
q <- debuter_xml.moodle( "exemple.numerique" )

# Catégorie : exemples
categorie.moodle( "Exemples SARP/Numérique" )

# Une question Moodle numérique, avec 3 décimales
numerique.moodle( "Donnez la valeur de &pi;", pi, n.decimales = 3 )

# On termine le questionnaire
finir_xml.moodle( )

# Importez le fichier exemple.numerique.xml dans Moodle pour voir le résultat...
print(
"Importez le fichier exemple.numerique.xml dans Moodle pour voir le résultat..." )
}
}
