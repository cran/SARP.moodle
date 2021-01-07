\name{glisser_textes.moodle}
\alias{glisser_texte}
\alias{glisser_textes}
\alias{glisser_textes.moodle}
\title{
  Créer une question demandant de replacer des mots dans un texte
}
\description{
  Cette fonction permet de créer une question dans laquelle il faut
  faire glisser des mots pour les replacer dans un texte à trous.
}
\usage{
glisser_textes.moodle( texte, titre = "Glisser les textes...",
                       groupe = rep( 1, n.zones ),
                       infini = FALSE,
                       ordre.aleatoire = TRUE, afficher.erreurs = TRUE,
                       commentaire.global = NA, 
                       penalite = NA, note.question = NA, idnum = NA,
                       temps,
                       fichier.xml = get( "fichier.xml",
                                          envir = SARP.Moodle.env ) )
}
\arguments{
  
  \item{texte}{ L'énoncé de la question (une chaîne de caractères). Cet
  énoncé peut contenir n'importe quel caractère (codage en UTF-8) et
  n'importe quelles balises HTML. Les trous à faire dans ce texte
  doivent être identifiés par des doubles-crochets (voir les détails). }

  \item{titre}{ L'intitulé de la question, en HTML. }

  \item{groupe}{ Pour chaque élément à replacer, le groupe auquel il
    appartient (voir les détails). L'ordre est l'ordre d'apparition dans
    le texte.}

  \item{infini}{ Pour chaque élément à replacer, indique s'il peut être
    réutilisé (\code{TRUE}) ou s'il disparaît des éléments une fois
    placé quelque part (\code{FALSE}). L'ordre est l'ordre d'apparition
    dans le texte. Si une seule valeur est indiquée, elle sera utilisée
    pour tous les éléments. Sinon, il faut indiquer autant de valeurs
    que d'éléments. }

  \item{ordre.aleatoire}{ Les éléments sont par défaut proposés dans
    l'ordre de leur numéro. Ce numéro est ici imposé à l'ordre
    d'apparition dans le texte. Cette option permet, si elle vaut
    \code{TRUE} de permuter aléatoirement ces numéros.}

  \item{afficher.erreurs}{ Si \code{TRUE}, en cas de réponse incorrecte
    à la question, les réponses erronées sont identifiées.}
  
  \item{commentaire.global, penalite, note.question, idnum}{Options
    communes à toutes les questions Moodle, voyez
    \code{\link{debut_question.moodle}}.}

  \item{temps}{ Le temps conseillé pour répondre à la question, en
    minutes. }

  \item{fichier.xml}{ Le fichier correspondant au questionnaire qui
    contiendra cette question (par défaut, le dernier créé).  }
}

\details{
  Cette question permet de proposer un texte dans lequel certains
  éléments sont remplacés par un cadre vide, dans lequel doivent être
  glissés des éléments.

  Dans l'énoncé fourni, les éléments qui devront être remplacés par des
  cadres vides doivent être entourés par des doubles crochets — par
  exemple, le texte \dQuote{Le roi [[Henri IV]] de France et de Navarre}
  apparaîtra dans Moodle avec un trou à la place de \dQuote{[[Henri
  IV]]} et, sous le texte, une étiquette intitulée \dQuote{Henri IV},
  qu'il faudra replacer.

  Les différents éléments à replacer peuvent être organisés en groupes,
  qui se distinguent par une couleur légèrement différente et une taille
  des étiquettes différentes.
  
  Afin de ne pas donner d'indication, les trous (et leurs étiquettes)
  auront tous la même taille au sein d'un groupe, choisie pour que le
  plus long texte tienne dans l'étiquette.
}


\value{
  Aucune valeur n'est renvoyée par cette fonction.
}
\author{
  Emmanuel Curis \email{emmanuel.curis@parisdescartes.fr}
}

\seealso{\code{\link{glisser_deposer.moodle}} et
  \code{\link{legender_image.moodle}} pour faire glisser des étiquettes
  sur une image.  }
