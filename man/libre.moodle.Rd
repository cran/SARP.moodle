\name{libre.moodle}
\alias{libre.moodle}
\alias{question_libre.moodle}
\title{
  Créer une question libre (« cloze », texte à compléter)
}
\description{
  Cette fonction permet de créer une question dont le format est libre,
  qui se présente sous forme d'un texte à trous — format dit « cloze ».
}
\usage{
question_libre.moodle( texte.intro, textes.avant, texte.final,
                       reponses, notes = NULL, types = NULL,
                       commentaires = NULL,
                       titre = "Question libre",
                       commentaire.global = NA,
                       fichier.xml = get( "fichier.xml",
                                          envir = SARP.Moodle.env ) )
}
\arguments{
  \item{texte.intro}{ L'introduction de la question (une chaîne de caractères). Cet
  énoncé peut contenir n'importe quel caractère (codage en UTF-8) et
  n'importe quelles balises HTML. Il se trouvera au tout début de la question. }

  \item{textes.avant}{ Un vecteur de chaîne de caractères. À chaque
  élément de ce vecteur sera associé un champ de réponse, qui sera
  \emph{précédé} du texte contenu dans cette chaîne de caractères. Ces
  textes peuvent contenir n'importe quel caractère (codage en UTF-8) et
  n'importe quelles balises HTML. Le nombre d'éléments du vecteur
  doit être égal au nombre d'éléments de \code{reponse}. }

  \item{texte.final}{ Le texte qui doit se placer après le dernier champ
  de réponse de la question. Il peut contenir n'importe quel caractère (codage en UTF-8) et
  n'importe quelles balises HTML. }

  \item{reponses}{ Une liste dont chaque élément décrit la ou les
  réponses attendues pour chaque « trou » du texte. Le nombre d'éléments
  de cette liste définit le nombre de champs réponses dans la question
  et doit être égal à la longueur du vecteur \code{textes.avant}.
  Consultez les détails pour plus d'information. }

  \item{notes}{ Le vecteur des notes associées à chaque réponse. S'il
  est fourni, il doit être de même longueur que \code{reponses}.}

  \item{types}{ Le vecteur des types de champ réponse. S'il est fourni,
  il doit être de même longueur que \code{reponses}. Les types possibles
  sont les textes \code{NUMERICAL} (par défaut), \code{MULTICHOICE},
  \code{MULTICHOICE_V}, \code{MULTICHOICE_H}, \code{SHORTANSWER} et
  \code{SHORTANSWER_C}.  Consultez les détails pour plus d'information.}

  \item{commentaires}{ Une liste de commentaires associés aux réponses
  de chaque champ réponse. Si elle est fournie, elle doit être de même
  longueur que \code{reponses}. Consultez les détails pour plus
  d'information.}

  \item{titre}{ L'intitulé de la question, en HTML. }

  \item{commentaire.global}{Le commentaire global affiché après avoir
  répondu à la question. Il n'y en aura pas s'il vaut \code{NA},
  \code{NULL} ou est manquant.}
  
  \item{fichier.xml}{ Le fichier correspondant au questionnaire qui
    contiendra cette question (par défaut, le dernier créé).  }
}

\details{ Cette fonction construit, à partir des éléments fournis, une
question de type « cloze » : un texte à trous, les réponses étant à
entrer dans les trous ou à choisir dans une liste. C'est donc une suite
de « champs réponses », séparés par des textes.

Chaque champ réponse est défini par la ou les réponses possibles, son
  type, sa note et éventuellement les commentaires renvoyés à l'étudiant
  en fonction de ses réponses. Ces informations sont obtenues à partir
  des paramètres ci-dessus, le \emph{i}-ème élément étant utilisé pour
  le \emph{i}-ème champ réponse.

Le type \code{NUMERICAL} correspond à un champ attendant une réponse
 numérique — c'est l'équivalent d'une question numérique. Une seule
 réponse est possible, qui sera la bonne réponse. Il ne peut donc y
 avoir aussi qu'un seul commentaire.

Les types \code{MULTICHOICE}, \code{MULTICHOICE_V},
 \code{MULTICHOICE_H}, \code{SHORTANSWER} et \code{SHORTANSWER_C}
 correspondent à un champ permettant de choisir (cocher) une ou
 plusieurs réponses dans une liste prédéfinie — c'est l'équivalent d'un
 QCM. Ces cinq types fonctionnent de façon similaire, seule la façon
 d'afficher la question varie (elles donnent respectivement un menu
 déroulant, une série de boutons verticaux, une série de boutons
 horizontaux et un champ de texte libre insensible ou sensible à la
 casse).  Dans les cinq cas, \code{reponses} doit contenir pour ces
 champs une liste de deux éléments, l'un contenant le vecteur des textes
 des réponses (élément nommé \code{Textes}), le second indiquant pour
 chaque texte s'il correspond à une réponse correcte (\code{TRUE}) ou
 non (\code{FALSE}). S'il y a plusieurs réponses correctes, chacune
 donne une fraction égale des points. Les autres cas de réponses donnant
 une partie des points ne sont pas encore possibles avec cette
 fonction. Les commentaires de chaque réponse sont à donner dans un
 vecteur de même longueur que \code{reponses$Textes}; utilisez \code{NA}
 pour une réponse sans commentaire.

 Une fois le texte complet de la question créé, au format « cloze », à
 partir de ces éléments, la question est créée grâce à la fonction
 \code{\link{question.moodle}.}
}

\value{
Aucune valeur n'est renvoyée par cette fonction.
}
\author{
  Emmanuel Curis \email{emmanuel.curis@parisdescartes.fr}
}
\seealso{\code{\link{numerique.moodle}}, \code{\link{qcm.moodle}},
  \code{\link{qroc.moodle}}, \code{\link{question_ouverte.moodle}}, \code{\link{vrai_faux.moodle}}
}
\examples{
# Commencer un nouveau questionnaire
q <- debuter_xml.moodle( "exemple.libre" )

# Catégorie : exemples
categorie.moodle( "Exemples SARP/Question libre" )

# Une question Moodle libre avec champs numériques
question_libre.moodle( "Indiquez les chiffres du nombre 1974.",
                       list( "Unité : ", " — dizaine : ",
                             " — centaine : ", " — milliers : " ), ".",
                       reponses = list( 4, 7, 9, 1 ) )

# Avec un champ numérique et des champs à choix multiples
question_libre.moodle( "<cite>Le Corbeau et le Renard</cite>.",
                       list( "<i>Maître ",
                             " sur son arbre perché<br />Tenait en son bec un ",
                             ".<br />Maître ",
                             "</i>par l'odeur alléché<br />... est un poème composé en " ),
                       " par le poète Jean de la Fontaine.",
                       types = c( "MULTICHOICE", "MULTICHOICE_H",
                                  "SHORTANSWER", "NUMERICAL" ),
                       reponses = list( list( "Textes"  = c( "Renard", "Goupil",
                                                             "Corbeau", "Corneille" ),
                                              "Correct" = c( FALSE, FALSE, TRUE, FALSE ) ),
                                        list( "Textes"  = c( "Fromage", "Camembert",
                                                             "Bleu", "Livre" ), 
                                              "Correct" = c( TRUE, FALSE, FALSE, FALSE ) ),
                                        list( "Textes"  = c( "Renard", "Goupil",
                                                             "Corbeau", "Corneille" ),
                                              "Correct" = c( TRUE, FALSE, FALSE, FALSE ) ),
                                        1668 ) )

# On termine le questionnaire
finir_xml.moodle( )

# Importez le fichier exemple.libre.xml dans Moodle pour voir le résultat...
print( "Importez le fichier exemple.libre.xml dans Moodle pour voir le résultat..." )
}
