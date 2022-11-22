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
                       penalite = NA, note.question = NA, idnum = NA,
                       temps,
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
  doit être égal au nombre d'éléments de \code{reponses}. }

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
  \code{MULTICHOICE_V}, \code{MULTICHOICE_H}, \code{MULTICHOICE_S},
  \code{MULTICHOICE_VS}, \code{MULTICHOICE_HS}, \code{MULTIRESPONSE},
  \code{MULTIRESPONSE_S}, \code{MULTIRESPONSE_H},
  \code{MULTIRESPONSE_S}, \code{SHORTANSWER} et \code{SHORTANSWER_C}.
  Consultez les détails pour plus d'information.}

  \item{commentaires}{ Une liste de commentaires associés aux réponses
  de chaque champ réponse. Si elle est fournie, elle doit être de même
  longueur que \code{reponses}. Consultez les détails pour plus
  d'information.}

  \item{titre}{ L'intitulé de la question, en HTML. }

  \item{commentaire.global, penalite, note.question, idnum}{Options
    communes à toutes les questions Moodle, voyez
    \code{\link{debut_question.moodle}}.}

  \item{temps}{ Le temps conseillé pour répondre à la question, en
    minutes. }
  
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
 numérique — c'est l'équivalent d'une question numérique. Une ou
 plusieurs réponses sont possibles, qui seront de bonnes réponses, mais
 pas forcément donnant toute tous les points. Les réponses sont à
 fournir sous la forme d'une liste comportant un vecteur des valeurs,
 nommé \code{Valeur} et un vecteur des tolérances associées, nommé
 \code{Tolerance}.

Les types \code{MULTICHOICE}, \code{MULTICHOICE_V},
 \code{MULTICHOICE_H}, \code{MULTICHOICE_S}, \code{MULTICHOICE_VS},
 \code{MULTICHOICE_HS}, \code{MULTIRESPONSE}, \code{MULTIRESPONSE_S},
 \code{MULTIRESPONSE_S}, \code{MULTIRESPONSE_HS}, \code{SHORTANSWER} et
 \code{SHORTANSWER_C} correspondent à un champ permettant de choisir
 (cocher) une ou plusieurs réponses dans une liste prédéfinie — c'est
 l'équivalent d'un QCM. Tous ces types fonctionnent de façon similaire,
 seuls la façon d'afficher la question et le nombre de réponses
 sélectionnables varient (voyez le tableau récapitulatif ci-dessous).
 Dans tous les cas, \code{reponses} doit contenir pour ces champs une
 liste de deux éléments, l'un contenant le vecteur des textes des
 réponses (élément nommé \code{Textes}), le second indiquant pour chaque
 texte s'il correspond à une réponse correcte (\code{TRUE}) ou non
 (\code{FALSE}). S'il y a plusieurs réponses correctes, chacune donne
 une fraction égale des points. Pour préciser la note de chaque réponse,
 placez dans la liste un élément nommé \code{Notes}, contenant le
 vecteur des notes de chaque réponse (ces notes doivent être exprimées
 en pourcentage de la note, arrondi à l'entier le plus proche, parmi les
 valeurs autorisées par Moodle) ; dans ce cas, l'élément \code{Correct}
 est ignoré et peut être omis.  Les commentaires de chaque réponse sont
 à donner dans un vecteur de même longueur que \code{reponses$Textes};
 utilisez \code{NA} pour une réponse sans commentaire.

 \tabular{rcc}{Code \tab Disposition \tab Remarque \cr
   \code{MULTICHOICE} \tab Menu déroulant \tab Une seule réponse
   possible \cr
   \code{MULTICHOICE_V} \tab Série de boutons radio verticaux \tab Une seule
   réponse \cr
   \code{MULTICHOICE_H} \tab Série de boutons radio horizontaux \tab Une seule
   réponse \cr
   \code{MULTIRESPONSE} \tab Série de cases à cocher verticales \tab Plusieurs
   réponse possibles \cr
   \code{MULTICHOICE_H} \tab Série de cases à cocher horizontales \tab Plusieurs
   réponses possibles \cr
   \code{SHORTANSWER} \tab Champ de texte libre \tab sensible à
 la casse \cr
   \code{SHORTANSWER_C} \tab Champ de texte libre \tab insensible à
   la casse \cr
 }
 Les variantes avec le suffixe S conduisent à un ordre des réponses tiré
   au sort par Moodle lors de l'affichage de la question.

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
\dontrun{
# Commencer un nouveau questionnaire
q <- debuter_xml.moodle( "exemple.libre" )

# Catégorie : exemples
categorie.moodle( "Exemples SARP/Question libre" )

# Une question Moodle libre avec champs numériques
question_libre.moodle( "Indiquez les chiffres du nombre 1974.",
                       list( "Unit&eacute; : ", " &mdash; dizaine : ",
                             " &mdash; centaine : ", " &mdash; milliers : " ), ".",
                       reponses = list( 4, 7, 9, 1 ) )

# Avec un champ numérique et des champs à choix multiples
question_libre.moodle( "<cite>Le Corbeau et le Renard</cite>.",
                       list( "<i>Ma&icirc;tre ",
                             " sur son arbre perch&eacute;<br />Tenait en son bec un ",
                             ".<br />Ma&icirc;tre ",
                             paste0( "</i>par l'odeur all&eacute;ch&eacute;<br />...",
                                     "<br />est un po&egrave;me compos&eacute; en " ) ),
                       " par le po&egrave;te Jean de la Fontaine.",
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
print(
"Importez le fichier exemple.libre.xml dans Moodle pour voir le r\u00e9sultat..." )
}
}
