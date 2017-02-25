\name{question.moodle}
\alias{question.moodle}
\title{
  Créer une question Moodle au format XML
}
\description{
  Cette fonction permet de créer une question pour Moodle au format
  XML.}
\usage{
question.moodle( type = "cloze",
                 titre = "Question...", texte, reponses = NULL,
                 penalite = NA, note = NA, commentaire.global = NA,
                 autres.codes = NULL,
                 fichier.xml = get( "fichier.xml", envir = SARP.Moodle.env ) )
}
\arguments{
  \item{texte}{ Le texte de la question (une chaîne de caractères). Cet
    énoncé peut contenir n'importe quel caractère (codage en UTF-8) et
    n'importe quelles balises HTML. Il correspond à l'élément entre les
    balises \code{<questiontext>} et \code{</questiontext>} de la
    question. C'est le seul argument obligatoire. }

  \item{type}{ Le type de question à créer (élément \code{type} de la
    balise \code{<question>}). Vecteur contenant une seule chaîne de
    caractères.}

  \item{titre}{ L'intitulé de la question, en HTML. Il correspond à
    l'élément entre les balises \code{<name>} et \code{</name>} de la
    question. Indiquez \code{NULL} pour ne pas donner de titre. }

  \item{reponses}{ Un vecteur. Chaque élément correspond à une réponse
    possible, c'est-à-dire à un champ délimité par les balises
    \code{<answer>} et \code{</answer>}. \emph{Attention}, les valeurs
    de de ce vecteur sont indiquées telles qu'elles dans le fichier XML,
    sans être protégées : n'utilisez des formats particuliers qu'avec
    précaution, ou en les protégeant manuellement par une balise
    \code{<![CDATA[...]]>} --- ce que font les fonctions de création de
    question prédéfinies, si nécessaire.

    Pour indiquer des commentaires (champs \code{<feedback>}), créez un
    vecteur de chaînes de caractères de même longueur que celui des
    réponses et associez-le aux réponses comme attribut, de nom
    \code{commentaire}.  Utilisez \code{NA} pour laisser un commentaire
    vide.

    Par défaut, la première réponse est considérée comme parfaitement
    correcte (100\% de la note) et toutes les autres comme fausses (0\%).
    Pour indiquer des fractions pour chaque note, créez un vecteur
    numérique de même longueur que le nombre de réponses et associez-le
    aux réponses comme attribut, de nom \code{fractions}.  }

  \item{commentaire.global}{ Un vecteur de chaînes de caractère, de
    longueur 1, correspondant au retour à indiquer globalement (champ
    délimité par les balises \code{<generalfeedback>} et
    \code{</generalfeedback>} ).  Utilisez \code{NA} pour ne pas
    indiquer de commentaire. }

  \item{note}{ La note associée à cette question (champ délimité par les
    balises \code{<defaultgrade>} et \code{</defaultgrade>} ). Doit être
    un vecteur numérique avec un seul élément. Utilisez \code{NA} pour
    ne pas indiquer de note. }

  \item{penalite}{ La pénalité à utiliser en cas de tentatives
    multiples, si le premier essai est incorrect (champ délimité par les
    balises \code{<penalty>} et \code{</penalty>} ). Doit être un
    vecteur numérique avec un seul élément. Utilisez \code{NA} pour ne
    pas indiquer de pénalité. }

  \item{autres.codes}{ Un vecteur de chaînes de caractère, correspondant
    aux valeurs à donner à des champs optionnels de la question. Ils sont
    écrits dans l'ordre. Le nom des champs est déterminé à partir du nom
    des éléments du vecteur. }

  \item{fichier.xml}{ Le fichier correspondant au questionnaire qui
    contiendra cette question (par défaut, le dernier créé).  }
}

\details{ Cette fonction est la fonction principale, appelée par toutes
  les autres fonctions de création d'une question Moodle. Les autres
  fonctions étant d'usage plus simple, n'utilisez cette fonction que pour
  des cas non-prévus par les fonctions spécialisées.
}

\value{ Aucune valeur n'est renvoyée par cette fonction.  }

\author{ Emmanuel Curis \email{emmanuel.curis@parisdescartes.fr} }

\seealso{\code{\link{numerique.moodle}}, \code{\link{qcm.moodle}}
  \code{\link{qroc.moodle}}, \code{\link{question_libre.moodle}},
  \code{\link{question_ouverte.moodle}}, \code{\link{vrai_faux.moodle}}
}
\examples{
# Commencer un nouveau questionnaire
q <- debuter_xml.moodle( "exemple.question" )

# Catégorie : exemples
categorie.moodle( "Exemples SARP/Question" )

# Une question vrai-faux (en utilisant le type binaire)
reponses <- c( "Vrai", "Faux" )
attr( reponses, "commentaire" ) <- c( "Bravo !", "Elle n'est pas plate" )
attr( reponses, "fractions" ) <- c( 100, 0 )
question.moodle( type  = "truefalse", titre = "Question binaire",
                 texte = "La Terre est ronde", reponses = reponses )

# On termine le questionnaire
finir_xml.moodle( )

# Importez le fichier exemple.question.xml dans Moodle pour voir le résultat...
print( "Importez le fichier exemple.question.xml dans Moodle pour voir le résultat..." )
}
