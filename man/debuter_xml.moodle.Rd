\name{debuter_xml.moodle}
\alias{debuter_xml.moodle}
\alias{finir_xml.moodle}
\title{
  Commencer et terminer un questionnaire Moodle
}
\description{
  Ces deux fonctions permettent de commencer et de terminer la
  génération d'un questionnaire Moodle au format XML.
}
\usage{
debuter_xml.moodle( fichier.xml,
                    n.chiffres = 2, dec = ",",
                    racine = 2004197487,
                    glossaire = FALSE )

finir_xml.moodle(fichier.xml = get( "fichier.xml", envir = SARP.Moodle.env ))
}
\arguments{
  \item{fichier.xml}{
    Pour \code{debuter_xml.moodle}, le nom du fichier qui contiendra le
    questionnaire moodle au format XML. L'extension .xml est
    automatiquement rajoutée, sauf si elle est déjà présente dans ce
    nom.

    Pour \code{finir_xml.moodle}, le fichier correspondant au
    questionnaire à terminer (par défaut, le dernier créé).
  }
  \item{dec}{
    Le séparateur décimal à utiliser dans les textes. Par défaut, la
    virgule (comme utilisé en français). Il est modifié par \code{options}.
  }
  \item{n.chiffres}{
    Le nombre de décimales à utiliser pour les sorties au format
    XML.  Attention, cela n'affecte que les valeurs numériques
    explicitement converties en texte par un appel à la fonction
    \code{afficher_nombre.moodle}.
  }
  \item{racine}{
    La racine (graine) à utiliser pour le générateur de nombres
    aléatoires, afin d'avoir une génération de questionnaires
    reproductible. Utilisez \code{NA} pour ne pas définir la graine.
  }
  \item{glossaire}{
    Si \code{TRUE}, le fichier XML créé est au format permettant de
    construire un glossaire. Sinon, c'est le format XML moodle
    « classique » qui est utilisé.
  }
}
\details{ Lorsque l'on débute un questionnaire avec
  \code{debuter_xml.moodle}, le fichier XML est créé avec son
  en-tête. Le descripteur de fichier est mémorisé dans l'environnement
  de la bibliothèque : toutes les fonctions ultérieures utiliseront ce
  fichier par défaut pour écrire les questions créées. Attention, en cas
  d'appels multiples pour travailler sur plusieurs questionnaires en
  parallèle, seul le dernier fichier créé est mémorisé.
}
\value{ La fonction \code{debuter_xml.moodle} renvoie un descripteur de
  fichier correspondant au questionnaire nouvellement créé. Ce
  descripteur peut être utilisé par toutes les fonctions de la
  bibliothèque, mais aussi par la fonction \code{\link{cat}} de R
  directement, si nécessaire.

  La fonction \code{finir_xml.moodle} ne renvoie rien.
}
\author{
  Emmanuel Curis \email{emmanuel.curis@parisdescartes.fr}
}
%\note{
%%  ~~further notes~~
%}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{ \code{\link{options}( OutDec )} pour le choix du séparateur
  décimal à l'affichage.
}
\examples{
# Commencer un nouveau questionnaire
q <- debuter_xml.moodle( "exemple" )

# Une question Moodle basique
vrai_faux.moodle( "Cette question a deux réponses" )

# On termine le questionnaire
finir_xml.moodle( )

# Importez le fichier exemple.xml dans Moodle pour voir le résultat...
print( "Importez le fichier exemple.xml dans Moodle pour voir le résultat..." )
}
%% \keyword{ ~kwd1 }
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
