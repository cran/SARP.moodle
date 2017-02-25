\name{SARP.moodle}
\alias{SARP.moodle}
\alias{SARP.moodle-package}
\docType{package}
\title{
  Création de questions Moodle au format XML avec R
}
\description{
  Cette bibliothèque permet de générer un fichier de questions pour
  Moodle, au format XML, sans avoir à connaître ce format. Les questions
  peuvent être construites à partir d'un fichier CSV ou directement à
  partir de fonctions R.
}
\details{
  Cette bibliothèque permet de faciliter la création de questions pour
  les tests Moodle : elle génère un fichier XML qui peut être importé
  dans la base de questions d'un cours Moodle. Il est ensuite possible
  de piocher dans ces questions pour créer un test.  Afin d'éviter toute
  confusion avec d'autres fonctions, toutes les fonctions de cette
  bibliothèque se terminent par le suffixe \code{.moodle}.

  L'utilisation la plus simple est la conversion d'un fichier CSV (ou
  tout autre format texte lisible par \code{\link{read.table}}) avec une
  ligne par question au format XML (\code{\link{csv.moodle}}). Mais il
  est aussi possible de créer les questions directement dans R, en
  créant le fichier XML (\code{\link{debuter_xml.moodle}}) puis en
  appelant les fonctions appropriées. Dans ce cas, il ne faut pas
  oublier de clore le fichier avant de l'importer
  (\code{\link{finir_xml.moodle}}).

  La plupart des types de questions de base de Moodle sont connus :
  vrai-faux (\code{\link{vrai_faux.moodle}}), questions à choix parmi
  des réponses prédéfinies (\code{\link{qcm.moodle}}), questions à
  réponse numérique (\code{\link{numerique.moodle}}), questions à
  réponse libre (\code{\link{qroc.moodle}},
  \code{\link{question_ouverte.moodle}}) et le format libre permettant
  de poser plusieurs questions en une
  (\code{\link{question_libre.moodle}}).  Il est possible de générer un
  type de question non explicitement supporté grâce à la fonction
  générique \code{\link{question.moodle}}, à condition que vous
  connaissiez les noms des balises XML nécessaires.

  Il est possible de trier les questions en catégories afin de les
  repérer plus facilement dans la base de question
  (\code{\link{categorie.moodle}} ; c'est indispensable si vous prévoyez
  dans votre test le tirage au sort d'une question parmi une série de
  questions similaires)

  Afin de faciliter la construction des énoncés des questions, quelques
  fonctions sont proposées pour convertir des objets fréquemment
  utilisés en code HTML tout en respectant les conventions
  typographiques françaises : nombres
  (\code{\link{afficher_nombre.moodle}}), échantillons de valeurs
  (vecteur de nombres, \code{\link{afficher_echantillon.moodle}},
  construisant un tableau).
}

\author{
  Emmanuel Curis \email{emmanuel.curis@parisdescartes.fr}
}
\keyword{ package }
