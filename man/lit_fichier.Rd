\name{lier_fichier.moodle}
\alias{lier_fichier.moodle}
\title{
  Insérer un lien vers un fichier dans une question
}
\description{
  Cette fonction permet de créer un liens vers un fichier à télécharger dans les
  questions Moodle (texte, mais aussi réponses).
}
\usage{
lier_fichier.moodle( nom.fichier, texte.lien = NULL, interne = TRUE )
}
\arguments{
  \item{nom.fichier}{Le nom du fichier vers lequel pointera le lien. Si
    le fichier est externe au fichier XML, ce doit être le nom tel qu'il apparaîtra
    dans le dossier moodle : attention aux conversions de caractères que
    peut faire Moodle lors de l'import d'un fichier... Ce doit être un
    vecteur de type \code{character} contenant un seul élément. }

  \item{texte.lien}{ Un vecteur de chaîne de caractère, ne contenant
    qu'un seul élément, indiquant le texte à afficher comme lien vers le
    fichier. S'il est absent, le nom du fichier entre crochets est utilisé.}

  \item{interne}{ Indique si le fichier est déjà sur Moodle (\code{FALSE})
    ou si elle est incluse dans le fichier XML (\code{TRUE}). Dans le
    premier cas, l'URL de base doit avoir été définie par
    \code{definir_dossier.image.moodle} (voyez les détails, ci-après).
  }
}
  
\details{
  La fonction \code{lier_fichier.moodle} crée une balise HTML \code{<a href=>}
  avec une URL pointant vers le fichier. Cette URL est obtenue en
  concaténant une racine et le nom du fichier indiqué.

  Pour un fichier extérieur au fichier XML, la racine, définie avec la
  fonction \code{\link{definir_dossier.image.moodle}}, peut être
  n'importe quel schéma d'URL valide. Le plus souvent, ce sera soit une
  URL vers un site externe qui contient les fichiers soit, encore plus
  fréquent, une URL référençant le fichier dans Moodle. Il faut alors
  préciser \code{interne = FALSE} lors de l'appel à
  \code{lier_fichier.moodle}

  Par défaut, le fichier est inclus dans le fichier XML. Lors de la
  création de la question, les balises ainsi générées seront identifiées
  et les fichiers encodés dans le fichier XML, avec la fonction
  \code{coder_image.moodle}. Attention cependant, les chemins d'accès
  locaux étant perdus, les fichiers  doivent se
  trouver dans le répertoire de travail au moment de la création de la
  question. Il est cependant possible d'indiquer un dossier local
  différent, avec \code{definir_dossier.image.moodle}, en
  précisant \code{local = TRUE}.
}
\value{ La fonction \code{lier_fichier.moodle} renvoie une chaîne de
  caractère qui contient le code HTML créé. Cette chaîne peut ensuite
  être utilisée, directement ou après concaténation avec d'autres
  éléments, dans la constitution des textes ou des réponses de n'importe
  quelle question moodle.
}
\author{
  Emmanuel Curis \email{emmanuel.curis@parisdescartes.fr}
}

\section{Attention}{Vous devez appeler au moins une fois la fonction
  \code{definir_dossier.image.moodle} pour pouvoir ensuite lier des
  fichiers externes au fichier XML. Au moment de la création du fichier
  XML, il n'y a aucun moyen de vérifier que l'URL indiquée est
  correcte...
}
\section{Astuce}{Pour importer plusieurs fichiers en une fois, sans les
  inclure au fichier XML, vous avez tout intérêt à les regrouper dans
  une archive au format zip, que vous décompresserez après l'avoir
  importée dans le dossier moodle ciblé.
}

\seealso{\code{\link{inserer_formule.moodle}} pour insérer une formule
  mathématique à l'aide d'un code latex;
  \code{\link{inserer_SMILES.moodle}} pour insérer une formule chimique
  à l'aide d'un code SMILES ; \code{\link{lier_image.moodle} pour
    insérer une image (balise img)}
}
