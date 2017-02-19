\name{definir_dossier.image.moodle}
\alias{definir_dossier.image.moodle}
\alias{lier_image.moodle}
\alias{image.moodle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Insérer une image dans une question
}
\description{
  Ces fonctions permettent de créer des liens vers des images dans les
  questions Moodle (texte, mais aussi réponses).
}
\usage{
definir_dossier.image.moodle( URL )

lier_image.moodle( nom.image, largeur = -1, hauteur = -1,
                   description = NULL )
}
\arguments{
  \item{URL}{ L'URL du dossier ou de l'activité Moodle contenant les
    images à lier. Voyez les détails pour plus d'information. Ce doit
    être un vecteur de type \code{character}, ne contenant qu'un seul élément.}

  \item{nom.image}{Le nom du fichier contenant l'image, tel qu'il
  apparaîtra dans le dossier moodle. Attention aux conversions de
  caractères que peut faire Moodle lors de l'import d'un fichier... Ce
  doit être un vecteur de type \code{character} contenant un seul élément.}

  \item{largeur, hauteur}{ Les dimensions d'affichage de l'image dans la
  question (équivalent des champs \code{height} et \code{width} de la
  balise \code{<img>}). Seules les dimensions numériques, finies,
  positives sont prises en compte. Toutes les autres valeurs sont
  ignorées et le champ correspondant n'est pas créé dans le code
  HTML. Ces deux valeurs doivent être des vecteurs numériques contenant
  un seul élément.}

  \item{description}{ Un vecteur de chaîne de caractère, ne contenant
    qu'un seul élément. S'il existe, ce texte sera utilisé comme
    description textuelle de l'image (champ \code{alt}, fortement
    conseillé).  }
}
  
\details{
  La fonction \code{lier_image.moodle} crée une balise HTML \code{<img>}
  avec une URL permettant de trouver l'image. Cette URL est obtenue en
  concaténant une racine et le nom du fichier image indiqué.

  La racine peut être n'importe quel schéma d'URL valide. Le plus
  souvent, ce sera soit une URL vers un site externe qui contient les
  images soit, encore plus fréquent, une URL référençant l'image dans
  Moodle.

  La méthode la plus simple pour obtenir cette racine de l'URL est la
  suivante :
  \enumerate{
    \item Créez une activité « Dossier » dans Moodle, cachée aux utilisateurs.

    \item Dans ce dossier, importez la ou les images, puis enregistrez.

    \item Affichez le contenu de ce dossier, comme un utilisateur.

    \item Affichez le code source de la page HTML correspondance (Ctrl-U
    avec Firefox)

    \item Dans ce code source, repérez le nom d'une image, dans un
    élément \code{<img src="">}. Ce nom devrait ressembler à
    https://votre.site.moodle/pluginfile.php/00000/mod_folder/content/0/votre_image.jpg?forcedownload=1,
    où 0000 est un nombre quelconque qui identifie ce dossier dans la
    base Moodle.

    \item Mémorisez cette URL et utilisez-la comme base:
    \code{lier_image.moodle(
      "https://votre.site.moodle/pluginfile.php/0000/mod_folder/content/0/"
      ) }
    }

    Toutes les images situées dans ce dossier (au moment de l'import
    du fichier XML ou ajoutée ultérieurement) sont alors directement
    utilisables dans la fonction \code{lier_image.moodle}, en indiquant
    juste leur nom.
}
  
\value{
  La fonction \code{lier_image.moodle} renvoie une chaîne de caractère
  qui contient le code HTML créé. Cette chaîne peut ensuite être
  utilisée, directement ou après concaténation avec d'autres éléments,
  dans la constitution des textes ou des réponses de n'importe quelle
  question moodle.
}
\author{
  Emmanuel Curis \email{emmanuel.curis@parisdescartes.fr}
}

\section{Attention}{Vous devez appeler au moins une fois la fonction
  \code{definir_dossier.image.moodle} pour pouvoir ensuite lier des
  images. Au moment de la création du fichier XML, il n'y a aucun moyen
  de vérifier que l'URL indiquée est correcte...
}

\section{Astuce}{Pour importer plusieurs images en une fois, vous avez
  tout intérêt à les regrouper dans une archive au format zip, que vous
  décompresserez après l'avoir importée dans le dossier moodle ciblé.
}
%\seealso{
%}
\examples{
# Commencer un nouveau questionnaire
q <- debuter_xml.moodle( "exemple.image" )

# On termine le questionnaire
finir_xml.moodle( )

# Importez le fichier exemple.image.xml dans Moodle pour voir le résultat...
print( "Importez le fichier exemple.image.xml dans Moodle pour voir le résultat..." )
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
