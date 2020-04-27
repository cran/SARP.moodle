\name{definir_dossier.image.moodle}
\alias{definir_dossier.image.moodle}
\alias{lier_image.moodle}
\alias{inserer_image.moodle}
\alias{coder_image.moodle}
%\alias{image.moodle}
\title{
  Insérer une image dans une question
}
\description{
  Ces fonctions permettent de créer des liens vers des images dans les
  questions Moodle (texte, mais aussi réponses).
}
\usage{
definir_dossier.image.moodle( URL, local = FALSE )

lier_image.moodle( nom.image, largeur = -1, hauteur = -1,
                   description = NULL, interne = FALSE )

coder_image.moodle( nom.image,
                    dossier.image = get( "dossier.images",
                                         envir = SARP.Moodle.env ) )
}
\arguments{

  \item{URL}{ L'URL du dossier ou de l'activité Moodle contenant les
    images à lier. Voyez les détails pour plus d'information. Ce doit
    être un vecteur de type \code{character}, ne contenant qu'un seul
    élément.}

  \item{local}{ Précise si l'URL indiquée est locale (\code{TRUE}) ou
    sur le serveur Moodle (\code{FALSE}) }.

  \item{nom.image}{Le nom du fichier contenant l'image. Si l'image est
    externe au fichier XML, ce doit être le nom tel qu'il apparaîtra
    dans le dossier moodle : attention aux conversions de caractères que
    peut faire Moodle lors de l'import d'un fichier... Ce doit être un
    vecteur de type \code{character} contenant un seul élément. }

  \item{dossier.image}{Le nom du dossier contenant l'image, en local.}

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

  \item{interne}{ Indique si l'image est déjà sur Moodle (\code{FALSE})
    ou si elle est incluse dans le fichier XML (\code{TRUE}). Dans le
    premier cas, l'URL de base doit avoir été définie par
    \code{definir_dossier.image.moodle} (voyez les détails, ci-après).
  }
}
  
\details{
  La fonction \code{lier_image.moodle} crée une balise HTML \code{<img>}
  avec une URL permettant de trouver l'image. Cette URL est obtenue en
  concaténant une racine et le nom du fichier image indiqué.

  Pour une image extérieure au fichier XML, la racine, définie avec la
  fonction \code{definir_dossier.image.moodle}, peut être n'importe quel
  schéma d'URL valide. Le plus souvent, ce sera soit une URL vers un
  site externe qui contient les images soit, encore plus fréquent, une
  URL référençant l'image dans Moodle.

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
    \preformatted{definir_dossier.image.moodle("https://votre.site.moodle/pluginfile.php/0000/mod_folder/content/0/" ) }
  }

  Toutes les images situées dans ce dossier (au moment de l'import du
  fichier XML ou ajoutées ultérieurement) sont alors directement
  utilisables dans la fonction \code{lier_image.moodle}, en indiquant
  juste leur nom.

  Il est aussi possible d'inclure l'image dans le fichier XML. Pour
  cela, précisez \code{interne = TRUE} lors de l'appel à
  \code{lier_image.moodle}. Lors de la création de la question, les
  balises ainsi générées seront identifiées et les images encodées dans
  le fichier XML, avec la fonction \code{coder_image.moodle}. Attention
  cependant, les chemins d'accès locaux étant perdus, les fichiers
  contenant les images doivent se trouver dans le répertoire de travail
  au moment de la création de la question. Il est cependant possible
  d'indiquer un dossier local d'images différent, avec
  \code{definir_dossier.image.moodle}, en précisant \code{local = TRUE}.
}
\value{
  La fonction \code{lier_image.moodle} renvoie une chaîne de caractère
  qui contient le code HTML créé. Cette chaîne peut ensuite être
  utilisée, directement ou après concaténation avec d'autres éléments,
  dans la constitution des textes ou des réponses de n'importe quelle
  question moodle.

  La fonction \code{coder_image.moodle} renvoie une chaîne de caractère
  qui contient le code XML permettant d'intégrer l'image dans le fichier
  XML, y compris son encodage en base 64. Cette chaîne peut ensuite être
  utilisée dans la constitution des élements d'une
  question moodle. Attention cependant, savoir où le mettre dans le
  fichier XML n'est pas trivial: normalement, l'utilisation de cette
  fonction ne devrait pas être nécessaire, si \code{lier_image.moodle} a
  été utilisée correctement.
}
\author{
  Emmanuel Curis \email{emmanuel.curis@parisdescartes.fr}
}

\section{Attention}{Vous devez appeler au moins une fois la fonction
  \code{definir_dossier.image.moodle} pour pouvoir ensuite lier des
  images externes au fichier XML. Au moment de la création du fichier
  XML, il n'y a aucun moyen de vérifier que l'URL indiquée est
  correcte...
}

\section{Astuce}{Pour importer plusieurs images en une fois, vous avez
  tout intérêt à les regrouper dans une archive au format zip, que vous
  décompresserez après l'avoir importée dans le dossier moodle ciblé.
}

\seealso{\code{\link{inserer_formule.moodle}} pour insérer une formule
  mathématique à l'aide d'un code latex;
  \code{\link{inserer_SMILES.moodle}} pour insérer une formule chimique
  à l'aide d'un code SMILES.}
  
\examples{
\dontrun{
# Commencer un nouveau questionnaire
q <- debuter_xml.moodle( "exemple.image" )

# On crée une image quelconque
png( "essai.png", width = 640, height = 400 )
qqnorm( rnorm( 20 ), pch = 19 )
dev.off()

# On pose une question là-dessus
vrai_faux.moodle( paste0( "Le graphe ci-dessous est un diagramme de Henry",
                          "<br />\n",
                          lier_image.moodle( "essai.png", interne = TRUE ) ) )

# On termine le questionnaire
finir_xml.moodle( )

# Importez le fichier exemple.image.xml dans Moodle pour voir le résultat...
print( "Importez le fichier exemple.image.xml dans Moodle pour voir le résultat..." )

# Le code XML...
coder_image.moodle( "essai.png" )
}
}
