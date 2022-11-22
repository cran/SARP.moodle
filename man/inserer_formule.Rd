\name{inserer_formule.moodle}
\alias{inserer_formule.moodle}
\title{
  Insérer une formule mathématique, comme image, grâce à latex
}
\description{
  Cette fonction permet de convertir une formule mathématique au format
  Latex en image prête à insérer dans un texte.
}
\usage{
inserer_formule.moodle( formule, filtre.Moodle = FALSE,
                        displaystyle = TRUE, marge = 2, 
                        couleurs = TRUE, enjoliver = TRUE,
                        cmd.latex = "latex -shell-escape --interaction errorstopmode",
                        packages = c( "amsfonts", "amsmath",
                                      "amssymb" , "dsfont" ),
                        options.convert = list( 'density' = 150,
                                                'outext'  = ".png" ) )

}
\arguments{
  \item{formule}{Une chaîne de caractères contenant la formule
    mathématique à convertir, au format latex (sans les délimiteurs).
  }

  \item{filtre.Moodle}{Si \code{TRUE}, la formule est entourée des
    marqueurs permettant à Moodle de faire la conversion, en utilisant
    son filtre (qui va dépendre de la configuration faite sur le
    serveur).  Cela a l'avantage de faire une base plus légère (pas
    d'image...) et un affichage plus harmonieux, mais aussi plus lent et
    avec moins de souplesse, puisque les bibliothèques disponibles sont
    fonction de l'installation faite. Sinon, elle est convertie en image
    localement avec latex et l'image est insérée dans le fichier XML.}

  \item{displaystyle}{Si \code{TRUE}, la commande latex
    \code{\\displaystyle} est ajoutée en début de formule.}

  \item{marge}{La marge à laisser autour de la formule (en points).}

  \item{couleurs}{Si \code{TRUE}, le package \dQuote{xcolor} est inclus
    et les noms de couleurs usuels sont utilisables.}

  \item{enjoliver}{Si \code{TRUE}, quelques adaptations de la formule
    sont faites pour avoir un résultat plus esthétique. Actuellement,
    cela correspond à protéger la virgule entre deux chiffres par des
    accolades (évitant ainsi une espace après la virgule) et à remplacer
    les symboles inégalité large par défaut (\dQuote{\\leq} et
    \dQuote{\\geq}) par leur version avec l'égalité parallèle au signe
    (\dQuote{\\leqslant} et \dQuote{\\geqslant}).}

  \item{cmd.latex}{Une chaîne de caractères indiquant la commande à
    utiliser pour lancer latex}
  
  \item{packages}{Un vecteur de chaînes de caractères indiquant les noms
    des packages latex à inclure}

  \item{options.convert}{Une liste d'options à passer au package
    \dQuote{standalone}, utilisé pour créer l'image.}
}

\details{
  Cette fonction permet de créer une image d'une formule mathématique, à
  partir du code latex correspondant.

  La taille de l'image dépend de la longueur de la formule, mais aussi
  de la définition choisie pour créer l'image. Il n'est pas très facile
  de prévoir la qualité du résultat, puisque la taille du texte dans
  Moodle dépend, elle, de la feuille de style choisie et des réglages du
  navigateur.  L'option \dQuote{density} (par défaut à 150) permet de
  modifier cette taille: plus elle est élevée, plus l'image sera grande.

  La conversion est faite avec latex et le package \dQuote{standalone}:
  les deux doivent être installés et latex exécutable par R pour que la
  conversion puisse avoir lieu.

  Les commandes des formules latex commencent par le caractère \\, qui
  sert de code d'échappement dans R. Il faut donc les protéger en les
  doublant (\\\\) dans les formules.
}

\value{
  Une chaîne de caractère contenant le code XML nécessaire pour inclure
  une image (contenant la formule créée), et qu'elle soit incluse au
  fichier XML au moment de la création de la question.
}

\seealso{
  \code{\link{inserer_SMILES.moodle}} pour convertir un code SMILES en
  une formule chimique et l'insérer dans une question Moodle.

  \code{\link{lier_image.moodle}} pour insérer une image quelconque.
}

\author{
  Emmanuel Curis \email{emmanuel.curis@parisdescartes.fr}
}
\examples{
  # Création d'une question demandant la définition de la tangente
  #  (Non exécuté car latex non nécessairement installé)
\dontrun{
  # Commencer un nouveau questionnaire
  q <- debuter_xml.moodle( "tangente" )

  # Catégorie : exemples
  categorie.moodle( "Exemples SARP/Latex" )

  # Le format par défaut, avec deux bonnes réponses et trois mauvaises,
  qcm.moodle( "Quelle est la définition de la tangente&thinsp;?",
              inserer_formule.moodle( "\\\\frac{\\\\sin x}{\\\\cos x}" ),
              inserer_formule.moodle( "\\\\frac{\\\\cos x}{\\\\sin x}" ) )

  # On termine le questionnaire
  finir_xml.moodle( )

  # Importez le fichier tangente.xml dans Moodle pour voir le résultat...
  print( "Importez le fichier exemple.xml dans Moodle pour voir le résultat..." )
}
}
