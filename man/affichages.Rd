\name{affichage.moodle}
\alias{affichage.moodle}
\alias{afficher_nombre.moodle}
\alias{afficher_echantillon.moodle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Aide aux affichages dans les textes des questions
}
\description{
  Ces fonctions permettent de créer une chaîne de caractère au format
  HTML, qui peut ensuite être intégrée au texte d'une question (énoncé,
  réponse, commentaire...).
}
\usage{
afficher_echantillon.moodle( x, tableau = TRUE,
                             n.lignes = 1, trier = FALSE )
afficher_nombre.moodle( x,
                        dec = get( "decimal", envir = SARP.Moodle.env ),
                        n.chiffres = get( "nombre.chiffres", envir = SARP.Moodle.env ) )
}
\arguments{
  \item{x}{L'élément à convertir au format HTML. Voyez les détails pour
  plus de précisions.}

  Options pour \code{afficher_echantillon.moodle}
  
  \item{tableau}{Si \code{TRUE}, un tableau HTML est construit. Sinon,
    une simple énumération des valeurs est préparée. }
  
  \item{n.lignes}{Si l'on construit un tableau, combien de lignes
  doit-il avoir ? \emph{Actuellement ignoré}}

  \item{trier}{Si \code{TRUE}, les valeurs de l'échantillon sont triées
  par ordre croissant avant d'être converties en code HTML. }
  
  Options pour \code{afficher_nombre.moodle}

  \item{dec}{ Le symbole à utiliser comme séparateur décimal. Par
    défaut, celui indiqué à la création du questionnaire. }

  \item{n.chiffres}{ Le nombre de chiffres à conserver pour l'affichage.
  Si le nombre est plus petit, en valeur absolue, que 10^-n.chiffres, il
  ne peut pas être affiché avec ce nombre de décimales : c'est alors le
  nombre de chiffres significatifs. Sinon, c'est le nombre de
  décimales. }
}

\details{Pour \code{afficher_echantillon.moodle}, \code{x} peut être un
  vecteur ou une liste; chaque élément est converti en texte avec
  \code{afficher_nombre.moodle}. La liste des valeurs est alors créée,
  soit sous la forme (x1 ; x2 ; ...), soit sous la forme d'un tableau
  d'une ligne, avec un filet au-dessus et en-dessous.

  Pour \code{afficher_nombre.moodle}, \code{x} doit être un vecteur
  contenant au plus une seule valeur (dans le cas contraire, seule la
  première est utilisée, les autres sont ignorées avec un
  avertissement). S'il est de longueur nulle, le symbole de l'ensemble
  vide (entité \code{&empty;}) est renvoyé, mis en forme avec le style
  utilisé pour les messages d'erreur. Les valeurs manquantes (\code{NA})
  ou non-numériques (\code{NaN}) sont affichées avec ces caractères, et
  le style des messages d'erreur. Les valeurs infinies (\code{Inf} et
  \code{-Inf}) sont affichées avec le symbole \eqn{\infty}{infini}
  (entité \code{&infin;}). Les chaînes de caractère sont affichées
  inchangées. Enfin, les nombres sont mis en forme avec le symbole
  \code{dec} comme séparateur décimal, un signe moins correct (entité
  \code{&minus;}) et, si nécessaire, une puissance de 10 correcte (soit
  \code{&thinsp;&times;&thinsp;10<sup>n</sup>}, où \eqn{n} est la
  puissance, pour avoir \eqn{\times 10^n}{×10^n} avec des espacements
  corrects).}

\value{Ces fonctions renvoient une unique chaîne de caractères, au
  format HTML, qui peut être utilisée pour construire le texte d'un
  énoncé, d'une réponse affichée, d'un commentaire...  }

\author{
  Emmanuel Curis \email{emmanuel.curis@parisdescartes.fr}
}

\examples{
# Commencer un nouveau questionnaire
q <- debuter_xml.moodle( "exemple.affichages" )

# Catégorie : exemples
categorie.moodle( "Exemples SARP/Affichages" )

# Signe moins, décimale
vrai_faux.moodle( "Quelle est l'écriture correcte de l'opposé de cinq huitièmes?",
                  afficher_nombre.moodle( -5/8 ), -5/8 )

# Affichage des puissances
vrai_faux.moodle( paste( "L'inverse de", afficher_nombre.moodle( 10^8 ),
                         "est", afficher_nombre.moodle( 10^-8 ) ) )

# Affichage d'un échantillon
e <- rnorm( 10 ) * 2 + 20 
numerique.moodle( paste0( "Donnez la plus petite valeur de l'échantillon ci-dessous.",
                          afficher_echantillon.moodle( e ) ),
                  min( e ) )

# On termine le questionnaire
finir_xml.moodle( )

# Importez le fichier exemple.affichages.xml dans Moodle pour voir le résultat...
print( "Importez le fichier exemple.affichages.xml dans Moodle pour voir le résultat..." )
}
