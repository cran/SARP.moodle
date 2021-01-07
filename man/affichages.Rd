\name{affichage.moodle}
\alias{affichage.moodle}
\alias{afficher_poly.moodle}
\alias{afficher_nombre.moodle}
\alias{afficher_echantillon.moodle}
\alias{afficher_echantillons.moodle}
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
afficher_echantillon.moodle( x, tableau = TRUE, trier = FALSE,
                             n.chiffres = get( "nombre.chiffres",
                                               envir = SARP.Moodle.env ),
                             marge = c( 10, 10, 1, 1 ), \dots )

afficher_echantillons.moodle( x, trier = FALSE,
                              n.chiffres = get( "nombre.chiffres",
                                                envir = SARP.Moodle.env ),
                              marge = c( 10, 10, 1, 1 ),
                              pre.texte = paste0( "\u00c9chantillon\u00a0",
                                                  1:length( x ) ),
                              couleur.trait = "Black", lg.trait = "2",
                              \dots )

afficher_nombre.moodle( x,
                        dec = get( "decimal", envir = SARP.Moodle.env ),
                        n.chiffres = get( "nombre.chiffres",
                                          envir = SARP.Moodle.env ),
                        unite = "" )

afficher_poly.moodle( degre, variable, a, \dots )
}
\arguments{
  \item{x}{L'élément à convertir au format HTML. Voyez les détails pour
  plus de précisions.}

  \item{n.chiffres}{ Le nombre de chiffres à conserver pour l'affichage.
    Si le nombre est plus petit, en valeur absolue, que 10^-n.chiffres,
    il ne peut pas être affiché avec ce nombre de décimales : c'est
    alors le nombre de chiffres significatifs. Sinon, c'est le nombre de
    décimales. }
  
  \item{trier}{Si \code{TRUE}, les valeurs de l'échantillon sont triées
    par ordre croissant avant d'être converties en code HTML.  S'il y a
    plusieurs échantillons, chacun est trié séparément. }

  \item{marge}{Les marges à laisser au sein de chaque case du tableau
    affichant les résultats. Ce doit être un vecteur de quatre valeurs
    entières, correspondant dans l'ordre aux marges gauche, droite,
    haute et basse, exprimées en pixels. }

  \item{\dots}{Autres options pour
    \code{\link{sortie_R.moodle.numeric}}, qui est appelé pour
    l'affichage sous forme de tableau.}

  Options pour \code{afficher_echantillon.moodle}
  
  \item{tableau}{Si \code{TRUE}, un tableau HTML est construit. Sinon,
    une simple énumération des valeurs est préparée. }

  Options pour \code{afficher_echantillons.moodle}

  \item{pre.texte}{Les textes à afficher au début de chaque ligne,
    permettant d'identifier les échantillons. }

  \item{couleur.trait}{La couleur des filets du tableau (ce doit être
    une couleur valable en CSS)}

  \item{lg.trait}{L'épaisseur des filets du tableau (exprimée en
    pixels)}
  
  Options pour \code{afficher_nombre.moodle}

  \item{dec}{ Le symbole à utiliser comme séparateur décimal. Par
    défaut, celui indiqué à la création du questionnaire. }

  \item{unite}{ L'unité à utiliser pour l'affichage de la valeur. }

  Options pour \code{afficher_poly.moodle}

  \item{degre}{ Le degré du polynome. }

  \item{variable}{ Le texte correspondant à la variable du polynome. Ce
    texte peut être quelconque et inclure des balises HTML. S'il
    correspond à une unique lettre minuscule (typiquement, x), les
    balises de mise en italique sont automatiquement ajoutées, afin de
    se conformer aux règles typographiques françaises. }

  \item{a}{ Vecteur de longueur \code{degre + 1} donnant les
    coefficients du polynôme, par degré décroissant. }
}

\details{Pour \code{afficher_echantillon.moodle}, \code{x} peut être un
  vecteur ou une liste; chaque élément est converti en texte avec
  \code{afficher_nombre.moodle}. La liste des valeurs est alors créée,
  soit sous la forme (x1 ; x2 ; ...), soit sous la forme d'un tableau
  d'une ligne, avec un filet au-dessus et en-dessous.

  Pour \code{afficher_echantillon.moodle}, \code{x} doit être une liste,
  dont chaque élément est un échantillon. Un tableau est alors créé,
  avec une ligne par échantillon (la ligne commençant par le texte
  fourni dans \code{pre.texte} et permettant d'identifier
  l'échantillon). Chaque valeur de chaque échantillon est convertie en
  texte avec \code{afficher_nombre.moodle}.
  
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
  corrects).

  La fonction \code{afficher_poly.moodle} permet de créer le code HTML
  d'affichage d'un polynome de degré quelconque ; l'affichage se fait
  par puissances décroissantes (\eqn{a\,x + b}{a x + b} pour un polynôme
  de degré 1, par exemple).
}

\value{Ces fonctions renvoient une unique chaîne de caractères, au
  format HTML, qui peut être utilisée pour construire le texte d'un
  énoncé, d'une réponse affichée, d'un commentaire...  }

\author{
  Emmanuel Curis \email{emmanuel.curis@parisdescartes.fr}
}

\examples{
\dontrun{
# Commencer un nouveau questionnaire
q <- debuter_xml.moodle( "exemple.affichages" )

# Catégorie : exemples
categorie.moodle( "Exemples SARP/Affichages" )

# Signe moins, décimale
vrai_faux.moodle( "Quelle est l'écriture correcte de l'opposé de cinq huitièmes&thinsp;?",
                  afficher_nombre.moodle( -5/8 ), -5/8 )

# Affichage des puissances
vrai_faux.moodle( paste( "L'inverse de", afficher_nombre.moodle( 10^8 ),
                         "est", afficher_nombre.moodle( 10^-8 ) ) )

# Affichage d'un échantillon
e <- rnorm( 10 ) * 2 + 20 
numerique.moodle( paste0( "Donnez la plus petite valeur de l'échantillon ci-dessous.",
                          afficher_echantillon.moodle( e ) ),
                  min( e ) )

# Affichage de deux échantillons
e1 <- rnorm( 10 ) * 2 + 20
e2 <- rnorm( 5 ) * 1 + 5
d <- mean( e1 ) - mean( e2 )
numerique.moodle( paste0( "Donnez la différence des moyennes des deux échantillons ci-dessous.",
                          afficher_echantillons.moodle( list( e1, e2 ) ) ),
                  c( d, -d ), notes = c( 100, 100 ) )

# Affichage d'un polynôme
numerique.moodle( paste0( "Quelle est la solution de l'équation ",
                          afficher_poly.moodle( 1, 'x', c( 2, -3 ) ),
                          " = 0&thinsp;?" ),
                  3/2 )

# On termine le questionnaire
finir_xml.moodle( )

# Importez le fichier exemple.affichages.xml dans Moodle pour voir le résultat...
print(
"Importez le fichier exemple.affichages.xml dans Moodle pour voir le résultat..." )
}
}
