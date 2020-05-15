\name{legender_image.moodle}
\alias{legender_image}
\alias{legender_image.moodle}
\title{
  Créer une question demandant de placer des éléments à des positions
  précises d'une image
}

\description{ Cette fonction permet de créer une question dans laquelle
  le but est de placer des éléments sur une image.
}

\usage{
legender_image.moodle( texte, titre = "L\u00e9gender...",
                       f.creer_figure,
                       fichier.image,
                       zones, marques,
                       ordre.aleatoire = TRUE, afficher.erreurs = TRUE,
                       commentaire.global = NA, penalite = NA, note.question = NA,
                       fichier.xml = get( "fichier.xml", envir = SARP.Moodle.env ),
                       \dots )
}
\arguments{
  \item{texte}{ L'énoncé de la question (une chaîne de caractères). Cet
  énoncé peut contenir n'importe quel caractère (codage en UTF-8) et
  n'importe quelles balises HTML.  }

  \item{titre}{ L'intitulé de la question, en HTML. }

  \item{f.creer_figure}{ La fonction qui va construire l'image et les
    zones sur lesquelles déposer les éléments (voyez les détails,
    ci-dessous). }

  \item{fichier.image}{ Le nom du fichier contenant l'image à
    légender. Ignoré si \code{f.creer_figure} est fournie.}

  \item{zones}{ Une liste décrivant les zones de la figure. Le format
    est celui utilisé par f.creer_figure, voyez les détails ci-dessous;
    les coordonnées doivent cependant être données directement en
    pixels. Ignoré si \code{f.creer_figure} est fournie.}

  \item{marques}{Une \code{data.frame} décrivant les marques à placer
    sur la figure. Le format est celui utilisé par f.creer_figure, voyez
    les détails ci-dessous. Ignoré si \code{f.creer_figure} est fournie.}

  \item{ordre.aleatoire}{Si \code{TRUE}, l'ordre des marques est permuté
    aléatoirement dans le fichier}

  \item{afficher.erreurs}{Si \code{TRUE}, en cas de réponse incorrecte à
    la question, les réponses erronées sont identifiées.}
  
  \item{commentaire.global, penalite, note.question}{Options communes à
    toutes les questions Moodle, voyez
    \code{\link{debut_question.moodle}}.}

  \item{fichier.xml}{ Le fichier correspondant au questionnaire qui
    contiendra cette question (par défaut, le dernier créé).  }

  \item{\dots}{ Arguments additionnels qui seront passés à
    \code{f.creer_figure}.}
}

\details{ Ce type de question affiche une image et, en-dessous, des
  étiquettes textuelles (code HTML possible) à placer sur l'image. Le
  placement de l'étiquette est défini par son coin supérieur gauche (qui
  est affiché sous forme de cible quand on la déplace). Les zones où
  placer les étiquettes sont invisibles et couvrent une surface libre de
  l'image, qui peut être rectangulaire, circulaire ou définie à l'aide
  d'un polygone.
  
  \bold{Utilisation de la fonction \code{f.creer_figure}}

  Avec cette méthode, la fonction se charge de créer le fichier d'image
  (au format PNG, dans le dossier temporaire de R) avant d'appeler
  \code{f.creer_figure}. La fonction \code{f.creer_figure} peut alors
  utiliser tous les outils graphiques basiques de R pour créer une
  figure et les zones à légender sur cette figure. Elle \emph{ne} doit
  \emph{pas} clore la figure (pas d'appel à \code{dev.off()} ou à
  \code{graphics.off}.

  La fonction \code{f.creer_figure} doit renvoyer une liste de deux
  éléments:
  \itemize{
    \item un élément nommé \code{Marques} qui définit les marques à
    placer sur la figure;

    \item un élément nommé \code{Zones} qui définit les zones possibles
    pour placer ces marques.
  }

  L'élément \code{Zones} doit être une liste qui contient autant
  d'éléments que de zones à définir. Chaque élément est lui-même une
  liste qui doit contenir
  \itemize{    
    \item un élément nommé \code{Type} qui définit, sous forme d'une
    chaîne de caractères, la forme géométrique de la zone. Moodle
    connaît trois types de forme: rectangulaire (chaîne à utiliser:
    \dQuote{Rectangle}), circulaire (\dQuote{Cercle}) et polygonale
    (\dQuote{Polygone}).

    \item un élément nommé \code{Coord}, qui doit être une data.frame
    contenant au moins deux colonnes appelées \code{X} et \code{Y}, et
    autant de lignes que de coordonnées nécessaires pour définir la
    zone.  Pour une zone rectangulaire, la première ligne définit le
    coin supérieur, gauche et la seconde ligne le coin inférieur
    droit. Pour une zone polygonale, chaque ligne définit un sommet du
    polygone (il n'est pas nécessaire de répéter les coordonnées du
    premier sommet à la fin). Pour une zone circulaire, une seule ligne
    est nécessaire, définissant le centre du cercle. Le rayon du cercle
    doit être donné dans une troisième colonne, nommée \code{Rayon}. }

  Les coordonnées doivent être données en unités \dQuote{utilisateur},
  c'est-à-dire les coordonnées naturelles de l'image (celles des
  échelles des axes). La conversion en pixels sera faite
  automatiquement, à l'aide des fonctions \code{\link{grconvertX}} et
  \code{\link{grconvertY}}.  En général, les échelles ne sont pas les
  mêmes pour les deux axes, de ce fait la conversion du rayon d'un
  cercle ne conduit pas à la même valeur suivant qu'il est considéré
  parallèle à l'axe des abscisses ou à l'axe des ordonnées (ce qui
  revient à dire qu'un cercle serait représenté par une ellipse sur la
  figure).  La plus grande de ces deux valeurs est utilisée pour définir
  le rayon, en pixels, de la zone à créer.  Si vous voulez contrôler
  parfaitement la forme de la zone, vous devez forcer le graphe à avoir
  les mêmes échelles dans les deux dimensions (option \code{asp = TRUE}
  dans \code{\link{plot}}).

  L'élément \code{Marques} doit être une \code{data.frame} qui contient
  autant de lignes que de marques à placer. Cette \code{data.frame} doit
  avoir au moins une colonne nommée \dQuote{\code{Marque}}, qui contient
  le texte de chaque marque (tout code HTML est possible et sera mis en
  forme par Moodle). Par défaut, chaque marque ne peut être utilisée
  qu'une fois.  Pour préciser un nombre d'utilisation plus important, la
  colonne \dQuote{\code{Nombre}} peut être utilisée (y compris avec la
  valeur \code{+Inf}).
  
  \bold{Utilisation directe}

  Il est aussi possible de préciser une image quelconque, grâce à
  l'option \code{fichier.image}. Dans ce cas, les options \code{zones}
  et \code{marques} doivent contenir, respectivement, les zones à
  définir et les marques à placer, en utilisant le format ci-dessus.
  Seule différence, les coordonnées des points définissant les zones (et
  le rayon, pour une zone circulaire) doivent être exprimés directement
  en pixels.

}

\value{
  Aucune valeur n'est renvoyée par cette fonction.
}
\author{
  Emmanuel Curis \email{emmanuel.curis@parisdescartes.fr}
}

\seealso{\code{\link{glisser_deposer.moodle}} pour une autre façon de
  demander à placer des éléments sur une image;
  \code{\link{glisser_textes.moodle}} pour faire glisser des textes dans un
  texte à trou.  }
