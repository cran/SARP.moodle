\name{csv_optique.moodle}
\alias{csv_optique}
\alias{csv_optique.moodle}
\title{
  Convertir un fichier CSV en questions Moodle
}
\description{
  Cette fonction permet de convertir un fichier CSV, au format « lecture
  optique », en un fichier XML de questions pour Moodle
}
\usage{
csv_optique.moodle( fichier.csv,
                    forcer.multiple = FALSE, melanger.reponses = TRUE, 
                    somme.nulle = FALSE,
                    fichier.xml = if ( TRUE == nv.fichier )
                                    gsub( "\\\\.[Cc][Ss][Vv]$",
                                          ".xml", fichier.csv )
                                  else get( "fichier.xml",
                                            envir = SARP.Moodle.env ),
                    nv.fichier = TRUE,
                    embellir = TRUE, deja.HTML = FALSE,
                    categorie.base = "",
                    dossier.images = ".",
                    sep.images = c( '@@', '@@' ), inserer.images = TRUE,
                    sep.formules = c( '@\\\\$', '\\\\$@' ),
                    sep.SMILES = c( '@\\\\{', '\\\\}@' ),
                    sep = ";", header = TRUE, quote = '\"', 
                    ... )
}
\arguments{
  \item{fichier.csv}{
    Un vecteur de chaînes de caractères contenant les noms des fichiers
    à convertir. Chacun des fichiers est traité indépendamment, mais
    avec les mêmes valeurs des options qui suivent.
  }

  \item{forcer.multiple}{
    Si \code{TRUE}, les questions sont toujours
    considérées comme des réponses à choix multiples (l'étudiant pourra
    cocher plusieurs réponses). Sinon, si une seule bonne réponse est
    proposée, la question est à choix unique (l'étudiant ne pourra
    choisir qu'une seule réponse).
  }

  \item{melanger.reponses}{
    Si \code{TRUE}, autorise Moodle à permuter aléatoirement l'ordre des
    réponses lorsqu'il pose la question. Sinon, l'ordre du fichier est
    tout le temps utilisé.
  }

  \item{somme.nulle}{
    Cette option précise comment gérer les réponses incorrectes du
    QCM.  Si \code{FALSE}, les réponses incorrectes ont une note nulle.
    Si \code{TRUE}, les réponses incorrectes se voient attribuer une
    note négative telle que la somme de ces notes vaut -1, de sorte que
    si l'étudiant coche toutes les cases du QCM, il a 0 (sauf bien sûr
    si toutes les cases correspondent à des questions correctes...).
  }
  
  \item{fichier.xml}{
    Le nom du fichier XML à créer ou un fichier XML déjà créé avec
    \code{\link{debuter_xml.moodle}}. Par
    défaut, la fonction crée un fichier de même nom que le premier
    fichier fourni, en remplaçant l'extension \code{.csv} par
    \code{.xml}.
  }
  
  \item{nv.fichier}{
    Une valeur logique indiquant si l'on veut créer un nouveau fichier
    (\code{TRUE}) ou utiliser un fichier XML déjà ouvert avec
    \code{\link{debuter_xml.moodle}} (\code{FALSE})
  }
  
  \item{embellir,deja.HTML}{
    Actuellement inutilisés, prévus pour une version future
  }
    
  \item{categorie.base}{
    La catégorie de base à utiliser pour créer les questions. Elle
    contiendra toutes les questions du fichier.
  }

  \item{dossier.images}{ Le dossier d'image où trouver les images à
    intégrer au fichier XML, ou l'URL de base pour les trouver dans
    Moodle (voir \code{\link{definir_dossier.image.moodle}}). }

  \item{sep.images}{ Les codes servant à encadrer un nom d'image dans
    les textes du fichier CSV. }

  \item{inserer.images}{ Si \code{TRUE}, les images sont incluses dans
    le fichier XML. Sinon, seul le lien est construit. }
  
  \item{sep.formules}{ Les codes servant à encadrer une formule
    mathématique à convertir grâce à latex dans les textes du fichier
    CSV. }
  
  \item{sep.SMILES}{ Les codes servant à encadrer un code SMILES à
    convertir en formule chimique grâce à openbabel dans les textes du
    fichier CSV. }
  
  \item{sep, header, quote}{
    Options pour \code{\link{read.table}}, correspondant au format d'un
    fichier CSV par défaut lorsqu'il est créé par Libre office ou Excel.
  }
  \item{...}{
    Autres options pour \code{\link{read.table}}, si nécessaire.
  }
}
\details{
  Ces fonctions réalisent la conversion d'un ou plusieurs fichiers CSV
  de questions pour Moodle en un fichier XML. Les questions sont
  nécessairement des QCM.

  Ce fichier doit être au format utilisé par les lecteurs optiques de
  QCM. La première ligne, qui donne le numéro de question, est ignorée,
  ainsi que la première colonne.  Chacune des autres colonnes correspond
  à une question différente.

  Pour chaque question, la première ligne indique le titre, la suivante
  l'énoncé.  Ensuite, les réponses possibles et leur justesse alternent.


  \strong{Insérer des images}

  Il est possible de faire référence à une image dans les textes
  (énoncés, réponses, commentaires) en encadrant le nom de fichier de
  l'image par les séparateurs choisis, par défaut \code{@@}. Il est
  possible de redimensionner l'image en faisant suivre le nom de l'image
  d'un point d'exclamation, puis de la largeur, du caractère x et de la
  hauteur.  Attention, il semblerait que Moodle ne déforme pas les
  images, quelles que soient les dimensions demandées.  Par défaut,
  l'image sera incluse au fichier XML.

  Deux types d'image peuvent être créés au vol : les formules
  mathématiques et les formules chimiques. Voir les fonctions
  \code{\link{inserer_formule.moodle}} et
  \code{\link{inserer_SMILES.moodle}} pour plus de détails.
}
\section{Attention!}{
    Moodle est assez peu permissif pour les notes
  exprimées en pourcentage; en particulier, lorsque ce pourcentage ne
  tombe pas juste (33\% pour trois bonnes réponses, par exemple), à
  cause de l'arrondi il peut rejeter le fichier créé. Dans ce cas,
  désactivez l'arrêt en cas d'erreur et demandez l'arrondi à la note la
  plus proche pour pouvoir importer les questions.
  }
\value{
  De façon invisible, une liste des data.frames des questions après
  traitement de chaque fichier.  Elle n'a guère d'autre intérêt que pour
  détecter l'origine d'un problème de conversion.
}
\seealso{
  Les fonctions de base de création de questions pour plus de souplesse,
  et en particulier pour créer des questions non gérées par le format de
  fichier ci-dessus.

  \code{\link{csv.moodle}} pour convertir un fichier CSV au format
  proposé par SARP.moodle, permettant de gérer davantage de sortes de
  question.
}

\author{
  Emmanuel Curis \email{emmanuel.curis@parisdescartes.fr} (sur une idée
  de Wilfrid Cariou)
}
\examples{
  # Conversion du fichier d'exemple fourni
  #  (à placer dans le répertoire de travail)
\dontrun{
  csv_optique.moodle( "exemple_optique.csv" )
}
}
