\name{categorie.moodle}
\alias{categorie.moodle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Créer une catégorie de questions
}
\description{
  Cette fonction permet de créer une nouvelle catégorie de questions
  dans le questionnaire XML.
}
\usage{
categorie.moodle( nom.categorie, autoriser.dollar = FALSE,
                  temps = NULL,
                  fichier.xml = get( "fichier.xml", envir = SARP.Moodle.env ) )
}
\arguments{
  \item{nom.categorie}{ Le nom de la catégorie. Il est possible de
    donner une hiérarchie de catégories, en utilisant la barre oblique
    (\code{/}) comme séparateur des différents niveaux --- comme dans un
    chemin incluant des dossiers.}

  \item{autoriser.dollar}{Le dollar (\code{$}) est un caractère réservé,
    qui permet d'ancrer les catégories dans les divers cours. Pour
    éviter des difficultés, les dollars sont donc remplacés par des
    soulignés (\code{_}). Cette conversion peut être désactivée en
    indiquant \code{TRUE} pour cette option.}

  \item{temps}{Le temps conseillé pour répondre aux questions de la
    catégorie. Ce temps sera utilisé pour toutes les questions créées
    jusqu'au prochain appel de cette fonction, si aucun temps conseillé
    n'est indiqué lors de l'appel à la fonction créant la fonction.}

  \item{fichier.xml}{ Le fichier correspondant au questionnaire qui
    contiendra cette question (par défaut, le dernier créé).  }
}
\details{
  La catégorie proposée est toujours intégrée dans le cours actuel, en
  ajoutant \code{$course$} en début de hiérarchie.

  Pour éviter des sous-catégories sans nom, les séparateurs multiples sont
  simplifiés en un seul séparateur et ceux en fin de nom de catégorie
  sont supprimés. Si vous voulez vraiment des sous-catégories sans nom
  visible, utilisez des espaces entre les séparateurs ou après le dernier.
}
\value{
  Aucune valeur n'est renvoyée par cette fonction.
}
\author{
  Emmanuel Curis \email{emmanuel.curis@parisdescartes.fr}
}

\section{Attention}{Les catégories ne seront prises en compte que si
  l'option correspondante est cochée lors de l'import du fichier (case «
  Obtenir la catégorie à partir du fichier »)
  }
%\seealso{
%}
\examples{
\dontrun{
# Commencer un nouveau questionnaire
q <- debuter_xml.moodle( "exemple.categorie" )

# Catégorie des questions numériques pour les exemples de cette documentation
categorie.moodle( "Exemples SARP/Numériques" )
numerique.moodle( "Combien vaut 1&nbsp;+&nbsp;1&thinsp;?", 2 )
numerique.moodle( "Combien vaut 1&nbsp;&times&nbsp;1&thinsp;?", 1 )
numerique.moodle( "Combien vaut 1&nbsp;+&nbsp;1 en binaire&thinsp;?", 10 )

# Catégorie des QROC pour les exemples de cette documentation
categorie.moodle( "Exemples SARP/QROC" )
qroc.moodle( "Comment s'appelle cette biblioth&egrave;que&thinsp;?", "SARP.Moodle" )

# On termine le questionnaire
finir_xml.moodle( )

# Importez le fichier exemple.categorie.xml dans Moodle pour voir le résultat...
print(
"Importez le fichier exemple.categorie.xml dans Moodle pour voir le résultat..." )
}
}
