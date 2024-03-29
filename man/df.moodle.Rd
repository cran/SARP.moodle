\name{df.moodle}
\alias{df.moodle}
\title{
  Convertir une data.frame en questions Moodle
}
\description{
  Cette fonction permet de convertir une data.frame, convenablement
  construit, en un fichier XML de questions pour Moodle
}
\usage{
df.moodle( d,
           colonne.texte = NA, colonne.reponse = NA,
           colonne.note = NA, colonne.note_question = NA,
           colonne.titre = NA,
           colonne.code = NA, colonne.type = NA,
           colonne.retour = NA, colonne.global = NA,
           colonne.penalite = NA,
           colonne.temps = NA, colonne.decimale = NA,
           fichier.xml,
           creer.titre = TRUE, lg.titre = 30,
           embellir = TRUE, deja.HTML = FALSE,
           forcer.multiple = TRUE, melanger.reponses = TRUE,
           somme.nulle = FALSE, precision = 3,
           categorie.base = "",
           dossier.images, 
           sep.images = c( '@@', '@@' ), inserer.images = TRUE,
           sep.formules = c( '@\\\\$', '\\\\$@' ),
           sep.SMILES = c( '@\\\\{', '\\\\}@' ),
           nom.fichier = deparse( substitute( d ) ) )
}
\arguments{
  \item{d}{
    La data.frame à convertir.
  }
  
  \item{colonne.texte}{ Le nom ou le numéro de la colonne qui contient
    l'énoncé des questions. Si elle n'est pas indiquée, une colonne
    intitulée « Texte » est cherchée dans le fichier. Cette colonne est
    obligatoire.  }
  
  \item{colonne.reponse}{
    Le nom ou le numéro de la colonne qui contient la ou les réponses
    aux questions. Si elle n'est pas indiquée, une colonne
    intitulée « Réponse » est cherchée dans le fichier. Cette colonne est
    obligatoire. 
  }
  
  \item{colonne.note}{ Le nom ou le numéro de la colonne qui contient la
    note associée à chaque réponse. Si elle n'est pas indiquée, une
    colonne intitulée « Note » (insensible à la casse) est cherchée dans
    le fichier. Cette colonne est obligatoire en cas de question à
    réponses multiples. Elle peut alors contenir soit le pourcentage de
    la note (voir les détails), soit « Vrai » ou « Faux » (insensible à
    la casse) et les pourcentages sont calculés en conséquence (avec
    toujours 0 pour « Faux » : indiquez explicitement un pourcentage
    négatif si nécessaire).  }
  
  \item{colonne.note_question}{ Le nom ou le numéro de la colonne qui
    contient la note globale de la question. Si elle n'est pas indiquée,
    une colonne intitulée « Note question » (insensible à la casse) est
    cherchée dans le fichier. Cette colonne est facultative. Si elle
    existe, elle doit contenir un entier strictement positif donnant la
    note globale de la question, ou rester vide. Toute note non indiquée
    sera supposée égale à 1 (valeur par défaut de Moodle).}
  
  \item{colonne.titre}{
    Le nom ou le numéro de la colonne qui contient le titre à donner à
    la question dans Moodle. Si elle est manquante, le titre est
    construit, sous la forme \code{xx yy} où \code{yy} correspond aux
    \code{lg.titre} premiers caractères de la question, et \code{xx} au
    code de la question.
  }
  \item{colonne.code}{
    Le nom ou le numéro de la colonne qui contient le code de la
    question. Cette colonne n'est obligatoire que pour créer des
    questions « cloze ». Si elle est manquante, un code interne de la
    forme \code{[Qnn]}, où \code{nn} est le numéro de la question, est
    construit, chaque question correspondant à une ligne avec les deux
    colonnes \code{colonne.texte} et \code{colonne.reponse}
    renseignées. Voyez les détails pour davantage de précisions.
  }
  \item{colonne.type}{
    Le nom ou le numéro de la colonne qui contient le type de la
    question. Cette colonne peut être utilisée pour indiquer qu'une
    question avec plusieurs réponses proposées n'a qu'une seule réponse
    possible, en indiquant QCU pour au moins une des réponses (question
    à choix unique); QCM permet d'indiquer que l'on peut cocher
    plusieurs réponses (question à choix multiples). Dans ce cas,
    \code{forcer.multiple} est ignoré.
  }
  \item{colonne.retour}{
    Le nom ou le numéro de la colonne qui contient le commentaire à
    afficher pour chaque réponse. Cette colonne est facultative.
  }
  \item{colonne.global}{
    Le nom ou le numéro de la colonne qui contient le commentaire à
    afficher globalement pour la question. Cette colonne est facultative.
  }
    
  \item{colonne.penalite}{ Le nom ou le numéro de la colonne qui
    contient la pénalité à mettre en cas de nouvelle tentative de la
    question. Si elle n'est pas indiquée, une colonne intitulée «
    Pénalité » (insensible à la casse) est cherchée dans le
    fichier. Cette colonne est facultative. Si elle existe, elle doit
    contenir la fraction de la note globale de la
    question qui sera prise en compte pour noter la nouvelle tentative
    (par exemple, si la question a une note globale de 2 et la pénalité
    est de 0,5, à la seconde tentative la note maximale possible sera de
    1) ou rester vide. Toute pénalité non indiquée sera supposée
    égale à 1 (pas de pénalité en cas de nouvelle tentative).
  }

  \item{colonne.temps}{ Le nom ou le numéro de la colonne qui contient
    le temps conseillé pour la question. Cette colonne est
    facultative.  Lorsqu'elle est présente, et renseignée, un texte
    indiquant le temps conseillé pour répondre est ajouté à l'énoncé de
    la question.
  }

  \item{colonne.decimale}{ Le nom ou le numéro de la colonne qui contient
    le nombre de chiffres après la virgule attendu pour la réponse à la
    question (pour les questions numériques uniquement). Cette colonne
    est facultative.  }

  \item{fichier.xml}{ Le nom du fichier XML, créé avec
    \code{\link{debuter_xml.moodle}}, dans lequel créer les questions
    que contient la data.frame.  }
  
  \item{creer.titre}{
    Une valeur logique indiquant si l'on veut créer un titre pour chaque
    question, à partir de son code et du début de son énoncé
    (\code{TRUE}) ou non. Ignoré si une colonne de titres a été fournie.
  }
  \item{lg.titre}{
    Le nombre de caractères de l'énoncé d'une question à conserver pour
    construire son titre. Ignoré si une colonne de titres a été fournie
    ou si \code{creer.titre=FALSE}.
  }
  \item{embellir,deja.HTML}{
    Actuellement inutilisés, prévus pour une version future
  }
  
  \item{forcer.multiple}{
    Si \code{TRUE}, les questions avec plusieurs réponses sont toujours
    considérées comme des réponses à choix multiples (l'étudiant pourra
    cocher plusieurs réponses). Sinon, si une seule bonne réponse est
    proposée (plus exactement, s'il existe au moins une réponse donnant 100 \% des
    points), la question est à choix unique (l'étudiant ne pourra
    choisir qu'une seule réponse).  Cette option est ignorée pour les
    questions qui auraient une valeur indiquée dans la colonne précisée
    dans \code{colonne.type}.
  }

  \item{melanger.reponses}{
    Si \code{TRUE}, autorise Moodle à permuter aléatoirement l'ordre des
    réponses lorsqu'il pose la question. Sinon, l'ordre du fichier est
    tout le temps utilisé.
  }

  \item{somme.nulle}{
    Cette option précise comment gérer les réponses incorrectes dans un
    QCM.  Si \code{FALSE}, la note de ces réponses est déterminée à
    partir du fichier, le code FAUX étant associé à une note nulle.
    Si \code{TRUE}, les réponses incorrectes se voient attribuer une
    note négative telle que la somme de ces notes vaut -1, de sorte que
    si l'étudiant coche toutes les cases du QCM, il a 0 (sauf bien sûr
    si toutes les cases correspondent à des questions correctes...).
  }

  \item{precision}{Le nombre de décimales à donner dans la réponse, pour
    une réponse numérique (utilisez \code{NA} pour une valeur exacte et
    ne pas avoir de message indiquant le nombre de décimales ajouté au
    texte de la question).
  }
  
  \item{categorie.base}{
    La catégorie de base à utiliser pour créer les questions. Elle
    contiendra toutes les sous-catégories du fichier, s'il y en a.
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

  \item{nom.fichier}{ Le nom à utiliser pour créer les titres et la
    catégorie de base des questions générées à partir de cette
    data.frame. }
}
\details{
  Cette fonctions réalise la conversion d'une data.frame
  de questions pour Moodle en un fichier XML. La nature des questions
  est déduite de la structure de la data.frame.

  Cette data.frame doit comporter au moins deux colonnes : l'une avec
  les énoncés des questions et l'autre avec les réponses. Chaque ligne
  correspond à une réponse possible.

  S'il y a un énoncé, mais pas de réponse, cet énoncé est supposé être
  une indication de catégorie (sauf si la question a le même code qu'une
  question « cloze », voir plus loin, ou si la colonne précisant le type
  de question indique un type particulier) qui sera créée comme une
  sous-catégorie de la catégorie de base, par un appel à
  \code{\link{categorie.moodle}}.

  Pour les questions simples, la question tient sur une ligne. La nature
  de la question est déduite de la réponse. Si la réponse peut être
  convertie en nombre, la question est supposée être une réponse
  numérique; elle sera créée avec un appel à
  \code{\link{numerique.moodle}}.  Si la réponse est identifiée comme
  une valeur logique (un des textes « V », « F », « T », « Vrai », «
  Faux », « True » ou « False », quelle que soit la casse), la question
  est supposée être une question binaire avec les deux réponses « Vrai »
  et « Faux »; elle sera créée avec un appel à
  \code{\link{vrai_faux.moodle}}.  Dans tous les autres cas, la question
  est supposée être à réponse ouverte et courte et sera créée par un
  appel à \code{\link{qroc.moodle}}.

  S'il y a une réponse, mais pas d'énoncé, cette réponse est supposée
  être une autre réponse possible à une question à choix multiple, dont
  l'énoncé est dans la première ligne précédente qui en contient un.
  Dans ce cas, le fichier doit contenir une colonne qui contient les
  notes associées à chaque réponse. Cette note peut être précise,
  exprimée en pourcentage suivant la convention Moodle, ou indicative,
  avec la simple mention « Vrai » pour les bonnes réponses et « Faux »
  pour les mauvaises. Dans le deuxième cas, les bonnes réponses sont
  associées à l'inverse du nombre de bonnes réponses, de sorte que
  cocher toutes les bonnes réponses donne la note totale; les mauvaises
  réponses sont associées à la note 0 (si \code{somme.nulle=FALSE}:
  cocher ou non ces réponses ne change rien à la note) ou à l'opposé de
  l'inverse du nombre de mauvaises réponses (si \code{somme.nulle=TRUE}:
  cocher ces réponses diminue la note et cocher toutes les réponses
  donne une somme nulle).  Par défaut, la question construite est
  toujours un QCM (plusieurs réponses peuvent être choisies), même s'il
  y a une seule bonne réponse. Pour demander qu'une seule réponse puisse
  être choisie (QCU), indiquez le type QCU dans la colonne indiquée par
  \code{colonne.type} pour la question considérée. Si vous voulez que
  toutes les questions avec une seule bonne réponse soient en QCU, vous
  pouvez aussi utiliser l'option \code{forcer.multiple = FALSE}.

  \strong{Construire des questions \dQuote{\emph{cloze}}}

  Pour pouvoir construire des questions \dQuote{\emph{cloze}}, le
  fichier doit obligatoirement contenir une colonne qui indique le code
  de chaque question (y compris les catégories). Ce code doit être
  unique pour une même question; toutes les lignes qui auront le même
  code seront donc considérées comme faisant partie de la même question.

  De ce fait, si plusieurs lignes d'énoncé renseignées ont le même code,
  elles sont considérées faisant partie de la même question et sont
  utilisées pour construire une question \dQuote{\emph{cloze}}, selon le
  même principe que la fonction \code{\link{question_libre.moodle}}:
  après chaque partie d'énoncé, un champ de réponse est créé, dont la
  bonne réponse est celle indiquée dans la colonne
  \code{colonne.reponse} et sert à définir le type de champ de réponse
  (selon la logique des questions en une ligne).  Pour avoir un texte
  après le dernier champ de réponse, laissez la colonne
  \code{colonne.reponse} vide tout en gardant le même code de question.

  Pour avoir des questions avec réponse à choisir dans une liste, mettez
  une réponse par ligne sans mettre d'énoncé avant, comme pour un QCM.
  La colonne \code{colonne.type} permet de préciser le type d'affichage,
  avec les codes Moodle (\code{MULTICHOICE}, \code{MULTICHOICE_S}...).
  Il est possible d'abréger le code: M correspond à un choix dans un
  menu déroulant (une seule réponse possible ; \code{MULTICHOICE}) ; H,
  à des réponses juxtaposés (\code{MULTICHOICE_H}) ; V, à des réponses
  superposées (\code{MULTICHOICE_V}) ; SA, à un champ libre attendant
  une réponse courte, de type Q. R. O. C. (\code{SHORTANSWER}).  Pour
  les réponses superposées ou juxtaposés, par défaut, une seule réponse
  peut être choisie et l'ordre est celui indiqué dans le fichier. Il est
  possible d'indiquer que plusieurs réponses peuvent être choisies en
  ajoutant le code M (VM, HM) ; il est possible de demander à Moodle de
  tirer au sort l'ordre des réponses en ajoutant le code A (MA, VA,
  VMA...) ou S (MS, VS...).  Pour le champ libre, par défaut la casse
  est ignorée. Il est possible d'indiquer que la casse doit être
  contrôlée grâce au code SAC (\code{SHORTANSWER_C}). L'ordre des codes
  n'a pas d'importance (HM et MH donneront tous deux une question de
  type \code{MULTICHOICE_H}).

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
}

\author{
  Emmanuel Curis \email{emmanuel.curis@parisdescartes.fr}
}
\examples{
  # Conversion du fichier d'exemple fourni
  #  (à placer dans le répertoire de travail)
\dontrun{
  csv.moodle( "exemple_Moodle.csv", colonne.code = "Code" )
}
}
