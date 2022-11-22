\name{description.moodle}
\alias{description.moodle}
\title{
  Créer une question Moodle contenant simplement du texte
}
\description{
  Cette fonction permet de créer une question de type description pour Moodle.}
\usage{
description.moodle( texte,
                    titre = "Description",
                    commentaire.global = NA, 
                    idnum = NA, 
                    fichier.xml = get( "fichier.xml", envir = SARP.Moodle.env ) )

}
\arguments{
  \item{texte}{ Le texte de la question (une chaîne de caractères). Cet
    énoncé peut contenir n'importe quel caractère (codage en UTF-8) et
    n'importe quelles balises HTML. }

  \item{titre}{ L'intitulé de la question, en HTML. }

  \item{fichier.xml}{ Le fichier correspondant au questionnaire qui
    contiendra cette question (par défaut, le dernier créé).  }

  \item{commentaire.global,idnum}{Option commune à toutes les questions
    Moodle, voyez \code{\link{debut_question.moodle}}.}
}