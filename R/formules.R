## —————————————————————————————————————————————————————————————————
## Création de XML Moodle avec R
## Emmanuel Curis — juin 2015 - mars 2020
##
## Fonctions permettant la création de formules mathématiques
## —————————————————————————————————————————————————————————————————
## HISTORIQUE
##   18 mars 2020 : création du fichier
##
##   24 mars 2020 : densité par défaut, 150 (images trop grandes sinon)
##
##   21 avr. 2020 : insertion de formules chimiques grâce à SMILES/openbabel
##
##   30 avr. 2020 : pour SMILES, possibilité d'indiquer un nom de molécule
##                    qui servira comme description de l'image
## —————————————————————————————————————————————————————————————————

## —————————————————————————————————————————————————————————————————
##
##              Insertion de formule mathématique via latex
## 
## —————————————————————————————————————————————————————————————————

inserer_formule.moodle <- function( formule, marge = 2, displaystyle = TRUE,
                                    couleurs = TRUE, enjoliver = TRUE,
                                    cmd.latex = "latex -shell-escape --interaction errorstopmode",
                                    packages = c( "amsfonts", "amsmath",
                                                  "amssymb" , "dsfont" ),
                                    options.convert = list( 'density' = 150,
                                                            'outext'  = ".png" ) )
{
    ## Le dossier temporaire
    dossier <- tempdir()

    ## ——————————————————————————————————————————————————————————————————————
    ##
    ## Création du fichier latex
    ## 
    
    ## On crée un fichier temporaire tex
    nom.fichier <- tempfile( "formule_", fileext = ".tex" )
    fichier <- file( nom.fichier, "w" )

    ## On y place l'en-tête latex
    ##  1) le type de document : « standalone »
    ##      avec les options de paramétrage
    cat( file = fichier, sep = "",
         "\\documentclass[border=", marge, "pt,",
         "convert={", paste0( names( options.convert ), "=",
                              unlist( options.convert ),
                              collapse = "," ),
         "}]{standalone}\n",
         "\n" )

    ##  2) la liste des packages nécessaires
    if ( length( packages ) > 1 ) {
        cat( file = fichier, sep = "\n",
             paste0( "\\usepackage{", packages, "}",
                     collapse = "\n" ) )
    }
    if ( couleurs ) {
        cat( file = fichier, sep = "\n",
             "\\usepackage[usenames,dvipsnames,svgnames,x11names,table]{xcolor}" )
    }

    ##  4) Quelques commandes pratiques…
    cat( file = fichier, sep = "", "\n",
         "\\newcommand{\\dd}{{\\rm d}}\n", # Élément différentiel
         "\\newcommand{\\prob}[1]{p\\left(#1\\right)}\n", # P()
         "\\newcommand{\\esp}[1]{\\mathbb{E}\\left(#1\\right)}\n", # E()
         "\\newcommand{\\var}[1]{\\mathbb{V}\\left(#1\\right)}\n", # V()
         "\\newcommand{\\ec}[1]{\\sigma\\left(#1\\right)}\n",
         "\\newcommand{\\ind}[1]{\\mathds{1}_{#1}}\n" )

    if ( TRUE == couleurs ) {
        cat( file = fichier, sep = "", "\n",
             "\\definecolor{vert}{rgb}{0.0, 0.5, 0.0}\n",
             "\\definecolor{rouge}{rgb}{1.0, 0.0, 0.0}\n",
             "\\definecolor{bleu}{rgb}{0.0, 0.0, 1.0}\n" )
    }

    ##  3) On commence le document
    cat( file = fichier, sep = "", "\n",
         "\\begin{document}\n" )

    if ( TRUE == enjoliver ) {
        ## On protège la virgule « séparateur décimal »
        formule <- gsub( "([0-9]),([0-9])", 
                         "\\1{,}\\2", formule )

        ## On remplace les symboles d'inégalité par plus joli
        formule <- gsub( "\\leq([^A-Za-z])", "\\leqslant\\1", formule )
        formule <- gsub( "\\geq([^A-Za-z])", "\\geqslant\\1", formule )
    }
    
    ## On met la formule
    cat( file = fichier, sep = "", "\n",
         "$", if ( displaystyle ) "\\displaystyle ",
         formule, "$" )

    ## Le document est terminé...
    cat( file = fichier, sep = "", "\n",
         "\\end{document}\n" )

    ## On ferme le fichier…
    close( fichier ) ; fichier <- NULL

    ## ——————————————————————————————————————————————————————————————————————
    ##
    ## Exécution de la commande latex
    ##
    commande <- paste0( "cd ", dossier, " ; ",
                        cmd.latex, " ", nom.fichier )
    code <- system( commande, ignore.stdout = TRUE, ignore.stderr = TRUE,
                    wait = TRUE )
    if ( code != 0 ) {
        stop( "Erreur dans la g\u00e9n\u00e9ration du DVI ou de l'image\n",
              "Formule : ", formule )
    }
##            show.output.on.console = FALSE ) [Windows seulement]

    ## ——————————————————————————————————————————————————————————————————————
    ##
    ## On construit le code pour insérer l'image...
    ##
    code.XML <- lier_image.moodle( gsub( "\\.tex$", "\\.png", nom.fichier ),
                                   description = formule, interne = TRUE )

    ## On renvoie le code XML
    code.XML
}


## —————————————————————————————————————————————————————————————————
##
##          Insertion de formule semi-développée via open babel
## 
## —————————————————————————————————————————————————————————————————

inserer_SMILES.moodle <- function( code.SMILES, nom.molecule = code.SMILES,
                                   largeur = 300, hauteur = 300,
                                   couleur.atomes = TRUE,
                                   couleur.fond = NA,
                                   couleur.liaisons = NA,
                                   double.liaisons.asymetrique = FALSE,
                                   masquer.terminaux = TRUE,
                                   dessiner.CH = FALSE,
                                   marges = FALSE,
                                   cmd.obabel = "obabel " )
{
    ## Le dossier temporaire
    dossier <- tempdir()

    ## ——————————————————————————————————————————————————————————————————————
    ##
    ## Création de la ligne de commande
    ##
    ## Le code à convertir
    ##   et les dimensions de l'image (300×300 = format par défaut)
 #   print( paste0( "Code à convertir : ", code.SMILES ) )
    options <- paste0( " -:\"", code.SMILES, "\"",
                       " -xw" , largeur, " -xh", hauteur )

    ## Avec des marges ?
    if ( FALSE == marges ) {
        options <- paste0( options, " -xm" )
    }
    
    ## Veut-on les couleurs des atomes selon leur nature?
    ##   (par défaut : oui)
    if ( FALSE == couleur.atomes ) {
        options <- paste0( options, " -xu" )
    }

    ## Couleur du fond
    if ( !is.na( couleur.fond ) ) {
        options <- paste0( options, " -xb ", couleur.fond )
    }

    ## Couleur des liaisons
    if ( !is.na( couleur.liaisons ) ) {
        options <- paste0( options, " -xB ", couleur.liaisons )
    }

    ## Veut-on préciser les chaînes terminales ?
    if ( masquer.terminaux ) {
        options <- paste0( options, " -xC" )
    }

    ## Veut-on afficher les carbones ?
    if ( dessiner.CH ) {
        options <- paste0( options, " -xa" )
    }

    ## Veut-on des doubles liaisons asymétriques ?
    if ( double.liaisons.asymetrique ) {
        options <- paste0( options, " -xs" )
    }

    ## On génère un nom pour le fichier image
    nom.fichier <- tempfile( "SMILES_", fileext = ".png" )

    ## ——————————————————————————————————————————————————————————————————————
    ##
    ## Exécution de la commande open Babel
    ##
#    print( options )
    commande <- paste0( # "cd ", dossier, " ; ",
                        cmd.obabel, " ", options, " -O ", nom.fichier )
    code <- system( commande, ignore.stdout = TRUE, ignore.stderr = FALSE,
                    wait = TRUE )
    if ( code != 0 ) {
        stop( "Erreur dans la g\u00e9n\u00e9ration de l'image\n",
              "Formule : ", code.SMILES )
    }
##            show.output.on.console = FALSE ) [Windows seulement]

    ## Si on ne veut pas de marge : on coupe avec convert
    if ( FALSE == marges ) {
#        print( "On va ôter les marges" )
        commande <- paste0( # "cd ", dossier, " ; ",
                            "convert ", nom.fichier, " -trim ", nom.fichier, "_tr.png ; ",
                            "mv -f ", nom.fichier, "_tr.png ", nom.fichier )
        code <- system( commande, ignore.stdout = TRUE, ignore.stderr = TRUE,
                        wait = TRUE )
        if ( code != 0 ) {
            stop( "Erreur dans le rognage de l'image\n",
                  "Formule : ", code.SMILES )
        }
    }
    
    ## ——————————————————————————————————————————————————————————————————————
    ##
    ## On construit le code pour insérer l'image...
    ##
    code.XML <- lier_image.moodle( nom.fichier,
                                   description = nom.molecule, interne = TRUE )

    ## On renvoie le code XML
    code.XML
}
