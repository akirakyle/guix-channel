(define-module (akira tex)
 #:use-module (guix packages)
 #:use-module (guix licenses)
 #:use-module (gnu packages tex))

(define simple-texlive-package (@@ (gnu packages tex) simple-texlive-package))

(define-public texlive-physics
  (package
    (inherit (simple-texlive-package "texlive-physics"
                                     (list "doc/latex/physics/"
                                           "tex/latex/physics/")
                                     (base32
                                      "1wy58wwcv1pv18xs1n71abnm73dqnxqijxvhfxk0rcmvbc6wvwrb")
                                     #:trivial? #t))
    (home-page "https://ctan.org/macros/latex/contrib/physics")
    (synopsis "Macros supporting the Mathematics of Physics")
    (description
     "The package defines simple and flexible macros for typesetting equations in the
languages of vector calculus and linear algebra, using Dirac notation.")
    (license lppl)))

(define-public texlive-braket
  (package
    (inherit (simple-texlive-package "texlive-braket"
                                     (list "doc/latex/braket/"
                                           "tex/latex/braket/")
                                     (base32
                                      "092wk3zc6zajgi6l9h9zna3ksk6k6y4gi8slnfk6vldndcg5sbqn")
                                     #:trivial? #t))
    (home-page "https://ctan.org/macros/latex/contrib/braket")
    (synopsis "Dirac bra-ket and set notations")
    (description
     "This package provides macros to typeset bra-ket notation, as well as set
specifiers, with a single (\"|\") or a double (\"||\" or (\"\\|\") vertical bar
specifier in between two bracketed parts.  Each macro comes in a fixed-size
version and an expanding version.  If the package finds itself operating under
e-tex, it uses the extended primitive \\middle for more reliable results")
    (license public-domain)))

(define-public texlive-svg
  (package
    (inherit (simple-texlive-package "texlive-svg"
                                     (list "doc/latex/svg/" "source/latex/svg/"
                                           "tex/latex/svg/")
                                     (base32
                                      "1yizgrjn6l9j1cf8mvkjz0zni7bzmajszc1y8q80xc723nwnbq7q")
                                     #:trivial? #t))
    (home-page "https://ctan.org/graphics/svg")
    (synopsis "Include and extract SVG pictures in LaTeX documents")
    (description
     "This bundle contains the two packages svg and svg-extract.  The svg package is
intended for the automated integration of SVG graphics into LaTeX documents.
Therefore the capabilities provided by Inkscape -- or more precisely its command
line tool -- are used to export the text within an SVG graphic to a separate
file, which is then rendered by LaTeX.  For this purpose the two commands
\\includesvg and \\includeinkscape are provided which are very similar to the
\\includegraphics command of the graphicx package.  In addition, the package
svg-extract allows the extraction of these graphics into independent files in
different graphic formats, exactly as it is rendered within the LaTeX document,
using either ImageMagick or Ghostscript.")
    (license lppl1.3c)))

(define-public texlive-mathtools
  (package
    (inherit (simple-texlive-package "texlive-mathtools"
                                     (list "doc/latex/mathtools/"
                                           "source/latex/mathtools/"
                                           "tex/latex/mathtools/")
                                     (base32
                                      "1gw9dazsi1h6zrb908msblyq1z366maxqxl5azj7bdinzk20995m")
                                     #:trivial? #t))
    (home-page "https://ctan.org/macros/latex/contrib/mathtools")
    (synopsis "Mathematical tools to use with amsmath")
    (description
     "Mathtools provides a series of packages designed to enhance the appearance of
documents containing a lot of mathematics.  The main backbone is amsmath, so
those unfamiliar with this required part of the LaTeX system will probably not
find the packages very useful.  Mathtools provides many useful tools for
mathematical typesetting.  It is based on amsmath and fixes various deficiencies
of amsmath and standard LaTeX.  It provides: Extensible symbols, such as
brackets, arrows, harpoons, etc.; Various symbols such as \\coloneqq (:=); Easy
creation of new tag forms; Showing equation numbers only for referenced
equations; Extensible arrows, harpoons and hookarrows; Starred versions of the
amsmath matrix environments for specifying the column alignment; More building
blocks: multlined, cases-like environments, new gathered environments; Maths
versions of \\makebox, \\llap, \\rlap etc.; Cramped math styles; and more...
Mathtools requires mhsetup.")
    (license lppl1.3+)))

(define-public texlive-setspace
  (package
    (inherit (simple-texlive-package "texlive-setspace"
                                     (list "doc/latex/setspace/"
                                           "tex/latex/setspace/")
                                     (base32
                                      "00ik8qgkw3ivh3z827zjf7gbwkbsmdcmv22c6ap543mpgaqqjcfm")
                                     #:trivial? #t))
    (home-page "https://ctan.org/macros/latex/contrib/setspace")
    (synopsis "Set space between lines")
    (description
     "This package provides support for setting the spacing between lines in a
document.  Package options include singlespacing, onehalfspacing, and
doublespacing.  Alternatively the spacing can be changed as required with the
\\singlespacing, \\onehalfspacing, and \\doublespacing commands.  Other size
spacings also available.")
    (license lppl1.3+)))

(define-public texlive-moreverb
  (package
    (inherit (simple-texlive-package "texlive-moreverb"
                                     (list "doc/latex/moreverb/"
                                           "source/latex/moreverb/"
                                           "tex/latex/moreverb/")
                                     (base32
                                      "0kba3df9cfiz168hsxhwg3a838p3vrgfp42fmwafsf3xq3q2z5hg")
                                     #:trivial? #t))
    (home-page "https://ctan.org/macros/latex/contrib/moreverb")
    (synopsis "Extended verbatim")
    (description
     "This package provides a collection of verbatim facilities that provide
line-numbered verbatim, verbatim that obeys TAB characters, verbatim input and
verbatim output to file.  The package makes use of the LaTeX required verbatim
package.  The package is formed from a series of small pieces, and is somewhat
unstructured.  The user who looks for thought-through verbatim facilities is
advised to consider using the fancyvrb package in place of moreverb.")
    (license lppl)))


(define-public texlive-bbold
  (package
    (inherit (simple-texlive-package "texlive-bbold"
                                     (list "doc/latex/bbold/"
                                           "fonts/source/public/bbold/"
                                           "fonts/tfm/public/bbold/"
                                           "source/latex/bbold/"
                                           "tex/latex/bbold/")
                                     (base32
                                      "0x3fhz582xcv33s9yiwka82j8bz3nxribgmni3j8j03r6dih8d8r")
                                     #:trivial? #t))
    (home-page "https://ctan.org/fonts/bbold")
    (synopsis "Sans serif blackboard bold")
    (description
     "This package provides a geometric sans serif blackboard bold font, for use in
mathematics; Metafont sources are provided, as well as macros for use with
LaTeX.  The Sauter font package has Metafont parameter source files for building
the fonts at more sizes than you could reasonably imagine.  See the blackboard
sampler for a feel for the font's appearance.")
    (license bsd-3)))

(define-public texlive-revtex
  (package
   (inherit (simple-texlive-package "texlive-revtex"
                                    (list "bibtex/bst/revtex/"
                                          "doc/latex/revtex/"
                                          "source/latex/revtex/"
                                          "tex/latex/revtex/")
                                    (base32
                                     "0w4vmrghnhs8bgpbdp2rzsh4b7hgvldyzkd870wa27k9wk2lk6a1")
                                    #:trivial? #t))
   (propagated-inputs
    (list texlive-textcase
          texlive-latex-natbib
          ;texlive-endfloat
          ))
   (home-page "https://ctan.org/macros/latex/contrib/revtex")
   (synopsis "Styles for various Physics Journals")
   (description
    "Includes styles for American Physical Society, American Institute of Physics,
and Optical Society of America.  The distribution consists of the RevTeX class
itself, and several support packages.")
   (license lppl1.3c)))

(define-public texlive-tikz-cd
  (package
    (inherit (simple-texlive-package "texlive-tikz-cd"
                                     (list "doc/latex/tikz-cd/"
                                           "tex/generic/tikz-cd/"
                                           "tex/latex/tikz-cd/")
                                     (base32
                                      "0yihh47a85khxmxl95hlsfma8n33yj95hsccsaq41zrap72psv37")
                                     #:trivial? #t))
    (home-page "https://ctan.org/graphics/pgf/contrib/tikz-cd")
    (synopsis "Create commutative diagrams with TikZ")
    (description
     "The general-purpose drawing package TiKZ can be used to typeset commutative
diagrams and other kinds of mathematical pictures, generating high-quality
results.  The purpose of this package is to make the process of creation of such
diagrams easier by providing a convenient set of macros and reasonable default
settings.  This package also includes an arrow tip library that match closely
the arrows present in the Computer Modern typeface.")
    (license gpl3)))

(define-public texlive-quantikz
  (package
    (inherit (simple-texlive-package "texlive-quantikz"
                                     (list "doc/latex/quantikz/"
                                           "tex/latex/quantikz/")
                                     (base32
                                      "1pa9ry2sn70sjkxqj0f569148xfc5iq77rw0sjnd344m3xsz38db")
                                     #:trivial? #t))
    (home-page "https://ctan.org/graphics/pgf/contrib/quantikz")
    (synopsis "Draw quantum circuit diagrams")
    (description
     "The purpose of this package is to extend TikZ with the functionality for drawing
quantum circuit diagrams.")
    (license cc-by4.0))) ;FIXME

(define-public texlive-xargs
  (package
    (inherit (simple-texlive-package "texlive-xargs"
                                     (list "doc/latex/xargs/"
                                           "source/latex/xargs/"
                                           "tex/latex/xargs/")
                                     (base32
                                      "1gbdnc1k819fncvnhzihx9q6qdxsrkpfjy47dh70bdwqf5klhqbh")
                                     #:trivial? #t))
    (home-page "https://ctan.org/macros/latex/contrib/xargs")
    (synopsis "Define commands with many optional arguments")
    (description
     "The package provides extended versions of \\newcommand and related LaTeX
commands, which allow easy and robust definition of macros with many optional
arguments, using a clear and simple xkeyval-style syntax.")
    (license lppl)))

(define-public texlive-xstring
  (package
    (inherit (simple-texlive-package "texlive-xstring"
                                     (list "doc/generic/xstring/"
                                           "tex/generic/xstring/")
                                     (base32
                                      "1azpq855kq1l4686bjp8haxim5c8wycz1b6lcg5q7x8kb4g9sppn")
                                     #:trivial? #t))
    (home-page "https://ctan.org/macros/generic/xstring")
    (synopsis "String manipulation for (La)TeX")
    (description
     "The package provides macros for manipulating strings -- testing a string's
contents, extracting substrings, substitution of substrings and providing
numbers such as string length, position of, or number of recurrences of, a
substring.  The package works equally in Plain TeX and LaTeX (though e-TeX is
always required).  The strings to be processed may contain (expandable) macros.")
    (license lppl1.3c)))


(define-public texlive-doublestroke
  (package
    (inherit (simple-texlive-package "texlive-doublestroke"
                                     (list "doc/fonts/doublestroke/"
                                           "fonts/map/dvips/doublestroke/"
                                           "fonts/source/public/doublestroke/"
                                           "fonts/tfm/public/doublestroke/"
                                           "fonts/type1/public/doublestroke/"
                                           "tex/latex/doublestroke/")
                                     (base32
                                      "0v9g025l0qfw4zrjkm9yypcsramwl2di997jgnznxpxms0v6ib7c")
                                     #:trivial? #t))
    (home-page "https://ctan.org/fonts/doublestroke")
    (synopsis "Typeset mathematical double stroke symbols")
    (description
     "This package provides a font based on Computer Modern Roman useful for
typesetting the mathematical symbols for the natural numbers (N), whole numbers
(Z), rational numbers (Q), real numbers (R) and complex numbers (C); coverage
includes all Roman capital letters, '1', 'h' and 'k'.  The font is available
both as Metafont source and in Adobe Type 1 format, and LaTeX macros for its use
are provided.  The fonts appear in the blackboard bold sampler.")
    (license public-domain))) ; not sure if this is right

(define-public texlive-comment
  (package
    (inherit (simple-texlive-package "texlive-comment"
                                     (list "doc/latex/comment/"
                                           "tex/latex/comment/")
                                     (base32
                                      "1c1mqziwxyf1bqzpw6ji65n7ypygm3lyknblxmf0c70w0ivw76pa")
                                     #:trivial? #t))
    (home-page "https://ctan.org/macros/latex/contrib/comment")
    (synopsis "Selectively include/exclude portions of text")
    (description
     "Selectively include/exclude pieces of text, allowing the user to define new,
separately controlled, comment versions.  All text between \\comment ...
\\endcomment or \\begin{comment} ... \\end{comment} is discarded.  The opening and
closing commands should appear on a line of their own.  No starting spaces,
nothing after it.  This environment should work with arbitrary amounts of
comment, and the comment can be arbitrary text.  Other 'comment' environments
are defined and selected/deselected with \\includecomment{versiona} and
\\excludecoment{versionb} These environments are used as \\versiona ...
\\endversiona or \\begin{versiona} ... \\end{versiona} with the opening and closing
commands again on a line of their own.")
    (license gpl2)))

(define-public texlive-subfig
  (package
    (inherit (simple-texlive-package "texlive-subfig"
                                     (list "doc/latex/subfig/"
                                           "source/latex/subfig/"
                                           "tex/latex/subfig/")
                                     (base32
                                      "0bq1328pb1ak91j7q8n1kh2fncr742lvff7apgf8kkxzxjfg2z9r")
                                     #:trivial? #t))
    (home-page "https://ctan.org/macros/latex/contrib/subfig")
    (synopsis "Figures broken into subfigures")
    (description
     "The package provides support for the manipulation and reference of small or
'sub' figures and tables within a single figure or table environment.  It is
convenient to use this package when your subfigures are to be separately
captioned, referenced, or are to be included in the List-of-Figures.  A new
\\subfigure command is introduced which can be used inside a figure environment
for each subfigure.  An optional first argument is used as the caption for that
subfigure.  This package supersedes the subfigure package (which is no longer
maintained).  The name was changed since the package is not completely backward
compatible with the older package The major advantage of the new package is that
the user interface is keyword/value driven and easier to use.  To ease the
transition from the subfigure package, the distribution includes a configuration
file (subfig.cfg) which nearly emulates the subfigure package.  The
functionality of the package is provided by the (more recent still) subcaption
package.")
    (license lppl)))

(define-public texlive-todonotes
  (package
    (inherit (simple-texlive-package "texlive-todonotes"
                                     (list "doc/latex/todonotes/"
                                           "source/latex/todonotes/"
                                           "tex/latex/todonotes/")
                                     (base32
                                      "0lhqzrvf216j3rzg7lmc1mvnr2mzr0a6c2kqrfwzw6qbpm9v29nk")
                                     #:trivial? #t))
    (propagated-inputs (list texlive-latex-xkeyval texlive-xcolor texlive-tools
                             texlive-pgf))
    (home-page "https://ctan.org/macros/latex/contrib/todonotes")
    (synopsis "Marking things to do in a LaTeX document")
    (description
     "The package lets the user mark things to do later, in a simple and visually
appealing way.  The package takes several options to enable
customization/finetuning of the visual appearance.")
    (license lppl1.3+)))


(define-public texlive-catchfile
  (package
    (inherit (simple-texlive-package "texlive-catchfile"
                                     (list "doc/latex/catchfile/"
                                           "source/latex/catchfile/"
                                           "tex/generic/catchfile/")
                                     (base32
                                      "1dpxy64hs0bjp8d2dmikflc995vazf7fi6z92w51fnj2fidgl8gx")
                                     #:trivial? #t))
    (home-page "https://ctan.org/macros/latex/contrib/catchfile")
    (synopsis "Catch an external file into a macro")
    (description
     "This package catches the contents of a file and puts it in a macro.  It requires
e-TeX.  Both LaTeX and plain TeX are supported.")
    (license lppl1.3+)))

(define-public texlive-transparent
  (package
    (inherit (simple-texlive-package "texlive-transparent"
                                     (list "doc/latex/transparent/"
                                           "source/latex/transparent/"
                                           "tex/latex/transparent/")
                                     (base32
                                      "172vh8fdrf67inzyrmah0kr1jdc8b3v9f18qrcrgabybhrh5j7qk")
                                     #:trivial? #t))
    (home-page "https://ctan.org/macros/latex/contrib/transparent")
    (synopsis "Using a color stack for transparency with pdfTeX")
    (description
     "Since version 1.40 pdfTeX supports several color stacks; the package uses a
separate colour stack for control of transparency (which is not, of course, a
colour).")
    (license lppl1.3+)))

(define-public texlive-minted
  (package
    (inherit (simple-texlive-package "texlive-minted"
                                     (list "doc/latex/minted/"
                                           "source/latex/minted/"
                                           "tex/latex/minted/")
                                     (base32
                                      "13cjsjb3b04n9arwp46ayk8fcicylxq5g1864cpxl1lxjxh1yi0l")
                                     #:trivial? #t))
    (propagated-inputs (list texlive-fvextra
                             texlive-latex-fancyvrb
                             texlive-lineno
                             texlive-latex-framed))
    (home-page "https://ctan.org/macros/latex/contrib/minted")
    (synopsis "Highlighted source code for LaTeX")
    (description
     "The package that facilitates expressive syntax highlighting in LaTeX using the
powerful Pygments library.  The package also provides options to customize the
highlighted source code output using fancyvrb.")
    (license lppl1.3+)))

(define-public texlive-fvextra
  (package
    (inherit (simple-texlive-package "texlive-fvextra"
                                     (list "doc/latex/fvextra/"
                                           "source/latex/fvextra/"
                                           "tex/latex/fvextra/")
                                     (base32
                                      "0nawx1fh55yhqspy5jgss2qmwpqmikfrg7628smk931rph9nq0aa")
                                     #:trivial? #t))
    (propagated-inputs (list texlive-latex-upquote))
    (home-page "https://ctan.org/macros/latex/contrib/fvextra")
    (synopsis "Extensions and patches for fancyvrb")
    (description
     "fvextra provides several extensions to fancyvrb, including automatic line
breaking and improved math mode.  It also patches some fancyvrb internals.
Parts of fvextra were originally developed as part of pythontex and minted.")
    (license lppl1.3+)))

(define-public texlive-beamertheme-metropolis
  (package
    (inherit (simple-texlive-package "texlive-beamertheme-metropolis"
                                     (list "doc/latex/beamertheme-metropolis/"
                                           "source/latex/beamertheme-metropolis/"
                                           "tex/latex/beamertheme-metropolis/")
                                     (base32
                                      "1vx9w44d5vjb6nr1ic14hknm1j38i02v0nbawwa3xn1z3i4xs8cj")
                                     #:trivial? #t))
    (propagated-inputs (list texlive-pgfopts))
    (home-page
     "https://ctan.org/macros/latex/contrib/beamer-contrib/themes/metropolis")
    (synopsis "A modern LaTeX beamer theme")
    (description
     "The package provides a simple, modern Beamer theme for anyone to use.  It tries
to minimize noise and maximize space for content.")
    (license cc-by-sa4.0)))

(define-public texlive-pgfopts
  (package
    (inherit (simple-texlive-package "texlive-pgfopts"
                                     (list "doc/latex/pgfopts/"
                                           "source/latex/pgfopts/"
                                           "tex/latex/pgfopts/")
                                     (base32
                                      "0p4bfqgkwmzhismrs7a10sblbgx4b6w259vdp1dd3hxvhc2kbnyn")
                                     #:trivial? #t))
    (propagated-inputs (list texlive-pgf))
    (home-page "https://ctan.org/macros/latex/contrib/pgfopts")
    (synopsis "LaTeX package options with pgfkeys")
    (description
     "The pgfkeys package (part of the pgf distribution) is a well-designed way of
defining and using large numbers of keys for key-value syntaxes.  However,
pgfkeys itself does not offer means of handling LaTeX class and package options.
 This package adds such option handling to pgfkeys, in the same way that
kvoptions adds the same facility to the LaTeX standard keyval package.")
    (license lppl1.3+)))

(define-public texlive-beamerposter
  (package
    (inherit (simple-texlive-package "texlive-beamerposter"
                                     (list "doc/latex/beamerposter/"
                                           "tex/latex/beamerposter/")
                                     (base32
                                      "1xp8a6d82n1kgagdc7mm7hjihdzn1k7y4lijy924hjdvnvdmqa2i")
                                     #:trivial? #t))
    (propagated-inputs (list texlive-fp texlive-latex-type1cm))
    (home-page "https://ctan.org/macros/latex/contrib/beamerposter")
    (synopsis "Extend beamer and a0poster for custom sized posters")
    (description
     "The package enables the user to use beamer style operations on a canvas of the
sizes provided by a0poster; font scaling is available (using packages such as
type1cm if necessary).  In addition, the package allows the user to benefit from
the nice colour box handling and alignment provided by the beamer class (for
example, with rounded corners and shadows).  Good looking posters may be created
very rapidly.  Features include: scalable fonts using the fp and type1cm
packages; posters in A-series sizes, and custom sizes like double A0 are
possible; still applicable to custom beamer slides, e.g.  16:9 slides for a
wide-screen (i.e.  1.78 aspect ratio); orientation may be portrait or landscape;
a 'debug mode' is provided.")
    (license lppl1.3+)))

(define-public texlive-fp
  (package
    (inherit (simple-texlive-package "texlive-fp"
                                     (list "doc/latex/fp/" "tex/latex/fp/"
                                           "tex/plain/fp/")
                                     (base32
                                      "1q555fx71cf88sn3npzb0j2i10ak920k0qc9ccdygz99vqg10dad")
                                     #:trivial? #t))
    (home-page "https://ctan.org/macros/latex/contrib/fp")
    (synopsis "Fixed point arithmetic")
    (description
     "An extensive collection of arithmetic operations for fixed point real numbers of
high precision.")
    (license lppl)))


(define-public my-texlive-fontspec
  (package
    (inherit (simple-texlive-package "texlive-fontspec"
                                     (list "doc/latex/fontspec/"
                                           "source/latex/fontspec/"
                                           "tex/latex/fontspec/")
                                     (base32
                                      "1k999jgdd4a9d20rywl53vzpvl3synqxik1fiskxwzlzibjlibv1")
                                     #:trivial? #t))
    (propagated-inputs (list
                        ;texlive-xunicode
                        texlive-lm
                        texlive-latex-l3packages
                        texlive-generic-iftex
                        ;texlive-euenc
                        ))
    (home-page "https://ctan.org/macros/unicodetex/latex/fontspec")
    (synopsis "Advanced font selection in XeLaTeX and LuaLaTeX")
    (description
     "Fontspec is a package for XeLaTeX and LuaLaTeX.  It provides an automatic and
unified interface to feature-rich AAT and OpenType fonts through the NFSS in
LaTeX running on XeTeX or LuaTeX engines.  The package requires the l3kernel and
xparse bundles from the LaTeX3 development team.")
    (license lppl1.3c)))

(define-public texlive-moderncv
  (package
    (inherit (simple-texlive-package "texlive-moderncv"
                                     (list "doc/latex/moderncv/"
                                           "tex/latex/moderncv/")
                                     (base32
                                      "12rbl52a7rlx9agrl4rrcf2jydpnr74x4f8x1bbnb9b31zdrwbvw")
                                     #:trivial? #t))
    (propagated-inputs (list texlive-xcolor
                             texlive-latex-colortbl
                             texlive-url
                             texlive-tools
                             texlive-microtype
                             texlive-latex-l3packages
                             texlive-generic-iftex
                             texlive-hyperref
                             texlive-latex-graphics
                             texlive-latex-fancyhdr
                             texlive-etoolbox
                             texlive-arydshln))
    (home-page "https://ctan.org/macros/latex/contrib/moderncv")
    (synopsis "A modern curriculum vitae class")
    (description
     "The class provides facilities for typesetting modern curriculums vitae, both in
a classic and in a casual style.  It is fairly customizable, allowing you to
define your own style by changing the colours, the fonts, etc.  The template.tex
file can be used as an example.")
    (license lppl1.3c)))

(define-public texlive-arydshln
  (package
    (inherit (simple-texlive-package "texlive-arydshln"
                                     (list "doc/latex/arydshln/"
                                           "source/latex/arydshln/"
                                           "tex/latex/arydshln/")
                                     (base32
                                      "0zhn8fq92ghkci457qmls90yd2q55zfgqd6rxyhzl5nsfhamcrvh")
                                     #:trivial? #t))
    (home-page "https://ctan.org/macros/latex/contrib/arydshln")
    (synopsis "Draw dash-lines in array/tabular")
    (description
     "The package is to draw dash-lines in array/tabular environments.  Horizontal
lines are drawn by \\hdashline and \\cdashline while vertical ones can be
specified as a part of the preamble using ':'.  The shape of dash-lines may be
controlled through style parameters or optional arguments.  The package is
compatible with array, colortab, longtable, and colortbl.")
    (license lppl1.0+)))

(define-public texlive-fontawesome
  (package
    (inherit (simple-texlive-package "texlive-fontawesome"
                                     (list "doc/fonts/fontawesome/"
                                           "fonts/enc/dvips/fontawesome/"
                                           "fonts/map/dvips/fontawesome/"
                                           "fonts/opentype/public/fontawesome/"
                                           "fonts/tfm/public/fontawesome/"
                                           "fonts/type1/public/fontawesome/"
                                           "tex/latex/fontawesome/")
                                     (base32
                                      "0m3wl0jc00h8r4w3fa5vkf062hmaadb2rvf3x9lm4pb0c99ia5x9")
                                     #:trivial? #t))
    (home-page "https://ctan.org/fonts/fontawesome")
    (synopsis "Font containing web-related icons")
    (description
     "The package offers access to the large number of web-related icons provided by
the included font.  The package requires the package, fontspec, if run with
XeTeX or LuaTeX.")))

(define-public texlive-sansmathfonts
  (package
    (inherit (simple-texlive-package "texlive-sansmathfonts"
                                     (list "doc/fonts/sansmathfonts/"
                                           "fonts/map/dvips/sansmathfonts/"
                                           "fonts/source/public/sansmathfonts/"
                                           "fonts/tfm/public/sansmathfonts/"
                                           "fonts/type1/public/sansmathfonts/"
                                           "fonts/vf/public/sansmathfonts/"
                                           "tex/latex/sansmathfonts/")
                                     (base32
                                      "1l6q26590kdr2b24psdwgjw199p3sgk2hh74gq6fd6qircc1z3cy")
                                     #:trivial? #t))
    (home-page "https://ctan.org/fonts/sansmathfonts")
    (synopsis "Correct placement of accents in sans-serif maths")
    (description
     "Sans serif small caps and math fonts for use with Computer Modern.")
    (license lppl1.3c)))

(define-public texlive-fontawesome5
  (package
    (inherit (simple-texlive-package "texlive-fontawesome5"
                                     (list "doc/fonts/fontawesome5/"
                                           "fonts/enc/dvips/fontawesome5/"
                                           "fonts/map/dvips/fontawesome5/"
                                           "fonts/opentype/public/fontawesome5/"
                                           "fonts/tfm/public/fontawesome5/"
                                           "fonts/type1/public/fontawesome5/"
                                           "tex/latex/fontawesome5/")
                                     (base32
                                      "1mxr1vbdiwnzjfy1mkb51zgslwncbgnzc97rxxlrvhxv29bj2lra")
                                     #:trivial? #t))
    (home-page "https://ctan.org/fonts/fontawesome5")
    (synopsis "Font Awesome 5 with LaTeX support")
    (description
     "This package provides LaTeX support for the included \"Font Awesome 5 Free\" icon
set.  These icons were designed by Fort Awesome and released under the SIL OFL
1.1 license.  The commercial \"Pro\" version is also supported, if it is installed
and XeLaTeX or LuaLaTeX is used.")
    (license lppl1.3c)))


(define-public texlive-luatexbase
  (package
    (inherit (simple-texlive-package "texlive-luatexbase"
                                     (list "doc/luatex/luatexbase/"
                                           "source/luatex/luatexbase/"
                                           "tex/luatex/luatexbase/")
                                     (base32
                                      "1nz2k9czqdmn08v75qa2bwanvcvyp9jmqcgwaxcy4fy4mpbrn8ra")
                                     #:trivial? #t))
    (propagated-inputs (list texlive-ctablestack))
    (home-page "https://ctan.org/macros/luatex/generic/luatexbase")
    (synopsis "Basic resource management for LuaTeX code")
    (description
     "The LaTeX kernel (LaTeX2e 2015/10/01 onward) builds in support for LuaTeX
functionality, also available as ltluatex.tex for users of plain TeX and those
with older LaTeX kernel implementations.  This support is based on ideas taken
from the original luatexbase package, but there are interface differences.  This
'stub' package provides a compatibility layer to allow existing packages to
upgrade smoothly to the new support structure.")
    (license lppl1.3+)))

(define-public texlive-ctablestack
  (package
    (inherit (simple-texlive-package "texlive-ctablestack"
                                     (list "doc/luatex/ctablestack/"
                                           "source/luatex/ctablestack/"
                                           "tex/luatex/ctablestack/")
                                     (base32
                                      "13l779436aj3hlchwvhkpiikbyfa2j4swzfrwqkjh9l8bc2cwg7n")
                                     #:trivial? #t))
    (home-page "https://ctan.org/macros/luatex/generic/ctablestack")
    (synopsis "Catcode table stable support")
    (description
     "This package provides a method for defining category code table stacks in
LuaTeX.  It builds on code provided by the 2015/10/01 release of LaTeX2e (also
available as ltluatex.sty for plain users).  It is required by the luatexbase
package (v1.0 onward) which uses ctablestack to provide a back-compatibility
form of this concept.")
    (license lppl1.3+)))

(define-public texlive-academicons
  (package
    (inherit (simple-texlive-package "texlive-academicons"
                                     (list "doc/fonts/academicons/"
                                           "fonts/truetype/public/academicons/"
                                           "tex/latex/academicons/")
                                     (base32
                                      "1qyszkj9w5j65jqibfcsa5x0z28sgr0f51jk4jlq7fhl163jqac0")
                                     #:trivial? #t))
    (home-page "https://ctan.org/fonts/academicons")
    (synopsis "Font containing high quality icons of online academic profiles")
    (description
     "The academicons package provides access in (La)TeX to 112 high quality icons of
online academic profiles included in the free \"Academicons\" font.  This package
requires either the Xe(La)TeX or Lua(La)TeX engine to load the \"Academicons\"
font from the system, which requires installing the bundled academicons.ttf font
file.  As new releases come out, it is recommended to install the bundled font
version as there may be differences between the package and previous font
versions or newest font versions not yet contemplated in the package.  The
\"Academicons\" font was designed by James Walsh and released (see
http://jpswalsh.github.io/academicons/) under the open SIL Open Font License.
This package is a redistribution of the free \"Academicons\" font with specific
bindings for (La)TeX.  It is inspired and based on the fontawesome\" package.
The academicons package provides the generic \\aiicon command to access icons,
which takes as mandatory argument the name of the desired icon.  It also
provides individual direct commands for each specific icon.  The full list of
icons and their respective names and direct commands can be found in the manual.
 For example, \\aiicon{googlescholar} yields the same result as \\aiGoogleScholar.")
    (license lppl1.3c)))

(define-public texlive-cellspace
  (package
    (inherit (simple-texlive-package "texlive-cellspace"
                                     (list "doc/latex/cellspace/"
                                           "tex/latex/cellspace/")
                                     (base32
                                      "1y1vrhqxk0533h31dq4mnd6l3h6ajrpi5mliahgag6k8fil1k8pv")
                                     #:trivial? #t))
    (home-page "https://ctan.org/macros/latex/contrib/cellspace")
    (synopsis "Ensure minimal spacing of table cells")
    (description
     "It is well known that high or deep cells tend to touch the \\hlines of a tabular.
 This package provides a modifier S acting on usual column types so that to
ensure a minimal distance that can be controlled through two parameters
\\cellspacetoplimit and \\cellspacebottomlimit.  The approach employed by this
package is noticeably simpler than that of tabls, which considers the dimensions
of each entire row; whereas you can ask the cellspace only to look at the cells
of potentially difficult columns.  The package depends on ifthen, array, calc,
and xkeyval.")
    (license lppl)))

(define-public texlive-tikz-feynman
  (package
    (inherit (simple-texlive-package "texlive-tikz-feynman"
                                     (list "doc/latex/tikz-feynman/"
                                           "tex/latex/tikz-feynman/")
                                     (base32
                                      "1yndmpghf3z5jddr3zcm5xw7v7zb6715d870ckjd5gifkvyv3nsy")
                                     #:trivial? #t))
    (propagated-inputs (list texlive-pgfopts texlive-generic-iftex))
    (home-page "https://ctan.org/graphics/pgf/contrib/tikz-feynman")
    (synopsis "Feynman diagrams with TikZ")
    (description
     "This is a LaTeX package allowing Feynman diagrams to be easily generated within
LaTeX with minimal user instructions and without the need of external programs.
It builds upon the TikZ package and leverages the graph placement algorithms
from TikZ in order to automate the placement of many vertices.  tikz-feynman
allows fine-tuned placement of vertices so that even complex diagrams can still
be generated with ease.")))

(define-public %all-my-latex-packages
  (list texlive-base
        texlive-luatexbase
        biber
        texlive-wrapfig
        texlive-ulem
        texlive-capt-of
        texlive-lm
        texlive-pgf
        texlive-physics
        texlive-braket
        texlive-revtex
        texlive-amsfonts
        texlive-etoolbox
        texlive-latex-trimspaces
        texlive-xcolor
        texlive-xstring
        texlive-xargs
        texlive-doublestroke
        texlive-latex-float
        texlive-svg
        texlive-standalone
        texlive-listings
        texlive-beamer
        texlive-translator
        texlive-caption
        texlive-enumitem
        texlive-biblatex
        texlive-quantikz
        texlive-latex-environ
        texlive-tikz-cd
        texlive-subfig
        texlive-comment
        texlive-todonotes
        texlive-latex-geometry
        texlive-fonts-ec
        texlive-latex-koma-script
        texlive-latex-ifplatform
        texlive-catchfile
        texlive-transparent
        texlive-latex-titlesec
        texlive-latex-fancyhdr
        texlive-minted
        texlive-beamertheme-metropolis
        texlive-latex-hanging ; for metropolis?
        texlive-grfext ; for metropolis?
        texlive-latex-multirow
        texlive-beamerposter
        texlive-ragged2e ; for beamerposter?
        texlive-everysel ; for beamerposter?
        texlive-latex-changepage
        texlive-underscore ; needed by maptlotlib
        texlive-stringenc
        texlive-cellspace
        texlive-generic-soul
        texlive-moderncv
        texlive-fontawesome5 ; for moderncv
        texlive-academicons ; for moderncv

        my-texlive-fontspec 
        texlive-luaotfload ; this package is broken on guix since it looks for fonts at "/etc/fonts/fonts.conf" instead of using fc-list et al
        ; https://github.com/latex3/luaotfload/blob/79fb28b633691fc7349e1d833317049ba9954e80/src/luaotfload-database.lua#L2411
        ; also ncurses is required to run luaotfload-tool
        ; also the current guix package fails to include fontspec.cfg

        texlive-carlisle ; for slashed.sty
        texlive-tikz-feynman
        ))
