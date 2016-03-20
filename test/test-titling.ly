\version "2.11.39"
\include "../fancy-headers.ily"
\include "../includes.ily"
\include "../toc-columns.ily"
\include "../titling.ily"
\include "../text-formatting.ily"

#(format #t "~%~s" (rehearsal-number))

\paper { tocTitle = "TABLE DES MATIÈRES" }
#(ly:set-option 'use-rehearsal-numbers #t)
\markuplist\table-of-contents

\opusTitle "Titre de l'œuvre"
\pieceToc "Ouverture"
{ c''1 \pageBreak c'' }

\act "Acte Premier"
\scene "Scène Première" ""
\sceneDescription \markup \wordwrap-center {
  bla bla bla bla bla bla bla bla bla bla
  bla bla bla bla bla bla bla bla bla bla
  bla bla bla bla bla bla bla bla bla bla
  bla bla bla bla bla bla bla bla bla bla
}
\pieceToc "Pièce 1"
{ d'' \pageBreak d'' }
\pieceToc "Pièce 2"
{ e'' }

\scene "Scène Deuxième" "Scène 2"
\pieceToc "Pièce 3"
{ f'' }
\pieceTitleToc
\markup { Pièce 4 avec titre différent }
\markup { Pièce 4 avec toc différent }
{ g'' }
\actEnd\markup { FIN DE L'ACTE PREMIER }

\act "Acte Deuxième"
\scene "Scène Première" ""
\pieceToc ""
{ a'' }
