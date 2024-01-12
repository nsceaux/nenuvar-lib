tinyQuote = {
  \override Score.StaffSymbol.staff-space = #(magstep -3)
  \set Score.fontSize = #-3
  \override BassFigure.font-size = #-1
}
teenyQuote = {
  \override Score.StaffSymbol.staff-space = #(magstep -8)
  \set Score.fontSize = #-8
  \override BassFigure.font-size = #-5
}

quoteLayout = \layout {
  indent = 0
  ragged-right = ##t
  \context { \Staff \remove "Time_signature_engraver" }
  \context { \RhythmicStaff \remove "Time_signature_engraver" }
  \context { \Voice \override Script.avoid-slur = #'outside }
  \context {
    \Score
    \override StaffGrouper.staff-staff-spacing.basic-distance = #1
    \override BarNumber.break-visibility = #'#(#f #f #t)
  }
  \context {
    \Lyrics
    \override VerticalAxisGroup.nonstaff-relatedstaff-spacing.basic-distance = #0
  }
}

rhythm =
#(define-music-function (parser location music) (ly:music?)
   #{\new RhythmicStaff { \override Voice.Stem.length-fraction = #0.6 \teenyQuote $music }#})

rhythmLayout = \layout {
  indent = 0
  ragged-right = ##t
  \context {
    \RhythmicStaff
    \remove "Time_signature_engraver" 
    \remove "Staff_symbol_engraver"
  }
  \context {
    \Score
    \override StaffSymbol.staff-space = #(magstep -8)
    fontSize = #-8
    \override StaffGrouper.staff-staff-spacing.basic-distance = #1
    \override BarNumber.break-visibility = #'#(#f #f #t)
  }
}
