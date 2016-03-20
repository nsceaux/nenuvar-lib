tinyQuote = {
  \override Score.StaffSymbol.staff-space = #(magstep -3)
  \set Score.fontSize = #-3
  \override BassFigure.font-size = #-1
}

quoteLayout = \layout {
  indent = 0
  ragged-right = ##t
  \context { \Staff \remove "Time_signature_engraver" }
  \context { \Voice \override Script #'avoid-slur = #'outside }
  \context {
    \Score
    \override StaffGrouper.staff-staff-spacing.basic-distance = #1
    \override BarNumber.break-visibility = #'#(#f #f #t)
  }
}
