let pairs_of_line l =
  Scanf.sscanf l "%d-%d,%d-%d" (fun a b c d -> (a,b),(c,d))

let is_one_fully_contained ((a,b),(c,d)) =
  a >= c && b <= d || a<=c && b>=d

let is_overlaping ((a,b),(c,d)) =
  a >= c && a <= d || b >=c && b <= d || c >= a && c <= b || d >= a && d <= b