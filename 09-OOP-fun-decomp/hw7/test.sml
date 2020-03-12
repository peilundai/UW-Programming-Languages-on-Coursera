datatype geom_exp = 
    NoPoints
  | Point of real * real (* represents point (x,y) *)
  | Line of real * real (* represents line (slope, intercept) *)
  | VerticalLine of real (* x value *)
  | LineSegment of real * real * real * real (* x1,y1 to x2,y2 *)
  | Intersect of geom_exp * geom_exp (* intersection expression *)
  | Let of string * geom_exp * geom_exp (* let s = e1 in e2 *)
  | Var of string
  | Shift of real * real * geom_exp (*deltaX, deltaY, subexpression*)



