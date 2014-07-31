triBaseLength = 32.0
triRatio      = 1 / 1.61
triBaseHeight = triBaseLength * triRatio
# triShape : Shape
# triShape = 
#   [(-triBaseLength/2,0), (triBaseLength/2,0), (0,triBaseHeight)]
# triShapeFlipped =
#   [(-triBaseLength/2,triBaseHeight), (triBaseLength/2,triBaseHeight), (0,0)]

# triBase : Color -> Coord -> Form
# triBase color (Coord cx cy) = 
#   let
#     stagger = triBaseLength / 2.0
#     oddExtra = 1.0
#     mx    = if
#       | cy `mod` 2 == 0 -> (toFloat cx) * stagger
#       | otherwise       -> (toFloat cx + oddExtra) * stagger
#     my    = (toFloat cy) * triBaseHeight
#     # --shape = if
#     # --  | cy `mod` 2 == 0 -> triShape
#     # --  --| cx `mod` 2 /= cy `mod` 2 -> triShape
#     # --  | otherwise       -> triShapeFlipped
#     shape = if
#       | cx `mod` 2 == 0 -> triShape
#       | otherwise       -> triShapeFlipped
#   in
#     filled color shape |> move (mx, my)

# tris : Color -> Int -> Int -> [Tri]
# tris color w h =
#   let
#     (x0, x1) = (-w `div` 2, ((w+1) `div` 2) - 1)
#     (y0, y1) = (-h `div` 2, ((h+1) `div` 2) - 1)
#     f x y    = let c = (Coord x y) in (Tri c (triBase color c))
#     g x      = map (f x) [y0..y1]
#   in
#     concatMap g [x0..x1]

# triForms : [Tri] -> [Form]
# triForms = map (\(Tri _ f) -> f)

# evenTris : [Tri] -> [Tri]
# evenTris = filter (\(Tri (Coord x y) _) -> x `mod` 2 == 0)

# oddTris : [Tri] -> [Tri]
# oddTris = filter (\(Tri (Coord x y) _) -> x `mod` 2 == 1)


# mapTriForms : (Form -> Form) -> [Tri] -> [Tri]
# mapTriForms f = map (\(Tri c form) -> (Tri c (f form)))

# displaceTris : (Float, Float) -> [Tri] -> [Tri]
# displaceTris xy = mapTriForms (\f -> move xy f)

# colorTris color = mapTriForms (\f -> colorForm color f)


# test : (Int,Int) -> VisState -> Element
# test (w,h) {time, tpb, phase} = 
#   --[triBase (Coord 0 0), triBase (Coord 1 0), triBase (Coord 0 1)]
#   let
#     --(w,h) = (640,360)
#     beatsPerPeriod = 4.0
#     --tpb'  = case tpb of
#     --  Just x  -> x
#     --  Nothing -> 1
#     p     = periodFrac (tpb * beatsPerPeriod) (time - phase)
#     ts    = tris blue (22*2) 20
#     --ts    = tris blue (22*2*2) (20*2)
#     --color = hsl 0 255 (sin (2*pi*p) * 0.25 + 0.75) 
#     color = hsl p 1.0 0.75 
#     --color = greyscale (sin (2*pi*p) * 0.5 + 0.5)
#     --ets   = evenTris ts |> colorTris color
#     --q     = (toFloat (ceiling (4*p))) / 4.0
#     q     = p
#     ets   = evenTris ts 
#       |> colorTris yellow
#       --|> filter (\(Tri (Coord x y) _) -> x == y || (x+1) == y)  
#       |> filter (\(Tri (Coord x y) _) -> x == y)  
#       |> displaceTris (q*triBaseLength * beatsPerPeriod, 0)
#     ots   = oddTris ts
#   in
#     collage (w-200) h <| triForms <| ets ++ ots

export triBaseLength