let modulo x y =
  let result = x mod y in
  if result >= 0 then result else result + y

let clamp_color col = max 0 (min 255 col)
let clamp x _min _max = max _min (min _max x)

let to_rgb r g b =
  if r > 255 || g > 255 || b > 255 || r < 0 || g < 0 || b < 0 then
    raise (Invalid_argument "RGB values must range from 0 to 255");
  (r lsl 16) lor (g lsl 8) lor b

let of_rgb rgb_value =
  ((rgb_value lsr 16) land 255, (rgb_value lsr 8) land 255, rgb_value land 255)

let avg_rgb rgb1 rgb2 =
  let r1, b1, g1 = of_rgb rgb1 and r2, b2, g2 = of_rgb rgb2 in
  to_rgb ((r1 + r2) / 2) ((g1 + g2) / 2) ((b1 + b2) / 2)

let cmyk_to_rgb c m y k =
  if
    c > 1. || m > 1. || y > 1. || k > 1. || c < 0. || m < 0. || y < 0. || k < 0.
  then raise (Invalid_argument "CMYK values must range from 0 to 1");
  to_rgb
    (255. *. (1. -. c) *. (1. -. k) |> int_of_float)
    (255. *. (1. -. m) *. (1. -. k) |> int_of_float)
    (255. *. (1. -. y) *. (1. -. k) |> int_of_float)

let rgb_to_cmyk rgb =
  let r, g, b = of_rgb rgb in
  let r', g', b' = (float r /. 255., float g /. 255., float b /. 255.) in
  let k = 1. -. max (max r' g') b' in
  let c = (1. -. r' -. k) /. (1. -. k)
  and m = (1. -. g' -. k) /. (1. -. k)
  and y = (1. -. b' -. k) /. (1. -. k) in
  (c, m, y, k)

let rgb_to_hsv rgb =
  let r, g, b = of_rgb rgb in
  let r', g', b' = (float r /. 255., float g /. 255., float b /. 255.) in
  let c_min = min (min r' g') b' and c_max = max (max r' g') b' in
  let delta = c_max -. c_min in
  let hue =
    if delta = 0. then 0.
    else if c_max = r' then 60. *. mod_float ((g' -. b') /. delta) 6.
    else if c_max = g' then (60. *. ((b' -. r') /. delta)) +. 120.
    else (60. *. ((r' -. g') /. delta)) +. 240.
  and sat = if c_max = 0. then 0. else delta /. c_max
  and v = c_max in
  (modulo (int_of_float hue) 360, sat, v)

let hsv_to_rgb h s v =
  let h = modulo h 360 in
  let c = v *. s in
  let x = c *. (1. -. (float @@ abs ((h / 60 mod 2) - 1))) and m = v -. c in
  let r', g', b' =
    if 0 <= h && h < 60 then (c, x, 0.)
    else if 60 <= h && h < 120 then (x, c, 0.)
    else if 120 <= h && h < 180 then (0., c, x)
    else if 180 <= h && h < 240 then (0., x, c)
    else if 240 <= h && h < 300 then (x, 0., c)
    else (c, 0., x)
  in
  let r, g, b = ((r' +. m) *. 255., (g' +. m) *. 255., (b' +. m) *. 255.) in
  to_rgb (r |> int_of_float) (g |> int_of_float) (b |> int_of_float)

let rgb_to_hsl rgb =
  let r, g, b = of_rgb rgb in
  let r', g', b' = (float r /. 255., float g /. 255., float b /. 255.) in
  let c_min = min (min r' g') b' and c_max = max (max r' g') b' in
  let delta = c_max -. c_min in
  let l = (c_max -. c_min) /. 2. in
  let hue =
    if delta = 0. then 0.
    else if c_max = r' then 60. *. mod_float ((g' -. b') /. delta) 6.
    else if c_max = g' then (60. *. ((b' -. r') /. delta)) +. 120.
    else (60. *. ((r' -. g') /. delta)) +. 240.
  and sat =
    if l = 0. || l = 1. then 0.
    else delta /. (1. -. abs_float ((2. *. c_max) -. delta -. 1.))
  in
  (modulo (int_of_float hue) 360, sat, l)

let hsl_to_rgb h s l =
  let h = modulo h 360 in
  let c = (1. -. abs_float ((2. *. l) -. 1.)) *. s in
  let x = c *. (1. -. (float @@ abs ((h / 60 mod 2) - 1)))
  and m = l -. (c /. 2.) in
  let r', g', b' =
    if 0 <= h && h < 60 then (c, x, 0.)
    else if 60 <= h && h < 120 then (x, c, 0.)
    else if 120 <= h && h < 180 then (0., c, x)
    else if 180 <= h && h < 240 then (0., x, c)
    else if 240 <= h && h < 300 then (x, 0., c)
    else (c, 0., x)
  in
  let r, g, b = ((r' +. m) *. 255., (g' +. m) *. 255., (b' +. m) *. 255.) in
  to_rgb (r |> int_of_float) (g |> int_of_float) (b |> int_of_float)

let hex_of_rgb = Printf.sprintf "%#.6x"
