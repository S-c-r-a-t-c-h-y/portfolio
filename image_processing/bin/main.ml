open Image_processing

let () =
  let img = PPM_images.open_ppm "src/adrien.pnm" in
  let rgb_img = PPM_images.ppm_to_rgb img in
  let inv_img = Filter.inverse rgb_img in
  let red_img = Filter.red rgb_img in
  let green_img = Filter.green rgb_img in
  let blue_img = Filter.blue rgb_img in
  let blue1_img = Filter.blue1 rgb_img in
  let green0_img = Filter.green0 rgb_img in
  (* let cropped_img = Transform.crop 200 200 512 512 rgb_img in *)
  let rotated_img = Transform.rotate_angle 90. rgb_img in
  let flipped_img = Transform.flip_horizontal rgb_img in
  let blured_img = Transform.gaussian_blur 11 rgb_img in
  let blured_img2 = Transform.box_blur rgb_img in
  let sharpened_img = Transform.sharpen rgb_img in
  let edged_img = Transform.edge rgb_img in
  let vignette_img = Filter.vignette 0.5 rgb_img in
  let bright_img = Filter.brighten 100 rgb_img in
  let embossed_img = Transform.emboss rgb_img in
  let hue_img = Filter.hue 100 rgb_img in
  let sat_img = Filter.saturation 1. rgb_img in
  Printf.ksprintf Graphics.open_graph " %dx%d" (rgb_img.width * 2)
    (rgb_img.height / 2);
  (* Printf.ksprintf Graphics.open_graph " %dx%d" (rgb_img.width * 2)
     rgb_img.height; *)
  Display.display_rgb_image rgb_img ~scale:0.5 ~scaling_algorithm:Resize.Bicubic
    0 0;
  Display.display_rgb_image vignette_img ~scale:0.5
    ~scaling_algorithm:Resize.Bicubic rgb_img.width 0;
  Display.hande_graph_closing ()
