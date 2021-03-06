open Notty
open Notty_unix

let menu_msg () =
  let line1 =
    I.string
      A.(fg white ++ st bold)
      "Please enter an operation, followed by a space, followed by"
  in
  let line2 =
    I.string
      A.(fg white ++ st bold)
      "the numbers you want to operate on."
  in

  let line3 =
    I.string A.(fg white ++ st bold) "Functions available: "
  in
  let line4 =
    I.string
      A.(fg white ++ st italic)
      "Add (takes in multiple inputs, returns float)"
  in
  let line5 =
    I.string
      A.(fg white ++ st italic)
      "Subtract (takes in multiple inputs, returns float) "
  in
  let line6 =
    I.string
      A.(fg white ++ st italic)
      "Divide (takes in multiple inputs, returns float)"
  in
  let line7 =
    I.string
      A.(fg white ++ st italic)
      "Multiply (takes in multiple inputs, returns float)"
  in
  let line8 =
    I.string
      A.(fg white ++ st italic)
      "Factorial (takes in one input, returns integer)"
  in
  let line9 =
    I.string
      A.(fg white ++ st italic)
      "FastExp (takes in three inputs, returns integer)"
  in
  let line10 =
    I.string
      A.(fg white ++ st italic)
      "Sin (takes in one input, returns float)"
  in
  let line11 =
    I.string
      A.(fg white ++ st italic)
      "Cos (takes in one input, returns float)"
  in
  let line12 =
    I.string
      A.(fg white ++ st italic)
      "Tan (takes in one input, returns float)"
  in
  let line13 =
    I.string
      A.(fg white ++ st italic)
      "Pythag (takes in two sides, returns third side as float)"
  in
  let line25 =
    I.string
      A.(fg white ++ st italic)
      "Mean (takes in multiple input, returns float)"
  in
  let line14 =
    I.string
      A.(fg white ++ st italic)
      "Median (takes in multiple input, returns float)"
  in
  let line15 =
    I.string
      A.(fg white ++ st italic)
      "StdDev (takes in multiple input, returns float)"
  in
  let line16 =
    I.string
      A.(fg white ++ st italic)
      "LinReg (takes in two lists, returns linear regression)"
  in
  let line17 =
    I.string
      A.(fg white ++ st italic)
      "Poly (takes in a function and a value to evaluate the function)"
  in
  let line18 =
    I.string
      A.(fg white ++ st italic)
      "Sigma (evaluates the sigma from the first number (floor) to the "
  in
  let line19 =
    I.string
      A.(fg white ++ st italic)
      "second number (ceiling) using the user-inputted polynomial)"
  in
  let line20 =
    I.string
      A.(fg white ++ st italic)
      "Derivative (takes in a function and a value to evaluate the "
  in
  let line21 =
    I.string A.(fg white ++ st italic) "derivative at the value)"
  in
  let line20a =
    I.string
      A.(fg white ++ st italic)
      "HDerivative (takes in a function, the degree of \
       differentiation, and a value to evaluate the "
  in
  let line21a =
    I.string A.(fg white ++ st italic) "derivative at the value)"
  in
  let line22 =
    I.string
      A.(fg white ++ st bold)
      "Enter Exit at any time to exit from the program"
  in
  let line23 =
    I.string
      A.(fg white ++ st bold)
      "If you want more information, use command 'help' followed"
  in
  let line24 =
    I.string
      A.(fg white ++ st bold)
      "by the function you would like to learn more about."
  in
  I.(pad ~l:1 ~t:2 ~b:2 line1) |> Notty_unix.output_image;
  I.(pad ~l:1 ~b:1 line2) |> Notty_unix.output_image;
  let long_line = Array.make 80 0x2500 in
  I.uchars A.(fg (rgb 1 2 4)) (Array.map Uchar.of_int long_line)
  |> Notty_unix.eol |> Notty_unix.output_image;
  I.(pad ~l:1 ~b:2 line3) |> Notty_unix.output_image;
  I.(pad ~l:1 ~b:1 line4) |> Notty_unix.output_image;
  I.(pad ~l:1 ~b:1 line5) |> Notty_unix.output_image;
  I.(pad ~l:1 ~b:1 line6) |> Notty_unix.output_image;
  I.(pad ~l:1 ~b:1 line7) |> Notty_unix.output_image;
  I.(pad ~l:1 ~b:1 line8) |> Notty_unix.output_image;
  I.(pad ~l:1 ~b:1 line9) |> Notty_unix.output_image;
  I.(pad ~l:1 ~b:1 line10) |> Notty_unix.output_image;
  I.(pad ~l:1 ~b:1 line11) |> Notty_unix.output_image;
  I.(pad ~l:1 ~b:1 line12) |> Notty_unix.output_image;
  I.(pad ~l:1 ~b:1 line13) |> Notty_unix.output_image;
  I.(pad ~l:1 ~b:1 line25) |> Notty_unix.output_image;
  I.(pad ~l:1 ~b:1 line14) |> Notty_unix.output_image;
  I.(pad ~l:1 ~b:1 line15) |> Notty_unix.output_image;
  I.(pad ~l:1 ~b:1 line16) |> Notty_unix.output_image;
  I.(pad ~l:1 ~b:1 line17) |> Notty_unix.output_image;
  I.(pad ~l:1 ~b:1 line18) |> Notty_unix.output_image;
  I.(pad ~l:1 ~b:1 line19) |> Notty_unix.output_image;
  I.(pad ~l:1 ~b:1 line20) |> Notty_unix.output_image;
  I.(pad ~l:1 ~b:1 line21) |> Notty_unix.output_image;
  I.(pad ~l:1 ~b:1 line20a) |> Notty_unix.output_image;
  I.(pad ~l:1 ~b:1 line21a) |> Notty_unix.output_image;
  I.uchars A.(fg (rgb 1 2 4)) (Array.map Uchar.of_int long_line)
  |> Notty_unix.eol |> Notty_unix.output_image;
  I.(pad ~l:1 ~b:2 line22) |> Notty_unix.output_image;
  I.(pad ~l:1 ~b:1 line23) |> Notty_unix.output_image;
  I.(pad ~l:1 ~b:2 line24) |> Notty_unix.output_image
