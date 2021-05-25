exception Malformed

let help str =
  let lower_str = String.lowercase_ascii str in
  match lower_str with
  | "add" ->
      "The add function takes in multiple inputs as either integers or \
       floats and will return the sum of all the numbers inputed as a \
       float. Order of inputs do not matter. The implementation is \
       tail-recursive. For example, [Add 1 2 3] will return 6. "
  | "divide" ->
      "The divide function takes in multiple inputs as either integers \
       or floats and will divide each input from its previous inputs. \
       The result is a float. Thus, order of inputs do matter. The \
       function is left associative and implementation is \
       tail-recursive. An input other than the first being 0 will \
       raise an exception. For example, [Divide 3. 1 2] = 1.5"
  | "multiply" ->
      "The multiply function takes in multiple inputs as either \
       integers or floats and will return the product of all the \
       numbers inputed as a float. Order of inputs do not matter. The \
       implementation is tail-recursive. For example, [Multiply 7 2. \
       3] will return 51. "
  | "subtract" ->
      "The subtract function takes in multiple inputs as either \
       integers or floats and will subtract each input from its \
       previous inputs. The result is a float. Thus, order of inputs \
       do matter. The mplementation is tail-recursive. For example, \
       [Subtract 3. 1 2] = 0."
  | "factorial" ->
      "The factorial function takes in exactly one input that is an \
       nonnegative integer. It will return the factorial of that \
       number as an integer. The implementation is tail recursive but \
       the largest number that it will take is 19. Integers larger \
       that or negative integers will raise exceptions. For example, \
       [Factorial 7] = 7! = 5040."
  | "fastexp" ->
      "The fast exponentiation function takes in three inputs. The \
       first two inputs are integers (denoted m and n). The third \
       input is in base 2 (binary represenation of e). The function \
       will return m ^ e (mod n). This is useful for public \
       cryptography. Order of inputs do matter. Exceptions are also \
       raised when inputs are not positive. This implementation is \
       tail-recursive. For example, [FastExp 3 7 101010] = 1"
  | "gcd" ->
      "The greatest common denominator function takes in two integer \
       inputs. The order does not matter. It will return the greatest \
       common denominator of the two inputs as an integer. Both \
       numbers must be positive. This implementation is \
       tail-recursive. For example [GCD 14 21] = 7"
  | "mean" ->
      "The mean function takes in multiple inputs as integers or \
       floats. The order does not matter. It will return the average \
       of all inputs as a float. For example, [Mean 2 2 2 6] = 3."
  | "sin" ->
      "The sine function takes in one input as an integer or float. It \
       will return a float that is the sine function evaluated at that \
       input, treated as an degree. Due to pi being approximated as a \
       float, some inputs may yield values that are very close \
       approximations.  For example, [Sin 0] = 0."
  | "cos" ->
      "The cosine function takes in one input as an integer or float. \
       It will return a float that is the cosine function evaluated at \
       that input, treated as an degree. Due to pi being approximated \
       as a float, some inputs may yield values that are very close \
       approximations.  For example, [Cos 0] = 1."
  | "tan" ->
      "The tangent function takes in one input as an integer or float. \
       It will return a float that is the tangent function evaluated \
       at that input, treated as an degree. Due to pi being \
       approximated as a float, some inputs may yield values that are \
       very close approximations. For example, [Tan 0] = 0."
  | "pythag" ->
      "The pythagorean function will find the third side of a right \
       angle triangle. It will prompt for the side you are looking \
       for, either a hypotenuse of leg. It will then proceed to ask \
       for the remaining two sides. The order of input does matter as \
       it will influence how the calculator treats the input. An \
       exception will be raised if two sides don't form a plausible \
       triangle in the order the inputs were received. For example, if \
       you were looking for the hypotenuse and input 3 and 4, the \
       function will return 5."
  | "median" ->
      "The median function takes in multiple inputs as integers or \
       floats. The order does not matter. It will sort the inputs and \
       return the middle input if the there is an odd number of \
       inputs. In the case of an even number of inputs, the average of \
       the middle two number is calculated and returned. For example, \
       [Median 2 3 4 6] = 3.5"
  | "stddev" ->
      "The standard deviation function takes in multiple inputs as \
       integers or floats. The order does not matter. It will sort the \
       inputs and return the standard deviation of all the inputs, \
       treating the list as a sample. For example, [StdDev 1 2 3 4 ] = \
       0.745"
  | "linreg" ->
      "The linear regression function will prompt for two lists of \
       integers or floats as inputs. Order of lists and order within \
       lists matter. The first list will be treated as the independent \
       variable and the second list will be treated as the dependent \
       variable. Inputs in the first and second list are paired by the \
       order they were inputted in. The two lists must be of the same \
       length or an exception will be raised. The function returns a \
       linear regression of the two lists. For example, with First \
       List as 1 2 3 4 and Second List 2 4 6 8, then a = 2. and b=0. \
       in the form y=ax+b. "
  | "poly" ->
      "The polynomial function will prompt for a polynomial function \
       as an input and then a value (integer or float) to evaluate \
       that function at. It will return a float. For example, with \
       function x^2+1 and value 1, the answer would return 2. "
  | "multivar" ->
      "The multivariable function will prompt for a function as an \
       input which can have mutliple variables. The user then inputs \
       the values (integer or float) to evaluate the function at. It \
       will return a float. "
  | "sigma" ->
      "The sigma function will prompt for a floor value and ceiling \
       value as integers and then a function. It will evaluate the \
       function at all the values from the floor to the ceiling and \
       sum them together. For example, if floor value is 0, ceiling \
       value is 2 and function is x^2+2, the answer will return as \
       11. "
  | "derivative" ->
      "The derivative function will prompt for a polynomial function f \
       as an input and a value v. It will then evaluate f'(v) and \
       return the value. For example, for f(v) = x^2 and v = 3, it \
       will return f'(3) = 6. "
  | "hderivative" ->
      "The higher order derivative function will prompt for a \
       polynomial function f as an input, a degree of differentiation \
       h, and a value v. It will then f'(v) h times and return the \
       value. For example, for f(v) = x^3, degree h = 2, and value v = \
       3, it will return f''(3) = 18. "
  | _ -> raise Malformed
