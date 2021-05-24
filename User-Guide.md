## User guide
This file contains information on the syntax used in our scripting language
### **1 | Simple variable assignements**
- `> x = 5` to create a variable x and assign it to 5.
- `> y = x` to create a variable y and assign it to x.
- `> z = 2*x` to create a variable z and assign it to 2*x.

### **2 | Printing results**
The value of any variable or the result of evalutating any expression can be printed to the terminal
by terminal by using print(). 
- `> print(3 + 5)` produces `8`
- `> print(z)` produces `10` (following from the previous code snippets)

Badly formed expressions passed into print will produce errors such as the one below.

- `> print(x + "a")` produces `Invalid operands used for addition`

### **3 | Manipulating Strings and Ints**
- The `++` operator can be used to concatenate two strings
  - `> print("hello" ++ " world")` produces `hello world`

- `toInt()` can be used to convert a String to an Int.
  - `> print(toInt("5.0"))` produces `5.0`

- `toString()` can be used to convert an Int to a String.
  - `> print("2.5 * 2.5 = " ++ toString(2.5 * 2.5))` produces `"2.5 * 2.5 = 6.25"`

### **4 | Processing input files containing a list of commands**
*Note: the file being read in must lie in the Code folder and you must include the extension*
- `> read <filename>` can be used to proccess any input file
  - For example, `> read test.txt` can be used to process the provided test file.

### **5 | Looping over a block of commands**
The `repeat` command can be used to apply a block of commands any number of times.

- `> repeat 10 { x = x + 1; print x; }` increments x by 1 and prints x 10 times.

- Repeat can also be applied when reading from     files: 

  `> repeat 5 { read test.txt; }` processes the file 5 times.

### **6 | If then else statements**
A conditional operation can be performed by using the following syntax.
- `if (<boolean condition>) then { <do something> } else { <do something else> }`
- E.g. `if(False || (10 == 10 && True)) then{x = 2.5; print x;} else {print 5;}` outputs `2.5`


### **7 | While and For Loops**
To allow more expressive looping, while and for loops can be used by using the following syntax

 `> x = True` 

 `> y = 5`

 `> while(x && y > 3) {print("x == " ++ toString(x)); print("y == " ++ toString(y)); y--;}`

This creates the output
`x == True`

`y == 5.0`

`x == True`

`y == 4.0`

For loops can be used as 

`> for(i = 0; i < 2; i++;){print(i);} `

`0.0`

`1.0`