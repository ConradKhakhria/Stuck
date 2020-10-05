# Compiler for the Stuck language

## Introduction
Stuck is an entirely imperative language where the only data storage locations are 
a global stack and a function's own arguments, and the only data type is a signed
integer. Consequently, it is very difficult to use.

## Syntax
Stuck is organised into functions, the names of which must be listed in alphabetical order,
with arguments also listed in alphabetical order. A Stuck function can only call itself or
functions declared before it, resulting in an infalliable yet really stupid way to deal with
value scope conflicts.

### Operations
Each line of a Stuck function can do one of six things:
1. Push a function argument onto the stack. The following pushes 'x' onto the stack:

        > x

2. Pop the stack into a function argument. This pops the stack into 'x':

        < x

3. Test whether a function argument is greater than zero. If it is, the indented block
that follows it will be executed, and if not it will be ignored. The following checks if
'x' is greater than zero and pushes it onto the stack if it is

        x
          > x

Note: there is no equivalent 'else' statement in the language. In cases where this is
necessary, a solution is to give the function an argument which is always greater than
zero and then check it. For example, the following does the same as the above code but
pops the stack into 'y' (which = 1) in case x <= 0:

        x
          > x
        y
          < y

4. Call a function. Arguments being passed to the function being called can be any
arithmetical expression (using symbols +, -, * and / as well as grouping expressions with
parentheses) with or without the arguments of the calling function, as long as there is no
whitespace within the expression for an individual argument. For example, to call a function
'c' which has 3 arguments from a function which has arguments 'f' and 'g' one could write:

        c f+g*2 (g+1)*4 10

5. Get user input. The user input must be an integer, and will be pushed directly onto the
stack. The following reads a number from stdin and pops it into argument 'm':

        ?
        < m

6. Print the top of the stack. This operation does not affect the stack itself, so the value
on the top of the stack can be used after it has been printed. The following prints the top
of the stack and then pops it into argument 'm':

        !
        < m

(7. Comments start with '#' and continue until the end of the line. They can occur anywhere
within the program.)

### Function declaration
Functions are defined with indented blocks. For example, the following defines a function
'a' which pushes its sole parameter to the stack and a function 'b' which pushes user input
to the stack using 'a':

        a b
          > b

        b c
          ?
          < c
          a c

The argument 'c' is needed because there's no way to directly pass the top of the stack as a
parameter to 'a'

Notice that Stuck has no concept of returning values from functions. Instead, any values that
need to be preserved for the calling function should be pushed onto the stack. For example, this
short program creates a function b which doubles its numeric input and pushes it to the stack
using a function 'a':

        a b
          > b

        b c
          a 2*c

To the new user, Stuck may feel overly minimalistic and incapable of doing the most basic things
without dozens of helper functions. While this is true, a number of interesting programs can be
implemented in Stuck, and there are some well-commented examples in the 'examples' directory to
make the syntax and functionality of Stuck easier to understand.

## Current status
So far, the parsing process has been fully implemented and tested, but the translation of parse
trees to actual assembly isn't complete yet. Currently, the plan is just to support 32-bit x86
assembly, but this might change in the future.

