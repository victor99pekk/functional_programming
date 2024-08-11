# functional_programming

### Projects in Haskell:
- Chatbot
- string-Alignment-Optimization-algorithm
- interpreter for a small imperative language


## Chatbot
Chatbot inspired by the early chatbot "Eliza" from the sixties. The chatbot uses pattern recursive pattern matching to reflect what you say, into responding questions. The chatbot is defined by a list of pattern based rules like this:

```haskell
eliza = [
  ("I need *",
      ["Why do you need * ?",
       "Would it really help you to get * ?",
       "Are you sure you need * ?"]),

  ("Why don't you *",
      ["Do you really think I don't * ?",
       "Perhaps eventually I will * .",
       "Do you really want me to * ?"]),
       
       {-  ... and so on ... -} ]
```
- the '*' here is the rest of the sentence that isn't part of the match. This sentence is then reflected back instead of the '*'. It is also modified by for example switching instances of "I am" to "You are", since it would otherwise generate weird responses from the chatbot.

<br>

## Interpreter

The interpreter uses many monadic parsers, parsers in the lowest level try to parse specific strings, these parsers are then combined to parse certain statements. If a parser fails to parse, another parser can try. It is important that the parsers return monads since they will not always be able to parse.

The language has just one data type, integer, and variables are not declared. In the while and if statements a positive expression value is interpreted as true while 0 and negative values mean false.

```haskell
-- Example of code
read k;
   read n;
   m := 1;
   while n-m do
     begin
       if m - m/k*k then
         skip;
       else
         -- note a square below
         write m^2;
       m := m + 1; -- an inline comment 
     end
```
<br>

the grammar of the language is given by:
```haskell
   program ::= statements
   statement ::= variable ':=' expr ';'
           | 'skip' ';'
           | 'begin' statements 'end'
           | 'if' expr 'then' statement 'else' statement
           | 'while' expr 'do' statement
           | 'read' variable ';'
           | 'write' expr ';'
   statements ::= {statement}
   variable ::= letter {letter}
```
This defines how you are allowed to build statements in the program, the collection of statements is the program.




