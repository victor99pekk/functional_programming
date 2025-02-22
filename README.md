# 3 - Projects in Haskell

[__(1) interpreter for a small imperative language__]((#-(1)-`Interpreter`))

- implementing a parser and interpreter for an imperative language

__(2) String alignment optimization-algorithm__

- Implemented a string alignment optimizer in Haskell using memoization to improve efficiency

__(3) Chatbot__

- recursive pattern matching chatbot

<br>
<br>

# (1) `Interpreter`

The parsed statements are executed using an interpreter that evaluates expressions, assigns variables, and handles control flow structures.

The interpreter uses many monadic parsers, parsers in the lowest level try to parse specific strings, these parsers are then combined to parse certain statements. If a parser fails to parse, another parser can try. It is important that the parsers return monads since they will not always be able to parse.

The language has just one data type, integer, and variables are not declared. In the while and if statements a positive expression value is interpreted as true while 0 and negative values mean false.

__Language features__

(1) `Variable assignments` (x := 10)
(2) `Control Flow` (if-then-else, while loops)
(3) `I/O-operations` (read, write)
(4) `Èxpressions` with arithmetic (+,-,/,^,...)


__Example code:__

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

__Gramar:__


This defines how you are allowed to build statements in the program, the collection of statements is the program.

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


# (2) `String Alignemnt optimization algorithm`

This project involves implementing a string alignment optimizer in Haskell using memoization to improve efficiency. The goal is to compute all optimal alignments between two strings based on a given scoring system that rewards matches and penalizes mismatches and spaces.

__Key Concepts__

- **String Alignment**: The process of aligning two strings by inserting spaces to maximize similarity based on a scoring system.
- **Memoization**: A technique used to store and reuse previously computed values to avoid redundant calculations, significantly improving efficiency.
- **Optimization**: The goal is to find the alignment(s) with the highest possible score, avoiding brute-force approaches that would be computationally expensive.


__Scoring system:__ `scoreMatch` = 1, `scoreMismatch` = -1, `scoreSpace` = -2
```plaintext
H - A S K E L L
- P A S C - A L
```


__Application in DNA Alignment__

This technique is highly useful in **DNA sequence alignment**, where biological sequences (e.g., DNA, RNA, or proteins) are compared to find similarities, evolutionary relationships, or mutations. By aligning genetic sequences optimally, researchers can:

- Identify **unknown viruses** by comparing RNA sequences.
- Detect **genetic variations** between species.
- Construct **evolutionary trees** to understand the history of species divergence.

Using Haskell's functional programming paradigm, the project efficiently handles the **combinatorial explosion** of possible alignments through dynamic programming and memoization, making it suitable for large-scale biological data analysis.


# (3) `Chatbot`
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




