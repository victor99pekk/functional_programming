# Projects in Haskell

This repository showcases a collection of Haskell projects that demonstrate advanced problem-solving techniques, including functional programming, optimization algorithms, and natural language processing.

---

[__1. Interpreter for a Small Imperative Language__](#1-interpreter-for-an-imperative-language)

- Developed a custom parser and interpreter for a small imperative language.
- Utilized Haskell’s powerful type system and monadic parsers to handle language parsing, execution, and error handling.

[__2. String Alignment Optimization Algorithm__](#2-string-alignment-optimization-algorithm)

- Implemented a string alignment optimizer using memoization to improve efficiency.
- Applied dynamic programming techniques to solve optimization problems commonly encountered in computational biology.

[__3. Chatbot__](#3-chatbot)

- Built a recursive pattern-matching chatbot inspired by the classic "Eliza" program.
- Used Haskell's functional programming paradigm to implement flexible rule-based conversational patterns.

---

## 1. Interpreter for an Imperative Language

In this project, I implemented a parser and interpreter for a custom, simplified imperative language using **Haskell**. The interpreter evaluates arithmetic expressions, handles variable assignments, and supports basic control flow constructs like `if-then-else` and `while` loops.

### Key Features:
- **Monadic Parsing**: Built using Haskell's monadic parsers to combine multiple parsing strategies for complex language structures. This approach allowed for better error recovery and modular code.
- **Control Flow**: Supports `if-then-else` and `while` loops, with a custom interpretation of boolean expressions based on integer values (non-zero = true, zero = false).
- **Variable Assignment**: Implements simple variable assignment without explicit declaration, mimicking dynamic typing.
- **I/O Operations**: Provides basic input/output operations (`read`, `write`) to simulate interaction with the user.

### Example Code:
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
         write m^2;
       m := m + 1; -- Inline comment
     end
```

### Language Grammar
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

## 2. String Alignment Optimization Algorithm
In this project, I implemented a string alignment optimization algorithm using Haskell’s functional programming techniques. The goal was to align two strings efficiently, taking into account different scoring systems for matches, mismatches, and gaps.

### Key Concepts:
- `Memoization`: Used to optimize the recursive solution by caching intermediate results, significantly improving the algorithm's performance.
- `Dynamic Programming`: Implemented to handle the combinatorial explosion of possible string alignments, ensuring efficient calculation even with large input sizes.
- `String Matching`: Utilized an alignment scoring system that rewards matches, penalizes mismatches, and introduces a gap penalty for space insertions.

### Scoring System:

*   `scoreMatch = +1`
*   `scoreMismatch = -1`
*   `scoreSpace = -2`

```plaintext
H - A S K E L L
- P A S C - A L
```


## 3. Chatbot

This project is a **recursive pattern-matching chatbot** inspired by the famous "Eliza" chatbot from the 1960s. The chatbot simulates a conversation by using predefined pattern-based rules, where user input is matched against a set of regular expressions.

### Key Features:

- **Pattern Matching**: Utilized recursive pattern matching to process user input and generate appropriate responses.
- **Rule-Based Responses**: Defined conversational rules with variable-length patterns, where the chatbot reflects part of the user's message.
- **Natural Language Processing**: Simulated simple conversational AI with a list of predefined patterns that adjust responses based on user input.

### Example rule:
```haskell
eliza = [
  ("I need *", ["Why do you need *?", "Would it really help you to get *?", "Are you sure you need *?"]),
  ("Why don't you *", ["Do you really think I don't *?", "Perhaps eventually I will *.", "Do you really want me to *?"]),
  {-  ... more patterns ... -}
]
```

### chatbot Highlights:

- Demonstrated understanding of **natural language processing** principles using functional programming.
- Showcased the ability to build a conversational system with limited resources and predefined rules.
- Explored recursive functions in Haskell for handling pattern matching and response generation.

This chatbot demonstrates my capability to apply functional programming to build interactive, AI-driven systems.



