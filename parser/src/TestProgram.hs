{- Test for Program -}
{-# LANGUAGE InstanceSigs #-}
module TestProgram where


import Program

p0, p1, p2, p3, p4 :: Program.T

p0 = fromString  ("\
\read k;\
\read n;\
\m := 1;\
\while n-m do\
\  begin\
\    if m - m/k*k then\
\      skip;\
\    else\
\      write m;\
\    m := m + 1;\
\  end")

p1 = fromString  ("\
\read n;\
\read b;\
\m := 1;\
\s := 0;\
\p := 1;\
\while n do\
\  begin\
\    q := n/b;\
\    r := n - q*b;\
\    write r;\
\    s := p*r+s;\                    
\    p := p*10;\
\    n :=q;\
\  end\
\write s;")

s1 = "\
\read n;\
\read b;\
\m := 1;\
\s := 0;\
\p := 1;\
\while n do\
\  begin\
\    q := n/b;\
\    r := n - q*b;\
\    write r;\
\    s := p*r+s;\                    
\    p := p*10;\
\    n :=q;\
\  end\
\write s;"

sp = putStr (toString p0)

sp1 = putStr (toString p1)

p2 = fromString (toString p0)

p3 = fromString (toString p1)

rp0 = Program.exec p0 [3,16]

rp1 = Program.exec p1 [1024, 2]

s4 = "\
\read a;\
\read b;\
\-- a comment\n\
\s := 3;\
\while a do\
\  begin\
\    c := a^s;\
\    d := 2^a;\
\    write c;\
\    write d;\                    
\    a := a-1;\
\  end\
\write a;"

s5 = "\
\read a;\
\read b;\
\s := 3;\
\while a do\
\  begin\
\    c := a^s;\
\    d := 2^a;\
\    write c;\
\    write d;\                    
\    a := a-1;\
\  end\
\write a;"

s6 = "\
\read a;\
\read -- input a value\n\
\     -- into variable b:\n\
\     b;\
\write a;\
\write b;\
\s := 3;"

p6 = fromString s6
rp6 = Program.exec p6 [1, 2]

s7 = "\
\read a;\
\read -- input a value\n\
\     b;\
\s := 3;"

p7 = fromString s7
rp7 = Program.exec p7 []

p5 = fromString s5
rp5 = Program.exec p5 []

p4 = fromString s4

rp4 = Program.exec p4 [4,4]
