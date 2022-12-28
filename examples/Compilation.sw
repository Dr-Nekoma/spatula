[defun main [(a Unit)]
    [print String "Hello World! :)"]]

{;
generateIL [DeclExpr (EApplication (EVariable (pack "print")) (ELiteral (LString (pack "Hello World"))))] []
writeFile "test.il" $ unpack . Data.Text.concat $ generateIL [DeclFun (pack "main") (TArrow TUnit TUnit) (EApplication (EVariable $ pack "print") (ELiteral (LString $ pack "abc")))] [pack ".assembly extern mscorlib {}\n", pack ".assembly test {}"]
writeFile "test.il" $ unpack . Data.Text.concat $ generateIL [DeclExpr (EApplication (EVariable (pack "print")) (ELiteral (LString (pack "Hello World")))), DeclFun (pack "main") (TArrow TUnit TUnit) (EApplication (EVariable $ pack "print") (ELiteral (LString $ pack "abc")))] [pack ".assembly extern mscorlib {}\n", pack ".assembly test {}"]
;}