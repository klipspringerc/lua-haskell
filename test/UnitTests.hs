module UnitTests where
import Data.HashMap.Strict as H

import Test.Tasty.HUnit
import PropertyTests (parseExecTest)

intOpExpUnitTests :: [(String, Assertion)]
intOpExpUnitTests =
  [ ( "^"
    , assertEqual ""
      "8" (parseExecTest "print (2^3)")
    )
  , ( "unop-"
    , assertEqual ""
      "-10" (parseExecTest "print (-10)")
    )
  , ( "*"
    , assertEqual ""
      "12" (parseExecTest "print (3*4)")
    )
  , ( "/"
    , assertEqual ""
      "2" (parseExecTest "print (8/4)")
    )
  , ( "%"
    , assertEqual ""
      "1" (parseExecTest "print (7%2)")
    )
  , ( "+"
    , assertEqual ""
      "12" (parseExecTest "print (3+9)")
    )
  , ( "-"
    , assertEqual ""
      "1" (parseExecTest "print (10-9)")
    )
  , ( "int op mix"
    , assertEqual ""
      "1" (parseExecTest "print ((-1*2^3+5-10/2)%3)")
    )  ]
 
boolOpExpUnitTests :: [(String, Assertion)]
boolOpExpUnitTests =
  [ ( "<"
    , assertEqual ""
      "true" (parseExecTest "print (2<3)")
    )
  , ( ">"
    , assertEqual ""
      "false" (parseExecTest "print (10>100)")
    )
  , ( "<="
    , assertEqual ""
      "false" (parseExecTest "print (3<=2)")
    )
  , ( ">="
    , assertEqual ""
      "false" (parseExecTest "print (1>=10)")
    )
  , ( "int bool op mix"
    , assertEqual ""
      "true" (parseExecTest "print (4*(-5)/2+10 < 10)")
    )
  ]

strOpExpUnitTests :: [(String, Assertion)]
strOpExpUnitTests =
  [ ( ".."
    , assertEqual ""
      "abcdef" (parseExecTest "print (\"abc\"..\"def\")")
    )
  , ( "unop#"
    , assertEqual ""
      "7" (parseExecTest "print (#\"abcdefg\")")
    )
  , ( "str op mix"
    , assertEqual ""
      "6" (parseExecTest "print (#(\"abc\"..\"def\"))")
    )
  , ( "int bool str op mix "
    , assertEqual ""
      "true" (parseExecTest "print (3*4 > #(\"abc\"..\"def\"))")
    )
  ]

logicOpExpUnitTests :: [(String, Assertion)]
logicOpExpUnitTests =
  [ ( "and"
    , assertEqual ""
      "false"
      (parseExecTest "print (false and true)")
    )
  , ( "lua and"
    , assertEqual ""
      "abc"
      (parseExecTest "print(99 and \"abc\")")
    )
  , ( "or"
    , assertEqual ""
      "99" (parseExecTest "print (99 or nil)")
    )
  , ( "lua or"
    , assertEqual ""
      "any thing" (parseExecTest "print (false or \"any thing\")")
    )
  , ( "~="
    , assertEqual ""
      "true" (parseExecTest "print (true ~= 34)")
    )
  , ( "=="
    , assertEqual ""
      "false" (parseExecTest "print (3 == true)")
    )
  , ( "logic mix"
    , assertEqual ""
      "99" (parseExecTest "print (99 or false == false and nil)")
    )
  ]


assignVarUnitTests :: [(String, Assertion)]
assignVarUnitTests =
  [ ( "single assignment"
    , assertEqual ""
      "false"
      (parseExecTest "do b = 33 and false; print(b) end")
    )
  , ( "multiple assignment"
    , assertEqual ""
      "abc"
      (parseExecTest "do a,b,c = 3*5+1, \"abc\" ; print(b) end")
    )
  ]

tableUnitTests :: [(String, Assertion)]
tableUnitTests =
  [ ( "table constructor"
    , assertEqual ""
      "{fromList [(3,true),(1,99),(x,abc)]}"
      (parseExecTest "do t = {[1] = 99, [\"x\"]=\"abc\", [#\"key\"]=true}; print (t) end")
    ) 

  , ( "table assignment and lookup"
    , assertEqual ""
      "{fromList [(1,99),(x,true)]}"
      (parseExecTest "do t = {}; t[1] = 99; t[\"x\"] = true; print (t) end")
    )
  ]

loopUnitTests :: [(String, Assertion)]
loopUnitTests =
  [ ( "for step"
    , assertEqual ""
      "45"
      (parseExecTest "do v=0; for i=1,10,1 do v = v+i end; print(v) end")
    )
    , ( "for negative step"
      , assertEqual ""
      "54"
      (parseExecTest "do v=0; for i=10,1,-1 do v = v+i end; print(v) end")
    )
    , ( "while"
      , assertEqual ""
      "16"
      (parseExecTest "do v=1; while v < 10 do v=v*2 end; print(v) end")
    )
  ]

ifUnitTests :: [(String, Assertion)]
ifUnitTests =
  [ ( "if else"
    , assertEqual ""
      "40"
      (parseExecTest "do a=20;b=40;op=\"+\"; if a > b then v=a else v=b end; print(v) end")
    )
    , ( "if elseif sequence"
      , assertEqual ""
      "-20"
      (parseExecTest "do a=20;b=40;op=\"-\"; if op == \"+\" then return a + b elseif op == \"-\" then return a - b else return -1 end end")
    )
  ]

funcUnitTests :: [(String, Assertion)]
funcUnitTests =
  [ ( "function call"
    , assertEqual ""
      "628"
      (parseExecTest "do function tcircum(r) do pi=314;return 2*pi*r/100 end end; tcircum(100) end")
    )
  ]
