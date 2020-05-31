{-# LANGUAGE OverloadedStrings #-}

import EasyTest
import SimpleLang

allTests = tests
    [ scope "state" $ tests
        [ expect $ empty "xdf" == 0
        , expect $ (let state1 = extend empty "a" 1 in (state1 "a", state1 "b")) == (1, 0)
        ]
    , scope "eval" $ tests
        [ expect $ eval empty (Val 5) == 5
        , expect $ eval (extend empty "a" 1) (Op (Val 1) Eql (Var "a")) == 1
		, expect $ eval (extend empty "a" 10) (Op (Val 3) Plus (Var "a")) == 13
		, expect $ eval (extend empty "a" 10) (Op (Val 3) Minus (Var "a")) == -7
		, expect $ eval (extend empty "a" 10) (Op (Val 3) Times (Var "a")) == 30
		, expect $ eval (extend empty "a" 10) (Op (Val 3) Divide (Var "a")) == 0
		, expect $ eval (extend empty "a" 10) (Op (Val 35) Divide (Var "a")) == 3
		, expect $ eval (extend empty "a" 10) (Op (Val 3) Gt (Var "a")) == 0
		, expect $ eval (extend empty "a" 10) (Op (Val 3) Ge (Var "a")) == 0
		, expect $ eval (extend empty "a" 10) (Op (Val 10) Lt (Var "a")) == 0
		, expect $ eval (extend empty "a" 10) (Op (Val 10) Le (Var "a")) == 1
        ]
    , scope "desugar" $ tests 
        [ expect $ desugar (Incr "A") == DAssign "A" (Op (Var "A") Plus (Val 1))
        ]
    , scope "run" $ tests
        [ expect $ runSimpler empty (DAssign "A" (Val 10)) "A" == 10
        , expect $ SimpleLang.run empty (Incr "A") "A" == 1
        , expect $ (let s = SimpleLang.run (extend empty "In" 4) factorial in s "Out") == 24
		, expect $ SimpleLang.run empty (Block [Incr "A" , Incr "A", Incr "A"]) "A" == 3
		, expect $ SimpleLang.run (extend empty "A" 3) (While (Op (Var "A") Le (Val 21)) (Assign "A" (Op (Var "A") Plus (Val 3)))) "A" == 24
        ]
    ]

main = EasyTest.run allTests