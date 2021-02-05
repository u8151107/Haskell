{-# LANGUAGE OverloadedStrings #-}

import EasyTest
import Poly

allTests = tests
    [ scope "applyPoly" $ tests
        [ expect $ applyPoly x 0 == 0
        , expect $ applyPoly x 5 == 5
        , expect $ applyPoly 10 1 == 10
        ]
    
    , scope "PolyEqual" $ tests 
        [expect $ (==) (P [1, 2]) (P [1, 2, 0]) == True
		,expect $ (==) (P [4, 7, 5]) (P [4, 7, 5, 0, 0, 0]) == True
		,expect $ (==) (P [0, 0, 0, 0]) (P []) == True
		,expect $ (==) (P [4, 7, 5]) (P [4, 0, 5]) == False
		]
		
	, scope "show" $ tests 
        [expect $ show (3 * x * x + 1) == "3 * x^2 + 1"
		,expect $ show (4 * x * x * x * x + 2 * x * x * x - 1 * x - 9) == "4 * x^4 + 2 * x^3 - x - 9"
		,expect $ show (0*x*x) == "0"
		,expect $  show (P [1, 2]) == "2 * x + 1"
		,expect $ show (P [5, 6, 2, 0, 1, 0, 0]) == "x^4 + 2 * x^2 + 6 * x + 5"
		]
		
	, scope "plus" $ tests 
        [expect $ plus (P [1,2]) (P [2,3]) == 5 * x + 3
		,expect $ plus (P []) (P [2,3, 0, 4 , 3 , 0, 1]) == x^6 + 3 * x^4 + 4 * x^3 + 3 * x + 2
		,expect $ plus (P []) (P []) == 0
		,expect $ plus (P [1, 2, 3]) (P [0, 0, 0, 4, 5, 6]) == 6 * x^5 + 5 * x^4 + 4 * x^3 + 3 * x^2 + 2 * x + 1
		]	
		
	, scope "times" $ tests 
        [expect $ times (P [1, 2]) (P [2, 3]) == 6 * x^2 + 7 * x + 2
		,expect $ times (P [5, 2]) (P [5, 2]) == 4 * x^2 + 20 * x + 25
		,expect $ times (P [1, 3]) (P [1, 3, 9]) == 27 * x^3 + 18 * x^2 + 6 * x + 1
		,expect $ times (P [1,3]) (P []) == 0
		]
		
	, scope "negate" $ tests 
        [expect $ negate (P [-2, 5, -6]) == (P [2, -5, 6])
		,expect $ negate (P [-68]) == 68
		,expect $ negate (P [-3, 0, 4]) == -4 * x^2 + 3
		]
		
	, scope "deriv" $ tests 
        [expect $ deriv (P [10, 3, 0, 5]) == 15 * x^2 + 3
		,expect $ deriv (P [9, 3, 7]) == 14 * x + 3
		,expect $ deriv (P [0, 0, 0, 0, 0, 8]) == 40 * x^4
		]	
	, scope "nderiv" $ tests 
        [expect $ nderiv 2 (P [2, 3]) == 0
		,expect $ nderiv 2 (P [2, 3, 7]) == 14
		,expect $ nderiv 3 (P [2, 4, 7, 6, 8, 3, 5, 2, 7]) == 2352 * x^5 + 420 * x^4 + 600 * x^3 + 180 * x^2 + 192 * x + 36
		]	
    ]
	

main = EasyTest.run allTests