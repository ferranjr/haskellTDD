module StringCalculator_Test where

import StringCalculator
import Test.HUnit
import Control.Exception ( catch )

main = runTestTT tests


test1 = TestCase( assertEqual "two numbers, should add properly" (3) (add "1,2") )
test2 = TestCase( assertEqual "empty string, should add 0" (0) (add "") )
test3 = TestCase( assertEqual "one number, should be the same number" (1) (add "1") )

test4 = TestCase( assertEqual "allow add to handle new lines and commas" (6) (add "1\n2,3") )
test5 = TestCase( assertEqual "allow add to handle any delimer if specified" (6) (add "//;\n1;2;3"))


test6 = TestCase( assertEqual "Detect the delimeter is in entry line" (True) (hasDelimeter "//;\n1;2;3"))
test7 = TestCase( assertEqual "Detect the delimeter is NOT in entry line" (False) (hasDelimeter "1;2"))

test8 = TestCase( assertEqual "Delimeter extracted properly" (';') (delimeter "//;\n1;2"))

test9 = TestCase( assertEqual "Body gets extracted properly" ("1;2") (body "//;\n1;2"))

--test10= TestCase( assertEqual "Negatives not allowed" (error "negatives not allowed") ( catch(add "-1,2,3") ) )

test11 = TestCase( assertEqual "Numbers bigger than 1000 ignored" (3) (add "1,2,1001") )

tests = TestList [
            TestLabel "Adding two numbers"              test1,
            TestLabel "Adding empty String"             test2,
            TestLabel "Adding only one number"          test3,
            TestLabel "Allow newlines when splitting"   test4,
            TestLabel "Allow new delimeters to split"   test5,
            TestLabel "Detect delimeter in entry"       test6,
            TestLabel "Detect no delimeter in entry"    test7,
            TestLabel "Delimeter extracted properly"    test8,
            TestLabel "Body extracted properly"         test9{-,
            TestLabel "Negative numbers not allowed"    test10-},
            TestLabel "Numbers bigger than 1000"        test11
           ]
