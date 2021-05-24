{-# LANGUAGE TemplateHaskell #-}

module QuickCheck where

import Test.QuickCheck
import Test.QuickCheck.All

import Eval
import Expr
import REPL
import Tree

-- Check if commutativity holds for the 'add' and 'mul' operations we defined.
prop_is_commutative1 x y = add (Right (DoubleVal (x))) (Right (DoubleVal (y))) == add (Right (DoubleVal (y))) (Right (DoubleVal (x)))

prop_is_commutative2 x y = mul (Right (DoubleVal (x))) (Right (DoubleVal (y))) == mul (Right (DoubleVal (y))) (Right (DoubleVal (x)))

-- (a mod n) mod n = a mod n must hold (where n /= 0).
prop_mod a (Positive n) = (n >= 1) ==> modulo (modulo (Right (DoubleVal (a))) (Right (DoubleVal (n)))) (Right (DoubleVal (n))) == modulo (Right (DoubleVal (a))) (Right (DoubleVal (n)))

-- |a| must be >= 0.
prop_abs1 x = absolute (Right (DoubleVal (x))) >= Right (DoubleVal 0)

-- |x * y| = |x| * |y| must hold.
prop_abs2 x y = absolute (mul (Right (DoubleVal (x))) (Right (DoubleVal (y)))) ==  mul (absolute (Right (DoubleVal (x)))) (absolute (Right (DoubleVal (y))))

-- Idempotence ||x|| = |x| must hold.
prop_abs3 x = absolute (absolute (Right (DoubleVal (x)))) == absolute (Right (DoubleVal (x)))

-- |x + y| ≤ |x| + |y| must hold.
prop_abs4 x y = absolute (add (Right (DoubleVal (x))) (Right (DoubleVal (y)))) <=  add (absolute (Right (DoubleVal (x)))) (absolute (Right (DoubleVal (y))))

-- -- Applying round twice must result in the same number
prop_round_is_idempotent n = round4dp (round4dp n) == round4dp n



return []
runTests = $quickCheckAll