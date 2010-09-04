{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Control.Monad.Trans.Goto

main = defaultMain
    [testGroup "Functor"
        [testGroup "Identity"
            [testProperty "without goto" $
                \(x :: Int) (y :: Int) → (== x+y) . runGoto . fmap (+y) . return $ x
            ,testProperty "with goto" $
                \(x :: Int) (y :: Int) → (== x) . runGoto . fmap (+y) . goto . return $ x
            ]
        ,testGroup "Maybe"
            [testProperty "without goto" $
                \(x :: Int) (y :: Int) → (== Just (x+y)) . runGotoT . fmap (+y) . lift . Just $ x
            ,testProperty "with goto" $
                \(x :: Int) (y :: Int) → (== Just x) . runGotoT . fmap (+y) . goto . lift . Just $ x
            ]
        ]
    ,testGroup "Applicative"
        [testGroup "Identity"
            [testProperty "without goto" $
                \(x :: Int) (y :: Int) → runGoto (return (+y) <*> return x) == x+y
            ,testProperty "with goto" $
                \(x :: Int) (y :: Int) → runGoto (return (+y) <*> goto (return x)) == x
            ]
        ,testGroup "Maybe"
            [testGroup "Just"
                [testProperty "without goto" $
                    \(x :: Int) (y :: Int) → runGotoT (lift (Just (+y)) <*> lift (Just x)) == Just (x+y)
                ,testProperty "with goto" $
                    \(x :: Int) (y :: Int) → runGotoT (lift (Just (+y)) <*> goto (lift (Just x))) == Just x
                ]
            ]
        ]
    ,testGroup "Monad"
        [testGroup "Maybe"
            [testGroup "Just"
                [testProperty "without goto" $
                    \(x :: Int) (y :: Int) → (== Just (x+y)) . runGotoT $ do
                        a ← lift (Just x)
                        b ← lift (Just y)
                        return (a+b)
                ,testProperty "with goto" $
                    \(x :: Int) (y :: Int) → (== Just x) . runGotoT $ do
                        a ← lift (Just x)
                        goto $ return a
                        b ← lift (Just y)
                        return (a+b)
                ]
            ,testGroup "Nothing"
                [testProperty "without goto" $
                    \(x :: Int) (y :: Int) → (== Nothing) . runGotoT $ do
                        a ← lift (Just x)
                        b ← lift (Just y)
                        lift Nothing
                        return (a+b)
                ,testProperty "with goto" $
                    \(x :: Int) (y :: Int) → (== Just x) . runGotoT $ do
                        a ← lift (Just x)
                        goto $ return a
                        b ← lift (Just y)
                        lift Nothing
                        return (a+b)
                ]
            ]
        ,testGroup "State"
            [testProperty "without goto" $
                \(x :: Int) (y :: Int) → (== x+y) . flip execState x . runGotoT $ do
                    lift (modify (+y))
            ,testProperty "with goto" $
                \(x :: Int) (y :: Int) → (== x) . flip execState x . runGotoT $ do
                    goto $ return ()
                    lift (modify (+y))
            ]
       ]
   ]

       