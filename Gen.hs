{-# language TemplateHaskell #-}
module Gen where
import Test.QuickCheck
import Syntax

instance Arbitrary Atom where arbitrary = elements $ Atom <$> ["mary","john","bob"]
instance Arbitrary VariableName where
  arbitrary = VariableName 0 <$> elements ["X","Y","Z"]
    
instance Arbitrary Term where
  arbitrary = do constr <- elements ['Struct, 'Var, 'Wildcard, 'Cut]
                 if | constr == 'Struct   -> Struct <$> arbitrary @Atom <*> listOf (arbitrary @Term)
                    | constr == 'Var      -> Var <$> arbitrary @VariableName
                    | constr == 'Wildcard -> pure Wildcard
                    | constr == 'Cut      -> pure (Cut 0)
          

    

--instance Arbitrary clause where
  --arbitrary = do
    --lhs <- arbitrary @Atom
    --rhs_ <- listOf 
