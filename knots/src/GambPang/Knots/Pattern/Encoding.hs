{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module GambPang.Knots.Pattern.Encoding (
    decodePattern,
) where

import Control.Applicative (many, (<|>))
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import Data.Functor (void)
import Data.List (nub)
import qualified Data.Set as Set
import Data.Text (Text)
import GambPang.Knots.Pattern (EdgePosition (..), Pattern (Pattern), getEdge)

decodePattern :: Text -> Either String Pattern
decodePattern = A.parseOnly patternP

patternP :: Parser Pattern
patternP =
    Pattern
        <$> widthP
        <*> heightP
        <*> (tilesP <|> pure mempty)
        <*> (edgesP <|> pure mempty)
  where
    widthP = itemP "width" A.decimal
    heightP = itemP "height" A.decimal

    tilesP = Set.fromList <$> itemP "tiles" (pairP `A.sepBy` spaces)
    pairP = do
        _ <- A.char '('
        i <- A.decimal
        A.char ',' >> spaces
        j <- A.decimal
        _ <- spaces >> A.char ')'
        pure (i, j)

    edgesP = Set.fromList . mconcat <$> itemP "edges" (edgeP `A.sepBy` spaces)
    edgeP = do
        boundary <- A.many1 boundaryP
        (i, j) <- pairP
        pure $ getEdge i j <$> nub boundary

    boundaryP =
        A.take 1 >>= \case
            "T" -> pure TopEdge
            "L" -> pure LeftEdge
            "R" -> pure RightEdge
            "B" -> pure BottomEdge
            _ -> fail "Unexpected edge code"

    itemP name p = A.string name >> spaces >> A.char '=' >> spaces >> p <* newlines

space :: Parser Char
space = A.char ' '

spaces :: Parser ()
spaces = void $ many space

newline :: Parser Char
newline = A.char '\n'

newlines :: Parser ()
newlines = void $ many newline
