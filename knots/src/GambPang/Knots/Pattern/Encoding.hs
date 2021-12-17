{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module GambPang.Knots.Pattern.Encoding (
    decodePattern,
    toWorksheet,
    parseWorksheet,

    -- * Errors
    WorksheetError (..),
) where

import Control.Applicative (many, (<|>))
import Control.Monad ((>=>))
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import Data.Bifunctor (second)
import Data.Functor (void)
import Data.List (nub)
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import GambPang.Knots.Pattern (
    Edge (..),
    EdgePosition (..),
    Pattern (..),
    getEdge,
 )

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

toWorksheet :: Pattern -> Text
toWorksheet p = Text.unlines . reverse $ addPipes . Text.pack . fmap populate <$> positions
  where
    populate pos
        | isWorksheetBox pos =
            if standardPosition p.width pos `Set.member` p.blockedTiles
                then tileMask
                else crossingChar
        | isWorksheetEdge pos && standardEdge p.width pos `Set.member` p.blockedEdges = blockChar
        | otherwise = emptyChar
    positions = [[TiltPosition (i, j) | i <- [0 .. worksheetWidth p]] | j <- [0 .. worksheetHeight p]]
    addPipes x = "| " <> x <> " |"

crossingChar :: Char
crossingChar = '+'

blockChar :: Char
blockChar = 'x'

tileMask :: Char
tileMask = '0'

emptyChar :: Char
emptyChar = ' '

data Feature = FeatureTileBlock TiltPosition | FeatureEdgeBlock TiltPosition
    deriving (Eq, Show)

parseWorksheet :: Text -> Either WorksheetError Pattern
parseWorksheet =
    traverse (parseLine . second (trim . Text.unpack))
        . zip [0 ..]
        . reverse
        . Text.lines
        >=> finalize
  where
    trim xs = take (length xs - 4) $ drop 2 xs
    parseLine (j, row) = (length row,) <$> traverse (parseChar j) (zip [0 ..] row)
    finalize rows
        | equalRows (fst <$> rows) =
            let w = (maximum (fst <$> rows) - 1) `div` 4
                h = (length rows - 1) `div` 4
                features = catMaybes . mconcat $ snd <$> rows
             in pure
                    Pattern
                        { width = w
                        , height = h
                        , blockedTiles = Set.fromList $ mapMaybe (justTile w) features
                        , blockedEdges = Set.fromList $ mapMaybe (justEdge w) features
                        }
        | otherwise = Left UnequalRows

    equalRows xs = all (== maximum xs) xs

    justEdge w = \case
        FeatureEdgeBlock pos -> Just $ standardEdge w pos
        _ -> Nothing
    justTile w = \case
        FeatureTileBlock pos -> Just $ standardPosition w pos
        _ -> Nothing

parseChar :: Int -> (Int, Char) -> Either WorksheetError (Maybe Feature)
parseChar j (i, x)
    | x == blockChar && isWorksheetEdge pos =
        pure . Just $ FeatureEdgeBlock pos
    | x == tileMask && isWorksheetBox pos = pure . Just $ FeatureTileBlock pos
    | x == crossingChar && isWorksheetBox pos = pure Nothing
    | x == emptyChar = pure Nothing
    | otherwise = Left $ InvalidChar i j x
  where
    pos = TiltPosition (i, j)

data WorksheetError = InvalidChar Int Int Char | UnequalRows
    deriving (Eq, Show)

worksheetHeight :: Pattern -> Int
worksheetHeight p = 4 * p.height

worksheetWidth :: Pattern -> Int
worksheetWidth p = 4 * p.width

newtype TiltPosition = TiltPosition (Int, Int)
    deriving (Eq, Show)

standardPosition ::
    -- | Width
    Int ->
    TiltPosition ->
    (Int, Int)
standardPosition w (TiltPosition (i, j)) = (u, v)
  where
    x = i `div` 2
    y = j `div` 2

    u = (x + y + 1) `div` 2
    v = ((y - x + 1) `div` 2) + w

standardEdge ::
    -- | Width
    Int ->
    TiltPosition ->
    Edge
standardEdge w (TiltPosition (i, j)) = Edge (u, v)
  where
    u = ((i + j) `div` 2) + 1
    v = ((j - i) `div` 2) + 2 * w + 1

isWorksheetBox :: TiltPosition -> Bool
isWorksheetBox (TiltPosition (i, j)) = even i && even j && odd ((i + j) `div` 2)

isWorksheetEdge :: TiltPosition -> Bool
isWorksheetEdge (TiltPosition (i, j)) = odd i && odd j
