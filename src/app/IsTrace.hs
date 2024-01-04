module IsTrace (
    IsTrace(..)
    , reproduceFromLine
    , reproduceFromLineCol
) where

import qualified Data.List as L
import qualified Data.Text as T
import TextShow

class (Semigroup a) => IsTrace a where
    trace :: Semigroup a => T.Text -> a -> Builder

-- This and reproduceFromLine are convenience functions that are provided for types
-- implementing IsTrace; see the implementation of IsTrace in Lexer.x as an example
-- reproduceFromLineCol reproduces a section of source from the line and column number
reproduceFromLineCol :: T.Text -> (Int, Int) -> (Int, Int) -> Builder
reproduceFromLineCol origTxt (lineX, colX) (lineY, colY) =
    let asLines = T.lines origTxt
        numsLns = zip [1..(length asLines)] asLines
        relevantNumsLns = take (lineY - lineX + 1) $ drop (lineX - 1) numsLns
        reproducedNumsLns = case relevantNumsLns of
                      [] -> error "reproducedFromRange has no lns"
                      ((num1, firstLine):[]) ->
                          [(num1, T.take (colY - colX + 1) $ T.drop (colX - 1) firstLine)]
                      ((num1, firstLine):(num2, lastLine):[]) ->
                          [(num1, T.drop (colX - 1) firstLine)
                              , (num2, T.take (colY + 1) lastLine)]
                      ((num1, firstLine):xs) ->
                          let (numLast, lastLine) = L.last xs
                              midLines = L.init xs
                              firstLine' = T.drop colY firstLine
                              lastLine' = T.take colY lastLine
                          in (num1, firstLine'):(midLines ++ [(numLast, lastLine')])
    in numsLinesToReproduction reproducedNumsLns

-- reproduceFromLine reproduces a section of source from the line number only
-- (and not the column)
reproduceFromLine :: T.Text -> Int -> Int -> Builder
reproduceFromLine origTxt lineX lineY =
    let asLines = T.lines origTxt
        numsLns = zip [1..(length asLines)] asLines
        relevantNumsLns = take (lineY - lineX + 1) $ drop (lineX - 1) numsLns
    in numsLinesToReproduction relevantNumsLns

numsLinesToReproduction :: [(Int, T.Text)] -> Builder
numsLinesToReproduction numsLines =
    let maxNumTextWidth = length $ show $ fst $ last numsLines
        justifyNum' = justifyNum maxNumTextWidth
        numberedLines = ( \(n, ln) -> (justifyNum' n) <> ln ) <$> numsLines
    in fromText $ T.strip $ T.unlines numberedLines
    where
        justifyNum width n = T.justifyLeft (width + 1) ' ' (T.pack $ show n) <> "│ "

