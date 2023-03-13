{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Hlox.Parse where

import Data.Data (Proxy (..))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Void (Void)
import Hlox.Lex (WithPos (..))
import qualified Hlox.Lex as Lex
import Text.Megaparsec

data TokenStream = TokenStream
  { streamInput :: String,
    unStream :: [WithPos Lex.Token]
  }

proxy :: Proxy TokenStream
proxy = Proxy

instance Stream TokenStream where
  type Token TokenStream = WithPos Lex.Token
  type Tokens TokenStream = [WithPos Lex.Token]

  tokensToChunk Proxy xs = xs

  chunkToTokens Proxy = id

  chunkLength Proxy = length

  take1_ TokenStream {unStream = []} = Nothing
  take1_ TokenStream {streamInput, unStream = x : xs} =
    Just
      ( x,
        TokenStream
          { streamInput = drop (tokensLength proxy (x :| [])) streamInput,
            unStream = xs
          }
      )

  takeN_ n s@TokenStream {streamInput, unStream}
    | n <= 0 = Just ([], s)
    | otherwise =
        let (left, right) = splitAt n unStream
         in fmap
              ( \neleft ->
                  ( left,
                    TokenStream
                      { streamInput = drop (tokensLength proxy neleft) streamInput,
                        unStream = right
                      }
                  )
              )
              -- n > 0 and left is empty iff unStream is empty
              (NE.nonEmpty left)

  takeWhile_ pred s =
    go pred s []
    where
      go pred s@TokenStream {streamInput, unStream} chunk =
        case take1_ s of
          Nothing -> ([], s)
          Just (token, s') ->
            if pred token
              then go pred s' (token : chunk)
              else (chunk, s)

data ShowTokensST = ShowTokensST
  { showLine :: Pos,
    showS :: String
  }

instance VisualStream TokenStream where
  showTokens Proxy tokens =
    go tokens (ShowTokensST {showLine = pos1, showS = ""})
    where
      go tokens (ShowTokensST {showLine, showS}) =
        let (t, ts) = NE.uncons tokens
            start = startPos t
            startLine = sourceLine start
            startCol = sourceColumn start
            showt = show t
            s =
              replicate (unPos startLine - unPos showLine) '\n'
                ++ replicate (unPos startCol - 1) ' '
                ++ showt
         in case ts of
              Nothing -> s
              Just tokens' -> go tokens' ShowTokensST {showLine = startLine, showS = s}

  tokensLength Proxy = sum . fmap tokenLength

instance TraversableStream TokenStream where
  reachOffset o PosState {..} =
    case takeN_ o pstateInput of
      Nothing -> undefined
      Just (pre, pstateInput') ->
        ( Just $ preStr ++ postStr,
          PosState
            { pstateInput = pstateInput',
              pstateOffset = max o pstateOffset,
              pstateSourcePos = sourcePos,
              pstateTabWidth,
              pstateLinePrefix = preStr
            }
        )
        where
          preStr = case NE.nonEmpty pre of
            Nothing -> ""
            Just nePre -> reverse . takeWhile (/= '\n') . reverse . showTokens proxy $ nePre
          postStr = case NE.nonEmpty $ unStream pstateInput' of
            Nothing -> ""
            Just nePost -> takeWhile (/= '\n') . showTokens proxy $ nePost
          sourcePos = case unStream pstateInput' of
            [] -> undefined
            t : _ -> startPos t

  reachOffsetNoLine n s = undefined

-- type Parser = Parsec Void TokenStream

-- data Expr
--   =