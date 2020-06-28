{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Urbit.Ob.Co
-- Copyright: (c) 2019 Jared Tobin
-- License: MIT
--
-- Maintainer: Jared Tobin <jared@jtobin.io>
-- Stability: unstable
-- Portability: ghc
--
-- General functions for atom printing.
--
-- Roughly analogous to the +co arm in hoon.hoon.

module Urbit.Ob.Co (
    Patp(..)
  , Patq(..)

  , patp
  , patq

  , fromPatp
  , fromPatq

  , renderPatp
  , renderPatq

  , parsePatp
  , parsePatq
  ) where

import qualified Data.ByteString as BS
import Data.Char (isAsciiLower)
import Data.Foldable (foldrM)
import qualified Data.Serialize.Extended as C
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Word (Word8)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Prelude hiding (log)
import qualified Urbit.Ob.Ob as Ob (fein, fynd)

-- | Hoon's \@p encoding.
--
--   This encoding is an /obfuscated/ representation of some underlying number,
--   but a pronounceable, memorable, and unique one.
--
--   The representation exists for any natural number, but it's typically used
--   only for naming Azimuth points, and thus normal 32-bit Urbit ships.
--
--   (It's also used for naming comets, i.e. self-signed 128-bit Urbit ships.)
--
newtype Patp = Patp {
    unPatp :: BS.ByteString
  } deriving (Eq, Ord, Generic)

instance Show Patp where
  show = T.unpack . renderPatp

-- | Hoon's \@q encoding.
--
--   Unlike \@p, the \@q encoding is a /non-obfuscated/ representation of an
--   atom.
--
--   It's typically used for serializing arbitrary data in a memorable and
--   pronounceable fashion.
--
newtype Patq = Patq {
    unPatq :: BS.ByteString
  } deriving (Eq, Ord, Generic)

instance Show Patq where
  show = T.unpack . renderPatq

-- | Convert a 'Natural' to \@p.
--
--   >>> patp 0
--   ~zod
--   >>> patp 256
--   ~marzod
--   >>> patp 65536
--   ~dapnep-ronmyl
--   >>> patp 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
--   ~fipfes-fipfes-fipfes-fipfes--fipfes-fipfes-fipfes-fipfes
--
patp :: Natural -> Patp
patp = Patp . BS.reverse . C.unroll . Ob.fein

-- | Convert a 'Natural' to \@q.
--
--   >>> patq 0
--   ~zod
--   >>> patq 256
--   ~marzod
--   >>> patq 65536
--   ~nec-dozzod
--   >>> patp 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
--   ~fipfes-fipfes-fipfes-fipfes-fipfes-fipfes-fipfes-fipfes
--
patq :: Natural -> Patq
patq = Patq . BS.reverse . C.unroll

-- | Convert a \@p value to its corresponding 'Natural'.
--
--   >>> let zod = patp 0
--   >>> fromPatp zod
--   0
--
fromPatp :: Patp -> Natural
fromPatp = Ob.fynd . C.roll . BS.reverse . unPatp

-- | Convert a \@q value to its corresponding 'Natural'.
--
--   >>> let zod = patq 0
--   >>> fromPatq zod
--   0
--
fromPatq :: Patq -> Natural
fromPatq = C.roll . BS.reverse . unPatq

-- | Render a \@p value as 'T.Text'.
--
--   >>> renderPatp (patp 0)
--   "~zod"
--   >>> renderPatp (patp 15663360)
--   "~nidsut-tomdun"
renderPatp :: Patp -> T.Text
renderPatp (Patp bs) = render Padding LongSpacing bs

-- | Render a \@p value as 'T.Text'.
--
--   >>> renderPatq (patq 0)
--   "~zod"
--   >>> renderPatq (patq 15663360)
--   "~mun-marzod"
renderPatq :: Patq -> T.Text
renderPatq (Patq bs) = render NoPadding ShortSpacing bs

-- | Parse a \@p value existing as 'T.Text'.
--
--   >>> parsePatp "~nidsut-tomdun"
--   Right ~nidsut-tomdun
--   > parsePatp "~fipfes-fipfes-fipfes-doznec"
--   Right ~fipfes-fipfes-fipfes-doznec
--
parsePatp :: T.Text -> Either T.Text Patp
parsePatp = fmap Patp . parse

-- | Parse a \@q value existing as 'T.Text'.
--
--   >>> parsePatq "~nec-dozzod"
--   Right ~nec-dozzod
--   > parsePatq "~fipfes-fipfes-fipfes-doznec"
--   Right ~fipfes-fipfes-fipfes-doznec
--
parsePatq :: T.Text -> Either T.Text Patq
parsePatq = fmap Patq . parse

-- Padding option for rendering.
data Padding =
    Padding
  | NoPadding
  deriving Eq

-- Spacing option for rendering.
data Spacing =
    LongSpacing
  | ShortSpacing
  deriving Eq

-- General-purpose renderer.
render :: Padding -> Spacing -> BS.ByteString -> T.Text
render padding spacing bs =
      T.cons '~'
    . snd
    . BS.foldr alg (0 :: Int, mempty)
    $ padded
  where
    alg val (idx, acc) =
      let syl = if even idx then suffix val else prefix val
          glue
            | idx `mod` 8 == 0 = if idx == 0 then mempty else dash
            | even idx         = "-"
            | otherwise        = mempty
      in  (succ idx, syl <> glue <> acc)

    padded
      | padCondition = BS.cons 0 bs
      | otherwise    = bs

    dash = case spacing of
      LongSpacing  -> "--"
      ShortSpacing -> "-"

    padCondition =
      let len = BS.length bs
      in  case padding of
            NoPadding -> len == 0
            Padding   -> (odd len && len > 2) || len == 0

-- General-purpose (non-strict) parser.
parse :: T.Text -> Either T.Text BS.ByteString
parse p =
      fmap snd
    $ foldrM alg (0 :: Int, mempty) syls
  where
    alg syl (idx, acc) = do
      word <- if even idx then fromSuffix syl else fromPrefix syl
      return (succ idx, BS.cons word acc)

    syls =
        T.chunksOf 3
      . T.filter isAsciiLower
      $ p

prefixes :: V.Vector T.Text
prefixes = V.fromList
  ["doz","mar","bin","wan","sam","lit","sig","hid","fid","lis","sog","dir"
  ,"wac","sab","wis","sib","rig","sol","dop","mod","fog","lid","hop","dar"
  ,"dor","lor","hod","fol","rin","tog","sil","mir","hol","pas","lac","rov"
  ,"liv","dal","sat","lib","tab","han","tic","pid","tor","bol","fos","dot"
  ,"los","dil","for","pil","ram","tir","win","tad","bic","dif","roc","wid"
  ,"bis","das","mid","lop","ril","nar","dap","mol","san","loc","nov","sit"
  ,"nid","tip","sic","rop","wit","nat","pan","min","rit","pod","mot","tam"
  ,"tol","sav","pos","nap","nop","som","fin","fon","ban","mor","wor","sip"
  ,"ron","nor","bot","wic","soc","wat","dol","mag","pic","dav","bid","bal"
  ,"tim","tas","mal","lig","siv","tag","pad","sal","div","dac","tan","sid"
  ,"fab","tar","mon","ran","nis","wol","mis","pal","las","dis","map","rab"
  ,"tob","rol","lat","lon","nod","nav","fig","nom","nib","pag","sop","ral"
  ,"bil","had","doc","rid","moc","pac","rav","rip","fal","tod","til","tin"
  ,"hap","mic","fan","pat","tac","lab","mog","sim","son","pin","lom","ric"
  ,"tap","fir","has","bos","bat","poc","hac","tid","hav","sap","lin","dib"
  ,"hos","dab","bit","bar","rac","par","lod","dos","bor","toc","hil","mac"
  ,"tom","dig","fil","fas","mit","hob","har","mig","hin","rad","mas","hal"
  ,"rag","lag","fad","top","mop","hab","nil","nos","mil","fop","fam","dat"
  ,"nol","din","hat","nac","ris","fot","rib","hoc","nim","lar","fit","wal"
  ,"rap","sar","nal","mos","lan","don","dan","lad","dov","riv","bac","pol"
  ,"lap","tal","pit","nam","bon","ros","ton","fod","pon","sov","noc","sor"
  ,"lav","mat","mip","fip"]

prefix :: Integral a => a -> T.Text
prefix = V.unsafeIndex prefixes . fromIntegral

fromPrefix :: T.Text -> Either T.Text Word8
fromPrefix syl = case V.findIndex (== syl) prefixes of
    Nothing -> Left msg
    Just x  -> Right (fromIntegral x :: Word8)
  where
    msg = "(urbit-hob) bad parse: invalid prefix \"" <> syl <> "\""

suffixes :: V.Vector T.Text
suffixes = V.fromList
  ["zod","nec","bud","wes","sev","per","sut","let","ful","pen","syt","dur"
  ,"wep","ser","wyl","sun","ryp","syx","dyr","nup","heb","peg","lup","dep"
  ,"dys","put","lug","hec","ryt","tyv","syd","nex","lun","mep","lut","sep"
  ,"pes","del","sul","ped","tem","led","tul","met","wen","byn","hex","feb"
  ,"pyl","dul","het","mev","rut","tyl","wyd","tep","bes","dex","sef","wyc"
  ,"bur","der","nep","pur","rys","reb","den","nut","sub","pet","rul","syn"
  ,"reg","tyd","sup","sem","wyn","rec","meg","net","sec","mul","nym","tev"
  ,"web","sum","mut","nyx","rex","teb","fus","hep","ben","mus","wyx","sym"
  ,"sel","ruc","dec","wex","syr","wet","dyl","myn","mes","det","bet","bel"
  ,"tux","tug","myr","pel","syp","ter","meb","set","dut","deg","tex","sur"
  ,"fel","tud","nux","rux","ren","wyt","nub","med","lyt","dus","neb","rum"
  ,"tyn","seg","lyx","pun","res","red","fun","rev","ref","mec","ted","rus"
  ,"bex","leb","dux","ryn","num","pyx","ryg","ryx","fep","tyr","tus","tyc"
  ,"leg","nem","fer","mer","ten","lus","nus","syl","tec","mex","pub","rym"
  ,"tuc","fyl","lep","deb","ber","mug","hut","tun","byl","sud","pem","dev"
  ,"lur","def","bus","bep","run","mel","pex","dyt","byt","typ","lev","myl"
  ,"wed","duc","fur","fex","nul","luc","len","ner","lex","rup","ned","lec"
  ,"ryd","lyd","fen","wel","nyd","hus","rel","rud","nes","hes","fet","des"
  ,"ret","dun","ler","nyr","seb","hul","ryl","lud","rem","lys","fyn","wer"
  ,"ryc","sug","nys","nyl","lyn","dyn","dem","lux","fed","sed","bec","mun"
  ,"lyr","tes","mud","nyt","byr","sen","weg","fyr","mur","tel","rep","teg"
  ,"pec","nel","nev","fes"]

suffix :: Integral a => a -> T.Text
suffix = V.unsafeIndex suffixes . fromIntegral

fromSuffix :: T.Text -> Either T.Text Word8
fromSuffix syl = case V.findIndex (== syl) suffixes of
    Nothing -> Left msg
    Just x  -> Right (fromIntegral x :: Word8)
  where
    msg = "(urbit-hob) bad parse: invalid suffix \"" <> syl <> "\""

