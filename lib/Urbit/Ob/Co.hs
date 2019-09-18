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
    Patp

  , patp
  , fromPatp

  , render
  , parse
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
newtype Patp = Patp BS.ByteString
  deriving (Eq, Generic)

instance Show Patp where
  show = T.unpack . render

unPatp :: Patp -> BS.ByteString
unPatp (Patp p) = p

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

-- | Convert a \@p value to its corresponding 'Natural'.
--
--   >>> let zod = patp 0
--   >>> fromPatp zod
--   0
--
fromPatp :: Patp -> Natural
fromPatp = Ob.fynd . C.roll . BS.reverse . unPatp

-- | Render a \@p value as 'T.Text'.
--
--   >>> render (patp 0)
--   "~zod"
--   >>> render (patp 15663360)
--   "~nidsut-tomdun"
render :: Patp -> T.Text
render (Patp bs) = render' bs

render' :: BS.ByteString -> T.Text
render' bs =
      T.cons '~'
    . snd
    . BS.foldr alg (0 :: Int, mempty)
    $ padded
  where
    alg val (idx, acc) =
      let syl = if even idx then suffix val else prefix val
          glue
            | idx `mod` 8 == 0 = if idx == 0 then mempty else "--"
            | even idx         = "-"
            | otherwise        = mempty
      in  (succ idx, syl <> glue <> acc)

    padded =
      let len = BS.length bs
      in  if   (odd len && len > 2) || len == 0
          then BS.cons 0 bs
          else bs

-- | Parse a \@p value existing as 'T.Text'.
--
--   >>> parse "~nidsut-tomdun"
--   Right ~nidsut-tomdun
--   > parse "~fipfes-fipfes-fipfes-doznec"
--   Right ~fipfes-fipfes-fipfes-doznec
--
parse :: T.Text -> Either T.Text Patp
parse p =
      fmap (Patp . snd)
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
    Nothing -> Left (msg syl)
    Just x  -> Right (fromIntegral x :: Word8)
  where
    msg s = "urbit-hob (fromPrefix): invalid prefix \"" <> s <> "\""

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
    Nothing -> Left (msg syl)
    Just x  -> Right (fromIntegral x :: Word8)
  where
    msg s = "urbit-hob (fromSuffix): invalid suffix \"" <> s <> "\""

