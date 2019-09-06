{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Co (
    patp
  ) where

import qualified Data.IntMap.Strict as IMS
import Ob (fein)
import qualified Data.Text as T

newtype Patp = Patp T.Text
  deriving Eq

instance Show Patp where
  show = T.unpack . render

render :: Patp -> T.Text
render (Patp p) = T.cons '~' p

patp :: Int -> Maybe Patp
patp n
    | dyx <= 1  = fmap Patp (IMS.lookup sxz suffixes)
    | otherwise = fmap Patp (loop sxz 0 mempty)
  where
    sxz = fein n
    dyx = met 3 sxz
    dyy = met 4 sxz

    loop !tsxz !timp !trep = do
      let lug = end 4 1 tsxz
          etc =
            if   timp `mod` 4 /= 0
            then "-"
            else if   timp == 0
                 then ""
                 else "--"

      pre <- IMS.lookup (rsh 3 1 lug) prefixes
      suf <- IMS.lookup (end 3 1 lug) suffixes

      let res = pre <> suf <> etc <> trep

      if   timp == dyy
      then return trep
      else loop (rsh 4 1 tsxz) (succ timp) res

prefixes :: IMS.IntMap T.Text
prefixes = IMS.fromList $ zip [0..]
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

suffixes :: IMS.IntMap T.Text
suffixes = IMS.fromList $ zip [0..]
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

bex :: Integral a => a -> a
bex = (^) 2

rsh :: Integral a => a -> a -> a -> a
rsh a b c = c `div` bex (bex a * b)

met :: Integral a => a -> a -> a
met = loop 0 where
  loop !acc a !b
    | b == 0    = acc
    | otherwise = loop (succ acc) a (rsh a 1 b)

end :: Integral a => a -> a -> a -> a
end a b c = c `mod` bex (bex a * b)

