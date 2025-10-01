{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- | Convert ZM ADTs to TypeScript data definitions
module ZM.To.Dart
  ( generate,
    generateModule,
    -- , library
    defaultPrimitiveTypes,
  )
where

-- import           Data.Char                      ( chr
--                                                 , ord
--                                                 , toLower
--                                                 )

-- Pretty printing

import Data.Bifunctor
import Data.Bool
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import ZM
import ZM.Parser (mdl)
import ZM.To.Util

-- import           FileEmbed
-- import qualified Data.Set                      as Set
-- import           Data.Word

-- |
-- TODO:
-- - make objects into pure unmodifiable values. (no write access to fields)
-- - generate also semi-custom classes (Word/Ints,List,String..)
--   - Use js patching  to customise classes.
--
-- NOTE:
-- Constructors whose name clashes with the data type name:
-- A = A .. | ..
--
-- are rewritten as:
-- A = _A ...
--
-- \$setup

{- A simple ADT
>>> import Data.Word
>>> p0 = Proxy :: Proxy [Bool]
>>> p1 = Proxy :: Proxy AbsADT
>>> p2 = Proxy :: Proxy (BLOB Bool) -- (ByType Bit)
>>> p3 = Proxy :: Proxy (Either Bool [Bool]) -- (ByType Bit)
>>> p4 = Proxy :: Proxy Word8
>>> f1 = Flags { namespace = ["zm","adt"], primTypes = defaultPrimitiveTypes, addIndex  = False}
>>> gen p = generate f1 (absEnv p)
>>> g p = error . T.unpack .  mdlContent <$> gen p
>>> save = mapM_ (mdlWrite WriteFlags { srcDir    = "/Users/titto/workspace/flutter/notifications/lib", overwrite = True})

>>> gen (Proxy :: Proxy Bool)
[Module {mdlExtension = "dart", mdlPath = ["zm","adt","bool_k306f1981b41c"], mdlContent = "/*\nZM Type:\nBool \8801   False\n       | True\n*/\n\nimport '../../zm.dart' as zm;\n\nzm.ZMFold<Bool> $$Bool= <R>(zm.Folder<R> f) {return f(info_,[]);};\n\nzm.Decoder<Bool> $Bool = (zm.DecoderState st) {if (st.zero()) { return False(); } else { return True(); }};\n\nconst info_ = zm.ZMTypeInfo(\n\"Bool\",\n(0x30,0x6f,0x19,0x81,0xb4,0x1c),\n);\n\n\nsealed class Bool  extends zm.ZM {\n}\n\n\nclass False  extends Bool {\n\n}\n\nclass True  extends Bool {\n\n}\n\n\n"}]

>>> save $ gen (Proxy :: Proxy (Either Bool Int))

>>> save $ gen (Proxy :: Proxy (Maybe Bool))

>>> gen (Proxy :: Proxy Bool)
[Module {mdlExtension = "dart", mdlPath = ["zm","adt","bool_k306f1981b41c"], mdlContent = "/*\nZM Type:\nBool \8801   False\n       | True\n*/\n\nimport '../../zm.dart' as zm;\n\nzm.ZMFold<Bool> $$Bool= <R>(zm.Folder<R> f) {return f(info_,[]);};\n\nzm.Decoder<Bool> $Bool = (zm.DecoderState st) {if (st.zero()) { return False(); } else { return True(); }};\n\nconst info_ = zm.ZMTypeInfo(\n\"Bool\",\n(0x30,0x6f,0x19,0x81,0xb4,0x1c),\n);\n\n\nsealed class Bool  extends zm.ZM {\n}\n\n\nclass False  extends Bool {\n\n}\n\nclass True  extends Bool {\n\n}\n\n\n"}]

A primitive type
>>> gen (Proxy :: Proxy Word8)
[Module {mdlExtension = "dart", mdlPath = ["zm","adt","word8_kb1f46a49c8f8"], mdlContent = "/*\nZM Type:\nWord8 \8801   V0\n        | V1\n        | V2\n        | V3\n        | V4\n        | V5\n        | V6\n        | V7\n        | V8\n        | V9\n        | V10\n        | V11\n        | V12\n        | V13\n        | V14\n        | V15\n        | V16\n        | V17\n        | V18\n        | V19\n        | V20\n        | V21\n        | V22\n        | V23\n        | V24\n        | V25\n        | V26\n        | V27\n        | V28\n        | V29\n        | V30\n        | V31\n        | V32\n        | V33\n        | V34\n        | V35\n        | V36\n        | V37\n        | V38\n        | V39\n        | V40\n        | V41\n        | V42\n        | V43\n        | V44\n        | V45\n        | V46\n        | V47\n        | V48\n        | V49\n        | V50\n        | V51\n        | V52\n        | V53\n        | V54\n        | V55\n        | V56\n        | V57\n        | V58\n        | V59\n        | V60\n        | V61\n        | V62\n        | V63\n        | V64\n        | V65\n        | V66\n        | V67\n        | V68\n        | V69\n        | V70\n        | V71\n  \n*/\n\nimport '../../zm.dart' as zm;\n\nzm.ZMFold<Word8> $$Word8= <R>(zm.Folder<R> f) {return f(info_,[]);};\n\nzm.Decoder<Word8> $Word8 = (zm.DecoderState st) {if (st.zero()) { if (st.zero()) { if (st.zero()) { if (st.zero()) { if (st.zero()) { if (st.zero()) { if (st.zero()) { if (st.zero()) { return V0(); } else { return V1(); } } else { if (st.zero()) { return V2(); } else { return V3(); } } } else { if (st.zero()) { if (st.zero()) { return V4(); } else { return V5(); } } else { if (st.zero()) { return V6(); } else { return V7(); } } } } else { if (st.zero()) { if (st.zero()) { if (st.zero()) { return V8(); } else { return V9(); } } else { if (st.zero()) { return V10(); } else { return V11(); } } } else { if (st.zero()) { if (st.zero()) { return V12(); } else { return V13(); } } else { if (st.zero()) { return V14(); } else { return V15(); } } } } } else { if (st.zero()) { if (st.zero()) { if (st.zero()) { if (st.zero()) { return V16(); } else { return V17(); } } else { if (st.zero()) { return V18(); } else { return V19(); } } } else { if (st.zero()) { if (st.zero()) { return V20(); } else { return V21(); } } else { if (st.zero()) { return V22(); } else { return V23(); } } } } else { if (st.zero()) { if (st.zero()) { if (st.zero()) { return V24(); } else { return V25(); } } else { if (st.zero()) { return V26(); } else { return V27(); } } } else { if (st.zero()) { if (st.zero()) { return V28(); } else { return V29(); } } else { if (st.zero()) { return V30(); } else { return V31(); } } } } } } else { if (st.zero()) { if (st.zero()) { if (st.zero()) { if (st.zero()) { if (st.zero()) { return V32(); } else { return V33(); } } else { if (st.zero()) { return V34(); } else { return V35(); } } } else { if (st.zero()) { if (st.zero()) { return V36(); } else { return V37(); } } else { if (st.zero()) { return V38(); } else { return V39(); } } } } else { if (st.zero()) { if (st.zero()) { if (st.zero()) { return V40(); } else { return V41(); } } else { if (st.zero()) { return V42(); } else { return V43(); } } } else { if (st.zero()) { if (st.zero()) { return V44(); } else { return V45(); } } else { if (st.zero()) { return V46(); } else { return V47(); } } } } } else { if (st.zero()) { if (st.zero()) { if (st.zero()) { if (st.zero()) { return V48(); } else { return V49(); } } else { if (st.zero()) { return V50(); } else { return V51(); } } } else { if (st.zero()) { if (st.zero()) { return V52(); } else { return V53(); } } else { if (st.zero()) { return V54(); } else { return V55(); } } } } else { if (st.zero()) { if (st.zero()) { if (st.zero()) { return V56(); } else { return V57(); } } else { if (st.zero()) { return V58(); } else { return V59(); } } } else { if (st.zero()) { if (st.zero()) { return V60(); } else { return V61(); } } else { if (st.zero()) { return V62(); } else { return V63(); } } } } } } } else { if (st.zero()) { if (st.zero()) { if (st.zero()) { if (st.zero()) { if (st.zero()) { if (st.zero()) { return V64(); } else { return V65(); } } else { if (st.zero()) { return V66(); } else { return V67(); } } } else { if (st.zero()) { if (st.zero()) { return V68(); } else { return V69(); } } else { if (st.zero()) { return V70(); } else { return V71(); } } } } else { if (st.zero()) { if (st.zero()) { if (st.zero()) { return V72(); } else { return V73(); } } else { if (st.zero()) { return V74(); } else { return V75(); } } } else { if (st.zero()) { if (st.zero()) { return V76(); } else { return V77(); } } else { if (st.zero()) { return V78(); } else { return V79(); } } } } } else { if (st.zero()) { if (st.zero()) { if (st.zero()) { if (st.zero()) { return V80(); } else { return V81(); } } else { if (st.zero()) { return V82(); } else { return V83(); } } } else { if (st.zero()) { if (st.zero()) { return V84(); } else { return V85(); } } else { if (st.zero()) { return V86(); } else { return V87(); } } } } else { if (st.zero()) { if (st.zero()) { if (st.zero()) { return V88(); } else { return V89(); } } else { if (st.zero()) { return V90(); } else { return V91(); } } } else { if (st.zero()) { if (st.zero()) { return V92(); } else { return V93(); } } else { if (st.zero()) { return V94(); } else { return V95(); } } } } } } else { if (st.zero()) { if (st.zero()) { if (st.zero()) { if (st.zero()) { if (st.zero()) { return V96(); } else { return V97(); } } else { if (st.zero()) { return V98(); } else { return V99(); } } } else { if (st.zero()) { if (st.zero()) { return V100(); } else { return V101(); } } else { if (st.zero()) { return V102(); } else { return V103(); } } } } else { if (st.zero()) { if (st.zero()) { if (st.zero()) { return V104(); } else { return V105(); } } else { if (st.zero()) { return V106(); } else { return V107(); } } } else { if (st.zero()) { if (st.zero()) { return V108(); } else { return V109(); } } else { if (st.zero()) { return V110(); } else { return V111(); } } } } } else { if (st.zero()) { if (st.zero()) { if (st.zero()) { if (st.zero()) { return V112(); } else { return V113(); } } else { if (st.zero()) { return V114(); } else { return V115(); } } } else { if (st.zero()) { if (st.zero()) { return V116(); } else { return V117(); } } else { if (st.zero()) { return V118(); } else { return V119(); } } } } else { if (st.zero()) { if (st.zero()) { if (st.zero()) { return V120(); } else { return V121(); } } else { if (st.zero()) { return V122(); } else { return V123(); } } } else { if (st.zero()) { if (st.zero()) { return V124(); } else { return V125(); } } else { if (st.zero()) { return V126(); } else { return V127(); } } } } } } } } else { if (st.zero()) { if (st.zero()) { if (st.zero()) { if (st.zero()) { if (st.zero()) { if (st.zero()) { if (st.zero()) { return V128(); } else { return V129(); } } else { if (st.zero()) { return V130(); } else { return V131(); } } } else { if (st.zero()) { if (st.zero()) { return V132(); } else { return V133(); } } else { if (st.zero()) { return V134(); } else { return V135(); } } } } else { if (st.zero()) { if (st.zero()) { if (st.zero()) { return V136(); } else { return V137(); } } else { if (st.zero()) { return V138(); } else { return V139(); } } } else { if (st.zero()) { if (st.zero()) { return V140(); } else { return V141(); } } else { if (st.zero()) { return V142(); } else { return V143(); } } } } } else { if (st.zero()) { if (st.zero()) { if (st.zero()) { if (st.zero()) { return V144(); } else { return V145(); } } else { if (st.zero()) { return V146(); } else { return V147(); } } } else { if (st.zero()) { if (st.zero()) { return V148(); } else { return V149(); } } else { if (st.zero()) { return V150(); } else { return V151(); } } } } else { if (st.zero()) { if (st.zero()) { if (st.zero()) { return V152(); } else { return V153(); } } else { if (st.zero()) { return V154(); } else { return V155(); } } } else { if (st.zero()) { if (st.zero()) { return V156(); } else { return V157(); } } else { if (st.zero()) { return V158(); } else { return V159(); } } } } } } else { if (st.zero()) { if (st.zero()) { if (st.zero()) { if (st.zero()) { if (st.zero()) { return V160(); } else { return V161(); } } else { if (st.zero()) { return V162(); } else { return V163(); } } } else { if (st.zero()) { if (st.zero()) { return V164(); } else { return V165(); } } else { if (st.zero()) { return V166(); } else { return V167(); } } } } else { if (st.zero()) { if (st.zero()) { if (st.zero()) { return V168(); } else { return V169(); } } else { if (st.zero()) { return V170(); } else { return V171(); } } } else { if (st.zero()) { if (st.zero()) { return V172(); } else { return V173(); } } else { if (st.zero()) { return V174(); } else { return V175(); } } } } } else { if (st.zero()) { if (st.zero()) { if (st.zero()) { if (st.zero()) { return V176(); } else { return V177(); } } else { if (st.zero()) { return V178(); } else { return V179(); } } } else { if (st.zero()) { if (st.zero()) { return V180(); } else { return V181(); } } else { if (st.zero()) { return V182(); } else { return V183(); } } } } else { if (st.zero()) { if (st.zero()) { if (st.zero()) { return V184(); } else { return V185(); } } else { if (st.zero()) { return V186(); } else { return V187(); } } } else { if (st.zero()) { if (st.zero()) { return V188(); } else { return V189(); } } else { if (st.zero()) { return V190(); } else { return V191(); } } } } } } } else { if (st.zero()) { if (st.zero()) { if (st.zero()) { if (st.zero()) { if (st.zero()) { if (st.zero()) { return V192(); } else { return V193(); } } else { if (st.zero()) { return V194(); } else { return V195(); } } } else { if (st.zero()) { if (st.zero()) { return V196(); } else { return V197(); } } else { if (st.zero()) { return V198(); } else { return V199(); } } } } else { if (st.zero()) { if (st.zero()) { if (st.zero()) { return V200(); } else { return V201(); } } else { if (st.zero()) { return V202(); } else { return V203(); } } } else { if (st.zero()) { if (st.zero()) { return V204(); } else { return V205(); } } else { if (st.zero()) { return V206(); } else { return V207(); } } } } } else { if (st.zero()) { if (st.zero()) { if (st.zero()) { if (st.zero()) { return V208(); } else { return V209(); } } else { if (st.zero()) { return V210(); } else { return V211(); } } } else { if (st.zero()) { if (st.zero()) { return V212(); } else { return V213(); } } else { if (st.zero()) { return V214(); } else { return V215(); } } } } else { if (st.zero()) { if (st.zero()) { if (st.zero()) { return V216(); } else { return V217(); } } else { if (st.zero()) { return V218(); } else { return V219(); } } } else { if (st.zero()) { if (st.zero()) { return V220(); } else { return V221(); } } else { if (st.zero()) { return V222(); } else { return V223(); } } } } } } else { if (st.zero()) { if (st.zero()) { if (st.zero()) { if (st.zero()) { if (st.zero()) { return V224(); } else { return V225(); } } else { if (st.zero()) { return V226(); } else { return V227(); } } } else { if (st.zero()) { if (st.zero()) { return V228(); } else { return V229(); } } else { if (st.zero()) { return V230(); } else { return V231(); } } } } else { if (st.zero()) { if (st.zero()) { if (st.zero()) { return V232(); } else { return V233(); } } else { if (st.zero()) { return V234(); } else { return V235(); } } } else { if (st.zero()) { if (st.zero()) { return V236(); } else { return V237(); } } else { if (st.zero()) { return V238(); } else { return V239(); } } } } } else { if (st.zero()) { if (st.zero()) { if (st.zero()) { if (st.zero()) { return V240(); } else { return V241(); } } else { if (st.zero()) { return V242(); } else { return V243(); } } } else { if (st.zero()) { if (st.zero()) { return V244(); } else { return V245(); } } else { if (st.zero()) { return V246(); } else { return V247(); } } } } else { if (st.zero()) { if (st.zero()) { if (st.zero()) { return V248(); } else { return V249(); } } else { if (st.zero()) { return V250(); } else { return V251(); } } } else { if (st.zero()) { if (st.zero()) { return V252(); } else { return V253(); } } else { if (st.zero()) { return V254(); } else { return V255(); } } } } } } } }};\n\nconst info_ = zm.ZMTypeInfo(\n\"Word8\",\n(0xb1,0xf4,0x6a,0x49,0xc8,0xf8),\n);\n\n\nsealed class Word8  extends zm.ZM {\n}\n\n\nclass V0  extends Word8 {\n\n}\n\nclass V1  extends Word8 {\n\n}\n\nclass V2  extends Word8 {\n\n}\n\nclass V3  extends Word8 {\n\n}\n\nclass V4  extends Word8 {\n\n}\n\nclass V5  extends Word8 {\n\n}\n\nclass V6  extends Word8 {\n\n}\n\nclass V7  extends Word8 {\n\n}\n\nclass V8  extends Word8 {\n\n}\n\nclass V9  extends Word8 {\n\n}\n\nclass V10  extends Word8 {\n\n}\n\nclass V11  extends Word8 {\n\n}\n\nclass V12  extends Word8 {\n\n}\n\nclass V13  extends Word8 {\n\n}\n\nclass V14  extends Word8 {\n\n}\n\nclass V15  extends Word8 {\n\n}\n\nclass V16  extends Word8 {\n\n}\n\nclass V17  extends Word8 {\n\n}\n\nclass V18  extends Word8 {\n\n}\n\nclass V19  extends Word8 {\n\n}\n\nclass V20  extends Word8 {\n\n}\n\nclass V21  extends Word8 {\n\n}\n\nclass V22  extends Word8 {\n\n}\n\nclass V23  extends Word8 {\n\n}\n\nclass V24  extends Word8 {\n\n}\n\nclass V25  extends Word8 {\n\n}\n\nclass V26  extends Word8 {\n\n}\n\nclass V27  extends Word8 {\n\n}\n\nclass V28  extends Word8 {\n\n}\n\nclass V29  extends Word8 {\n\n}\n\nclass V30  extends Word8 {\n\n}\n\nclass V31  extends Word8 {\n\n}\n\nclass V32  extends Word8 {\n\n}\n\nclass V33  extends Word8 {\n\n}\n\nclass V34  extends Word8 {\n\n}\n\nclass V35  extends Word8 {\n\n}\n\nclass V36  extends Word8 {\n\n}\n\nclass V37  extends Word8 {\n\n}\n\nclass V38  extends Word8 {\n\n}\n\nclass V39  extends Word8 {\n\n}\n\nclass V40  extends Word8 {\n\n}\n\nclass V41  extends Word8 {\n\n}\n\nclass V42  extends Word8 {\n\n}\n\nclass V43  extends Word8 {\n\n}\n\nclass V44  extends Word8 {\n\n}\n\nclass V45  extends Word8 {\n\n}\n\nclass V46  extends Word8 {\n\n}\n\nclass V47  extends Word8 {\n\n}\n\nclass V48  extends Word8 {\n\n}\n\nclass V49  extends Word8 {\n\n}\n\nclass V50  extends Word8 {\n\n}\n\nclass V51  extends Word8 {\n\n}\n\nclass V52  extends Word8 {\n\n}\n\nclass V53  extends Word8 {\n\n}\n\nclass V54  extends Word8 {\n\n}\n\nclass V55  extends Word8 {\n\n}\n\nclass V56  extends Word8 {\n\n}\n\nclass V57  extends Word8 {\n\n}\n\nclass V58  extends Word8 {\n\n}\n\nclass V59  extends Word8 {\n\n}\n\nclass V60  extends Word8 {\n\n}\n\nclass V61  extends Word8 {\n\n}\n\nclass V62  extends Word8 {\n\n}\n\nclass V63  extends Word8 {\n\n}\n\nclass V64  extends Word8 {\n\n}\n\nclass V65  extends Word8 {\n\n}\n\nclass V66  extends Word8 {\n\n}\n\nclass V67  extends Word8 {\n\n}\n\nclass V68  extends Word8 {\n\n}\n\nclass V69  extends Word8 {\n\n}\n\nclass V70  extends Word8 {\n\n}\n\nclass V71  extends Word8 {\n\n}\n\nclass V72  extends Word8 {\n\n}\n\nclass V73  extends Word8 {\n\n}\n\nclass V74  extends Word8 {\n\n}\n\nclass V75  extends Word8 {\n\n}\n\nclass V76  extends Word8 {\n\n}\n\nclass V77  extends Word8 {\n\n}\n\nclass V78  extends Word8 {\n\n}\n\nclass V79  extends Word8 {\n\n}\n\nclass V80  extends Word8 {\n\n}\n\nclass V81  extends Word8 {\n\n}\n\nclass V82  extends Word8 {\n\n}\n\nclass V83  extends Word8 {\n\n}\n\nclass V84  extends Word8 {\n\n}\n\nclass V85  extends Word8 {\n\n}\n\nclass V86  extends Word8 {\n\n}\n\nclass V87  extends Word8 {\n\n}\n\nclass V88  extends Word8 {\n\n}\n\nclass V89  extends Word8 {\n\n}\n\nclass V90  extends Word8 {\n\n}\n\nclass V91  extends Word8 {\n\n}\n\nclass V92  extends Word8 {\n\n}\n\nclass V93  extends Word8 {\n\n}\n\nclass V94  extends Word8 {\n\n}\n\nclass V95  extends Word8 {\n\n}\n\nclass V96  extends Word8 {\n\n}\n\nclass V97  extends Word8 {\n\n}\n\nclass V98  extends Word8 {\n\n}\n\nclass V99  extends Word8 {\n\n}\n\nclass V100  extends Word8 {\n\n}\n\nclass V101  extends Word8 {\n\n}\n\nclass V102  extends Word8 {\n\n}\n\nclass V103  extends Word8 {\n\n}\n\nclass V104  extends Word8 {\n\n}\n\nclass V105  extends Word8 {\n\n}\n\nclass V106  extends Word8 {\n\n}\n\nclass V107  extends Word8 {\n\n}\n\nclass V108  extends Word8 {\n\n}\n\nclass V109  extends Word8 {\n\n}\n\nclass V110  extends Word8 {\n\n}\n\nclass V111  extends Word8 {\n\n}\n\nclass V112  extends Word8 {\n\n}\n\nclass V113  extends Word8 {\n\n}\n\nclass V114  extends Word8 {\n\n}\n\nclass V115  extends Word8 {\n\n}\n\nclass V116  extends Word8 {\n\n}\n\nclass V117  extends Word8 {\n\n}\n\nclass V118  extends Word8 {\n\n}\n\nclass V119  extends Word8 {\n\n}\n\nclass V120  extends Word8 {\n\n}\n\nclass V121  extends Word8 {\n\n}\n\nclass V122  extends Word8 {\n\n}\n\nclass V123  extends Word8 {\n\n}\n\nclass V124  extends Word8 {\n\n}\n\nclass V125  extends Word8 {\n\n}\n\nclass V126  extends Word8 {\n\n}\n\nclass V127  extends Word8 {\n\n}\n\nclass V128  extends Word8 {\n\n}\n\nclass V129  extends Word8 {\n\n}\n\nclass V130  extends Word8 {\n\n}\n\nclass V131  extends Word8 {\n\n}\n\nclass V132  extends Word8 {\n\n}\n\nclass V133  extends Word8 {\n\n}\n\nclass V134  extends Word8 {\n\n}\n\nclass V135  extends Word8 {\n\n}\n\nclass V136  extends Word8 {\n\n}\n\nclass V137  extends Word8 {\n\n}\n\nclass V138  extends Word8 {\n\n}\n\nclass V139  extends Word8 {\n\n}\n\nclass V140  extends Word8 {\n\n}\n\nclass V141  extends Word8 {\n\n}\n\nclass V142  extends Word8 {\n\n}\n\nclass V143  extends Word8 {\n\n}\n\nclass V144  extends Word8 {\n\n}\n\nclass V145  extends Word8 {\n\n}\n\nclass V146  extends Word8 {\n\n}\n\nclass V147  extends Word8 {\n\n}\n\nclass V148  extends Word8 {\n\n}\n\nclass V149  extends Word8 {\n\n}\n\nclass V150  extends Word8 {\n\n}\n\nclass V151  extends Word8 {\n\n}\n\nclass V152  extends Word8 {\n\n}\n\nclass V153  extends Word8 {\n\n}\n\nclass V154  extends Word8 {\n\n}\n\nclass V155  extends Word8 {\n\n}\n\nclass V156  extends Word8 {\n\n}\n\nclass V157  extends Word8 {\n\n}\n\nclass V158  extends Word8 {\n\n}\n\nclass V159  extends Word8 {\n\n}\n\nclass V160  extends Word8 {\n\n}\n\nclass V161  extends Word8 {\n\n}\n\nclass V162  extends Word8 {\n\n}\n\nclass V163  extends Word8 {\n\n}\n\nclass V164  extends Word8 {\n\n}\n\nclass V165  extends Word8 {\n\n}\n\nclass V166  extends Word8 {\n\n}\n\nclass V167  extends Word8 {\n\n}\n\nclass V168  extends Word8 {\n\n}\n\nclass V169  extends Word8 {\n\n}\n\nclass V170  extends Word8 {\n\n}\n\nclass V171  extends Word8 {\n\n}\n\nclass V172  extends Word8 {\n\n}\n\nclass V173  extends Word8 {\n\n}\n\nclass V174  extends Word8 {\n\n}\n\nclass V175  extends Word8 {\n\n}\n\nclass V176  extends Word8 {\n\n}\n\nclass V177  extends Word8 {\n\n}\n\nclass V178  extends Word8 {\n\n}\n\nclass V179  extends Word8 {\n\n}\n\nclass V180  extends Word8 {\n\n}\n\nclass V181  extends Word8 {\n\n}\n\nclass V182  extends Word8 {\n\n}\n\nclass V183  extends Word8 {\n\n}\n\nclass V184  extends Word8 {\n\n}\n\nclass V185  extends Word8 {\n\n}\n\nclass V186  extends Word8 {\n\n}\n\nclass V187  extends Word8 {\n\n}\n\nclass V188  extends Word8 {\n\n}\n\nclass V189  extends Word8 {\n\n}\n\nclass V190  extends Word8 {\n\n}\n\nclass V191  extends Word8 {\n\n}\n\nclass V192  extends Word8 {\n\n}\n\nclass V193  extends Word8 {\n\n}\n\nclass V194  extends Word8 {\n\n}\n\nclass V195  extends Word8 {\n\n}\n\nclass V196  extends Word8 {\n\n}\n\nclass V197  extends Word8 {\n\n}\n\nclass V198  extends Word8 {\n\n}\n\nclass V199  extends Word8 {\n\n}\n\nclass V200  extends Word8 {\n\n}\n\nclass V201  extends Word8 {\n\n}\n\nclass V202  extends Word8 {\n\n}\n\nclass V203  extends Word8 {\n\n}\n\nclass V204  extends Word8 {\n\n}\n\nclass V205  extends Word8 {\n\n}\n\nclass V206  extends Word8 {\n\n}\n\nclass V207  extends Word8 {\n\n}\n\nclass V208  extends Word8 {\n\n}\n\nclass V209  extends Word8 {\n\n}\n\nclass V210  extends Word8 {\n\n}\n\nclass V211  extends Word8 {\n\n}\n\nclass V212  extends Word8 {\n\n}\n\nclass V213  extends Word8 {\n\n}\n\nclass V214  extends Word8 {\n\n}\n\nclass V215  extends Word8 {\n\n}\n\nclass V216  extends Word8 {\n\n}\n\nclass V217  extends Word8 {\n\n}\n\nclass V218  extends Word8 {\n\n}\n\nclass V219  extends Word8 {\n\n}\n\nclass V220  extends Word8 {\n\n}\n\nclass V221  extends Word8 {\n\n}\n\nclass V222  extends Word8 {\n\n}\n\nclass V223  extends Word8 {\n\n}\n\nclass V224  extends Word8 {\n\n}\n\nclass V225  extends Word8 {\n\n}\n\nclass V226  extends Word8 {\n\n}\n\nclass V227  extends Word8 {\n\n}\n\nclass V228  extends Word8 {\n\n}\n\nclass V229  extends Word8 {\n\n}\n\nclass V230  extends Word8 {\n\n}\n\nclass V231  extends Word8 {\n\n}\n\nclass V232  extends Word8 {\n\n}\n\nclass V233  extends Word8 {\n\n}\n\nclass V234  extends Word8 {\n\n}\n\nclass V235  extends Word8 {\n\n}\n\nclass V236  extends Word8 {\n\n}\n\nclass V237  extends Word8 {\n\n}\n\nclass V238  extends Word8 {\n\n}\n\nclass V239  extends Word8 {\n\n}\n\nclass V240  extends Word8 {\n\n}\n\nclass V241  extends Word8 {\n\n}\n\nclass V242  extends Word8 {\n\n}\n\nclass V243  extends Word8 {\n\n}\n\nclass V244  extends Word8 {\n\n}\n\nclass V245  extends Word8 {\n\n}\n\nclass V246  extends Word8 {\n\n}\n\nclass V247  extends Word8 {\n\n}\n\nclass V248  extends Word8 {\n\n}\n\nclass V249  extends Word8 {\n\n}\n\nclass V250  extends Word8 {\n\n}\n\nclass V251  extends Word8 {\n\n}\n\nclass V252  extends Word8 {\n\n}\n\nclass V253  extends Word8 {\n\n}\n\nclass V254  extends Word8 {\n\n}\n\nclass V255  extends Word8 {\n\n}\n\n\n"}]

>>> gen (Proxy :: Proxy (Either Bool Bool))
[Module {mdlExtension = "dart", mdlPath = ["zm","adt","bool_k306f1981b41c"], mdlContent = "/*\nZM Type:\nBool \8801   False\n       | True\n*/\n\nimport '../../zm.dart' as zm;\n\nzm.ZMFold<Bool> $$Bool= <R>(zm.Folder<R> f) {return f(info_,[]);};\n\nzm.Decoder<Bool> $Bool = (zm.DecoderState st) {if (st.zero()) { return False(); } else { return True(); }};\n\nconst info_ = zm.ZMTypeInfo(\n\"Bool\",\n(0x30,0x6f,0x19,0x81,0xb4,0x1c),\n);\n\n\nsealed class Bool  extends zm.ZM {\n}\n\n\nclass False  extends Bool {\n\n}\n\nclass True  extends Bool {\n\n}\n\n\n"},Module {mdlExtension = "dart", mdlPath = ["zm","adt","either_k6260e465ae74"], mdlContent = "/*\nZM Type:\nEither a b \8801   Left a\n             | Right b\n*/\n\nimport '../../zm.dart' as zm;\n\nzm.ZMFold<Either<A,B>> $$Either<A extends zm.ZM,B extends zm.ZM>(zm.ZMFold<A> t1,zm.ZMFold<B> t2) {return <R>(zm.Folder<R> f) {return f(info_,[t1(f),t2(f)]);};}\n\nzm.Decoder<Either<A,B>> $Either<A extends zm.ZM,B extends zm.ZM>(zm.Decoder<A> t1,zm.Decoder<B> t2) {return (zm.DecoderState st) {if (st.zero()) { return Left(t1(st)); } else { return Right(t2(st)); }};}\n\nconst info_ = zm.ZMTypeInfo(\n\"Either\",\n(0x62,0x60,0xe4,0x65,0xae,0x74),\n);\n\n\nsealed class Either <A extends zm.ZM,B extends zm.ZM> extends zm.ZM {\n}\n\n\nclass Left <A extends zm.ZM,B extends zm.ZM> extends Either<A,B> {\nfinal A p0;\n\nLeft(this.p0);\n\n}\n\nclass Right <A extends zm.ZM,B extends zm.ZM> extends Either<A,B> {\nfinal B p0;\n\nRight(this.p0);\n\n}\n\n\n"}]
-}

-- -- x = prettyShow $ flat $ absType p4
--
-- -- t = mapM_
-- --   (mdlWrite WriteFlags { srcDir    = "/Users/titto/workspace/ts"
-- --                        , overwrite = True
-- --                        }
-- --   )
-- --   genT

-- p4 = Proxy :: Proxy RepoProtocol

-- | ZM Types that are mapped to primitive JS types
defaultPrimitiveTypes =
  M.fromList
    [ ("Word8.Kb1f46a49c8f8", ValueType "number"),
      ("Word7.Kf4c946334a7e", ValueType "number"),
      ("Word16.K295e24d62fac", ValueType "number"),
      ("Word32.K2412799c99f1", ValueType "number"),
      ("Char.K066db52af145", ValueType "string"),
      ("Array.K2e8b4519aeaa", ValueType "A[]"),
      ("Bytes.Kf8844385a443", ValueType "Uint8Array"),
      ("String.K2f006595638c", ValueType "string"),
      ("Filler.Kae1dfeece189", ValueType "string")
      -- ,("List.Kb8cd13187198",PrimType)
    ]

type TFlags = Flags PrimType

data PrimType = PrimType | ValueType {jsType :: T.Text}

-- instance Convertible ValueType String where safeConvert = Right . T.unpack . ref

-- | Generate one module for every ZM data type in the provided environment, plus an optional index module
generate :: TFlags -> AbsEnv -> [Module]
generate flags env =
  let mdls = map (generateModule flags env) (M.toList env)
   in if addIndex flags then generateIndex flags env : mdls else mdls

-- | Generate an index module
generateIndex :: TFlags -> AbsEnv -> Module
generateIndex flags env =
  dartModule ["all"] $
    imports
      (T.concat $ "./" : map (`T.snoc` '/') (namespaceT flags))
      env
      (M.keys env)

-- Does not use all flags
-- generateModule :: TFlags -> AbsEnv -> AbsRef -> Module
-- generateModuleO flags adtEnv adtRef = generateModule flags adtEnv (M.lookup )

-- dartModuleName adtName adtRef = undered $ moduleShortName_ adtName adtRef

-- dartModuleName adtName adtRef = fileRef adtRef

-- | Generate the indicated module
generateModule :: TFlags -> AbsEnv -> (AbsRef, AbsADT) -> Module
generateModule flags adtEnv (adtRef, adt) =
  let ns :: [Text] = namespaceT flags
      -- e.g. Bit
      name = declNameT adt
      -- e.g. Bit.K65149ce3b366
      absName = mdlRef adtEnv adtRef -- moduleShortName name adtRef
      filename = mdlRef adtEnv adtRef -- dartModuleName (T.toLower name) adtRef
      mct :: Maybe (ConTree Identifier Text) = (tsRef adtEnv name <$>) <$> declCons adt
      dct :: Maybe (ConTree Identifier ([Text] -> Text)) = (decRef adtEnv name <$>) <$> declCons adt
      constrs :: [(Identifier, Fields Identifier Text)] = maybe [] constructors mct
      constrNames :: [Text] = map (asT . fst) constrs
      -- e.g. ZM.ADT.Bit.K65149ce3b366
      -- mdlName = moduleName ns name adtRef
      -- e.g. ["ZM","ADT","Bit","K65149ce3b366"]
      mdlNameC :: [Text] = ns ++ [filename]
      -- \|True if it has more than one contructor and one constructor has the same name as the adt
      hasNameClash = length constrNames > 1 && (name `elem` constrNames)
      -- Prefix with '_' name clashing constructor
      fullConstrName cname =
        let n = asT cname
         in if hasNameClash && name == n then T.cons '_' n else n
      fullConstrNames :: [Text] = map fullConstrName constrNames
      -- fullConstrName cname = let name = asT cname in if hasNameClash then T.concat ["ZMC.",name] else name
      -- constructorsOpen = if hasNameClash then "namespace ZMC {" else ""
      -- constructorsClose = if hasNameClash then "}" else ""
      typeVars = typeVarsM adt
      adtTypeVars :: Text = typeVarsSeq " extends zm.ZM" adt
      -- sadt = substAbsADT (\ref -> adtFullName ns (declNameS adtEnv ref) ref) adt
      constrTypeVars :: Text = typeVarsSeq "" adt
      -- class Left <A extends zm.ZM,B extends zm.ZM> extends Either <A,B> {
      constrClass isSingleton superType _ct (cname, cf) =
        let nf = namedFieldsWith parName cf
         in -- cs = constructors ct
            T.unlines
              [ classHeader isSingleton superType (fullConstrName cname),
                -- , zmType (declNumParameters adt)
                constructor cname nf,
                -- toString cname nf,
                -- flat ct cname nf,
                "}"
              ]
      parName n = "p" ++ show n

      valueClass name valueType =
        T.unlines
          [ classHeader False "??" name,
            indentLines
              4
              [ T.concat [name, "(this.", jsType valueType, ") { }"],
                T.concat ["final ", jsType valueType, ";"],
                T.concat
                  ["//flatMaxSize() {return zm.EncoderState.sz", name, "(this.value);}"],
                T.concat
                  ["//flatEncode(st: zm.EncoderState) {st.zm", name, "(this.value);}"],
                "//toString(): string {return this.toStr(false)}",
                "//toStr(nested?:boolean): string {return this.value.toString();}",
                "//pretty(nested?:boolean): string {return this.toString(); }"
              ],
            "}"
          ]

      classHeader isSingleton superType constrName =
        let ext = if isSingleton then "zm.ZM" else superType <> constrTypeVars
         in T.unwords ["class", constrName, adtTypeVars, "extends", ext, "{"]

      typeId =
        let AbsRef (SHAKE128_48 v1 v2 v3 v4 v5 v6) = adtRef
         in pars (map h [v1, v2, v3, v4, v5, v6])
        where
          h v = T.concat ["0x", T.pack $ hex v]

      constructor _ [] = ""
      constructor cname nf =
        T.unlines
          [ -- final A p0;
            T.unlines $
              map
                (\(n, t) -> "final " <> tsType t <> " " <> n <> ";") -- T.pack $ parName (T.unpack n), ";"])
                nf,
            -- Left(this.p0);
            T.concat
              [ -- "const ",
                prettyT cname,
                "(",
                T.intercalate "," $
                  map (\(n, _) -> "this." <> n) nf,
                ");"
              ]
          ]

      toString cname nf =
        let ns = map fst nf
            complex s = if null ns then "" else s
            name = T.cons '"' $ T.snoc (prettyT cname) '"'
            asStr nm =
              T.concat
                [ "  ",
                  nm,
                  "(nested=false):string {return ",
                  complex "zm.nestedPars(nested,[",
                  name,
                  complex ",",
                  T.intercalate ","
                    . map (\n -> T.concat ["this.", n, ".", nm, "(true)"])
                    $ ns,
                  complex "].join(' '))",
                  "}\n"
                ]
         in T.concat
              [ "  toString():string {return this.toStr(false)}\n",
                asStr "toStr",
                asStr "pretty"
              ]

      flat ct cname nf =
        let ns = map fst nf
            Just (bpath, _) = constructorInfo cname ct
            maxSize =
              list ["0"] (map (\n -> T.concat ["this.", n, ".flatMaxSize()"])) ns
            flatEnc =
              T.concat $
                map ((\f -> T.concat ["st.", f, "();"]) . bool "zero" "one") bpath
                  ++ map (\n -> T.concat ["this.", n, ".flatEncode(st);"]) ns
         in T.unlines
              [ T.concat
                  [ "  flatMaxSize():number {return ",
                    T.intercalate "+" $
                      list [] ((: []) . T.pack . show . length) bpath
                        ++ maxSize,
                    ";}"
                  ],
                if T.null flatEnc
                  then "  flatEncode() {}"
                  else T.concat ["  flatEncode(st:zm.EncoderState) {", flatEnc, "}"]
              ]

      -- always import from quid2-core
      -- importLib as _ =
      --   T.concat
      --     [ "import * as ",
      --       as,
      --       " from '@quid2/core'"
      --     ]

      -- always import locally
      -- importLib as name = T.concat
      --   [ "import * as "
      --   , as
      --   , " from '"
      --   , T.concat $ replicate (length ns + 1) "../"
      --   , libFile name
      --   , "'"
      --   ]
      -- libFile = T.append "@quid2/core"

      -- THis can be either a local reference (in quid2), e.g.: "'../.." or an external reference to the quid2 lib
      -- importLib as name = T.concat
      --   [ "import * as "
      --   , as
      --   , " from '"
      --   , T.concat $ replicate (length ns + 1) "../"
      --   , libFile name
      --   , "'"
      --   ]
      numTypeParams = declNumParameters adt

      -- const $Bit : zm.zmFold<Bit> = function (f) {return f(___,[])}
      -- const $ByType : <A extends zm.Flat>(t1:zm.zmFold<A>) => zm.zmFold<ByType<A>> = function (t1) {return function (f) {return f(___,[t1(f)])}}
      zmType = zmFoldSig <> "$$" <> name <> zmType_
      zmFoldSig = "zm.ZMFold<" <> name <> constrTypeVars <> "> "
      zmType_
        | numTypeParams == 0 = "= <R>(zm.Folder<R> f) {return " <> funcCall "f" ["info_", "[]"] <> ";};"
        | otherwise =
            T.concat
              [ adtTypeVars,
                typeVars
                  pars
                  (\(n, v) -> "zm.ZMFold<" <> v <> "> " <> "t" <> showT (1 + n)),
                " {return <R>(zm.Folder<R> f) {return ", -- <R>
                funcCall "f" ["info_", arr (prepost 't' "(f)" numTypeParams)],
                ";}",
                -- " as ",zmFoldSig,
                ";}"
              ]

      typeUnflat edct =
        "zm.Decoder<" <> name <> constrTypeVars <> "> $" <> name <> adtTypeVars <> typeUnflat_ edct

      typeUnflat_ edct
        | numTypeParams == 0 = " = " <> simpleTypeUnflat edct
        | otherwise =
            T.concat
              [ typeVars pars (\(n, v) -> T.concat ["zm.Decoder<", v, "> ", "t", showT $ 1 + n]),
                " {return ",
                simpleTypeUnflat edct,
                "}"
              ]

      simpleTypeUnflat edct = "(zm.DecoderState st) {" <> unflatF_ edct <> "};"

      unflatF edct = T.unwords ["    return (st) {", unflatF_ edct, "}"]

      unflatF_ Nothing =
        T.concat ["return ", name, "(st.zm", name, "(decoders));"]
      unflatF_ (Just dct) =
        maybe
          ( T.unwords
              ["throw Error(\"", absName, "has no values and cannot be decoded\""]
          )
          dunflat
          dct

      -- lowerHead t = T.toLower T.head h : t

      -- unflat :: T.Text
      -- unflat = maybe (T.unwords ["throw Error(\"",absName,"has no values and cannot be decoded\""]) directUnflat dct

      -- directUnflat ct = T.unwords ["    return function(st) {",dunflat ct,"}"]

      -- BUG: fails for recursive defs
      -- preUnflat ct = T.unlines [decs ct
      --                           ,T.unwords ["    return function(st) {",unflat_ ct,"}"]]

      -- const decs = {"BLOB":[decoders[0],(Kf8844385a443.$Bytes)(flatDecoder)]};
      decs ct =
        T.unwords
          [ "const decs =",
            obj
              . map
                ( \(name, ts) ->
                    T.concat
                      [ "\"",
                        fullConstrName name,
                        "\":",
                        arr $ map (\t -> T.concat [decType t, "(zm.flatDecoder)"]) ts
                      ]
                )
              . filter (not . null . snd)
              . map (second fieldsTypes)
              . constructors
              $ ct,
            ";"
          ]

      dunflat (ConTree l r) =
        T.unwords ["if (st.zero()) {", dunflat l, "} else {", dunflat r, "}"]
      dunflat (Con name flds) =
        let ts = fieldsTypes flds
         in T.concat
              [ "return ",
                fullConstrName name,
                pars . map (\t -> T.concat [decType t, "(st)"]) $ ts,
                ";"
              ]

      unflat_ (ConTree l r) =
        T.unwords ["if (st.zero()) {", unflat_ l, ";} else {", unflat_ r, ";};"]
      unflat_ (Con name flds) =
        let arity = length (fieldsTypes flds)
         in T.concat
              [ if arity > 0
                  then T.concat ["const d=decs[\"", fullConstrName name, "\"]; "]
                  else "",
                "return ",
                fullConstrName name,
                pars $ prepost0 "d[" "](st)" arity
              ]

      -- otherwise = [pars $ prepost 't' ":zm.ZMFold" arity,"=> zm.ZMFold =",funcDef1 "" (prefix 't' arity) (funcDef1 "" ["f"] (funcCall "f" ["___",arr (prepost 't' "(f)" arity)]))]

      typeInfo =
        T.unlines
          [ "const info_ = zm.ZMTypeInfo(",
            T.pack $ show name <> ",",
            typeId <> ",",
            -- "  (decoders) {",
            -- indent 4 (unflatF edct),
            -- "  }",
            ");"
          ]

      remark =
        T.unlines ["/*", "ZM Type:", T.pack $ take 1000 $ prettyShow adt, "*/"]

      contentHeader ::
        Maybe (Maybe (ConTree Identifier ([T.Text] -> T.Text))) -> T.Text
      contentHeader edct =
        T.unlines
          [ remark,
            "import '../../zm.dart' as zm;",
            imports "./" adtEnv $ innerReferences adt,
            zmType,
            "",
            typeUnflat edct,
            "",
            typeInfo
          ]

      contentValue vt = T.unlines [contentHeader Nothing, valueClass name vt]

      contentNorm ::
        Maybe (ConTree Identifier T.Text) ->
        Maybe (Maybe (ConTree Identifier ([T.Text] -> T.Text))) ->
        T.Text
      contentNorm mct edct =
        T.unlines
          [ contentHeader edct,
            maybe
              -- No constructors, e.g. data Void
              (T.unwords ["class", name, "extends zm.ZM {}"])
              ( \ct ->
                  let cs = constructors ct
                      unionType =
                        T.unlines
                          [ T.unwords
                              [ "sealed class",
                                name,
                                adtTypeVars,
                                "extends zm.ZM {"
                              ],
                            "}"
                          ]
                   in T.unlines $
                        case cs of
                          [cons] -> [constrClass True name ct cons]
                          _ -> unionType : "" : map (constrClass False name ct) cs
              )
              mct
          ]
      -- contentPrim = T.unwords ["{", commaed (declNameT adt : fullConstrNames), "} from '../../core'"]
      -- contentPrim = T.concat ["* from '",libFile "core","/",declNameT adt,"'"]
      contentPrim =
        T.concat ["* from '", "../../lib/core", "/", declNameT adt, "'"]

      content = case primType flags absName of
        Just vt@(ValueType _) -> contentValue vt
        Just PrimType -> contentPrim
        Nothing -> contentNorm mct (Just dct)
   in dartModule mdlNameC content

-- libFile = T.append "./lib/"

-- import * as K306f1981b41c from '<dir>Bool/K306f1981b41c'
imports dir adtEnv =
  semis
    . map
      ( \ref ->
          T.unwords
            [ "import",
              T.concat ["'", dir, fileRef adtEnv ref, "'"],
              "as ",
              mdlRef adtEnv ref
            ]
      )

semis = T.unlines . map semi

semi = (`T.snoc` ';')

-- Return the corresponding Dart type
tsType :: (Pretty a) => Type a -> T.Text
tsType = pt . typeN
  where
    pt (TypeN f []) = pr f
    pt (TypeN f ps) = T.concat [pr f, sig (map pt ps)]
    pr = T.pack . prettyShow

-- decType :: Pretty a => Type a -> T.Text
decType = pt . typeN
  where
    -- pt (TypeN f []) = pr f
    pt (TypeN f ps) = f (map pt ps) -- T.concat [pr f, pars (map pt ps)]
    pr = T.pack . prettyShow

-- Type decoder reference
-- decRef _ _ (Var n) = \[] -> T.concat ["decoders[", showT $ n+1, "]"]
decRef _ _ (Var n) = \[] -> "t" <> showT (n + 1)
decRef env _ (Ext r) =
  \ds -> T.concat [tyRefWith "$" env r, params ds]
decRef _ self Rec = \ds -> T.concat ["$", self, params ds]

params [] = ""
params ds = par . commaed $ ds

{-
A                       -- variable
List<A>                 -- self reference
bool_k306f1981b41c.Bool -- external reference
-}

tsRef _ _ (Var n) = var n
tsRef env _ (Ext r) = tyRef env r -- T.concat [prettyT r, ".", declNameS env r]
tsRef _ self Rec = self

tyRef = tyRefWith ""

tyRefWith prefix adtEnv ref = mdlRef adtEnv ref <> "." <> prefix <> declNameS adtEnv ref

fileRef :: (Pretty a1, Ord a1, Show a1, Convertible a2 String) => M.Map a1 (ADT a2 consName ref) -> a1 -> Text
fileRef adtEnv ref = mdlRef adtEnv ref <> ".dart"

mdlRef adtEnv ref = let name = declNameS adtEnv ref in T.toLower (name <> "_" <> prettyT ref)

-- decRef _ _ (Var n) = T.concat["zm.zmConst(decoders[",showT n,"])"]
-- decRef env _ (Ext r) = T.concat [prettyT r, ".", declNameS env r]
-- decRef _ self Rec = T.cons '$' self

typeVars :: ADT name consName ref -> [T.Text]
typeVars adt = map (var . (\v -> v - 1)) [1 .. declNumParameters adt]

typeVarsSeq :: T.Text -> ADT name consName ref -> T.Text
typeVarsSeq post = list "" (sig . map (`T.append` post)) . typeVars

-- typeSigM f = list "" (sig . map f) . zip [0..] . typeVars

-- typeVarsM :: (Num a, Enum a) =>
--                ADT name consName ref -> ([b] -> String) -> ((a, T.Text) -> b) -> T.Text
typeVarsM adt lst f = list "" (lst . map f) . zip [0 ..] $ typeVars adt

funcAnon = funcDef1 ""

funcDef1 :: Text -> [Text] -> Text -> Text
funcDef1 name vs body = funcDef name vs $ T.unwords ["return", body]

funcDef name vs body = T.concat [" ", name, pars vs, " {", body, "}"]

funcCall name vs = T.concat [name, pars vs]

prefix :: (Show a, Num a, Enum a) => Char -> a -> [T.Text]
prefix pre = prepost pre ""

-- inx f vs = map (\v -> f T.concat[pre `T.cons` T.pack (show n),post]) vs

prepost ::
  (Show a, Num a, Enum a) =>
  Char ->
  T.Text ->
  a ->
  [T.Text]
prepost pre post n =
  map (\n -> T.concat [pre `T.cons` T.pack (show n), post]) [1 .. n]

prepost0 ::
  (Show a, Num a, Enum a) =>
  T.Text ->
  T.Text ->
  a ->
  [T.Text]
prepost0 pre post n =
  map (\n -> T.concat [pre `T.append` T.pack (show n), post]) [0 .. n - 1]

indent :: Int -> T.Text -> T.Text
indent n = T.append (T.replicate n " ")

-- Is this still required?
-- library :: M.Map FilePath T.Text
-- library =
--   asTextLibrary
--     $ $(embedFiles "quid2-ts" (anyMatching ["core/[^.]+\\.ts","core\\.ts","api\\.ts"]))

{-
abstract class Maybe_ <A extends zm.Flat> {
  static zid : [0xda,0x68,0x36,0x77,0x8f,0xd4];

  static decoder : function (decoders:any[]) {
    return function(st:any) {
            if (st.bit()) {return Just(decoders[0](st));}
            else return Nothing();
    }
  };
}
-}

indentLines :: Int -> [Text] -> Text
indentLines n = T.unlines . map (indent n)
