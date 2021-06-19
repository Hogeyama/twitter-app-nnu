   let types = ./types.dhall
in let Nijisanji = types.Nijisanji
in let Member    = types.MkMember
in let Group     = types.MkGroup Nijisanji
in let members =
  [ -- 一期生
    Member Nijisanji "月ノ美兎"      "MitoTsukino"    958737597336928256
  , Member Nijisanji "勇気ちひろ"    "Chihiro_yuki23" 958767484902957056
  , Member Nijisanji "える"          "Elu_World"      958726740108484608
  , Member Nijisanji "樋口楓"        "HiguchiKaede"   958646957190217728
  , Member Nijisanji "静凛"          "ShizuRin23"     958629229330968580
  , Member Nijisanji "渋谷ハジメ"    "sibuya_hajime"  958695135008628737
  , Member Nijisanji "鈴谷アキ"      "aki_suzuya"     958675678689243137
  , Member Nijisanji "モイラ"        "Moiramoimoimoi" 958632495196459009
    -- 二期生
  , Member Nijisanji "剣持刀也"      "rei_Toya_rei"   970692564096499712
  , Member Nijisanji "伏見ガク"      "gaku_fushimi"   970645330956963840
  , Member Nijisanji "ギルザレンⅢ世" "Gilzaren_III"   971032696393789440
  , Member Nijisanji "文野環"        "nekokan_chu"    971925378913611776
  , Member Nijisanji "宇志海いちご"  "ushimi_ichigo"  971316705363464192
  , Member Nijisanji "夕陽リリ"      "Yuuhi_Riri"     970645643965317126
  , Member Nijisanji "鈴鹿詩子"      "suzukautako"    970940146680868864
  , Member Nijisanji "物述有栖"      "AliceMononobe"  970660632834920449
  , Member Nijisanji "家長むぎ"      "ienaga_mugi23"  970618643120664576
  , Member Nijisanji "森中花咲"      "KazakiMorinaka" 973784758688927745
  -- サブアカウント
  , Member Nijisanji "勇気ちひろ2nd" "chihiro_2434"   1252111251741630464
  ]
in let listId = 1086003364310208512
in Group listId members
