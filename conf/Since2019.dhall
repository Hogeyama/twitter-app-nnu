   let types = ./types.dhall
in let Since2019 = types.Since2019
in let Member    = types.MkMember
in let Group     = types.MkGroup Since2019
in let members =
   [
   -- Member Since2019 "童田明治"               "warabeda_meiji"  1082065005061652480
   -- , Member Since2019 "久遠千歳"               "kudou_chitose"   1081927064033185794
   , Member Since2019 "夢月ロア"               "yuzuki_roa"      1085498064645705728
   , Member Since2019 "郡道美玲"               "g9v9g_mirei"     1085375212575571968
   , Member Since2019 "語部紡"                 "KataribeTsumugu" 1088046328423141380
   , Member Since2019 "瀬戸美夜子"             "seto_miyako"     1088029978761977856
   , Member Since2019 "小野町春香"             "onomachi_haruka" 1086415881154875392
   -- , Member Since2019 "御伽原江良"             "OtogibaraEra"    1088023486583304192
   , Member Since2019 "戌亥とこ"               "inui_toko"       1107557844855844864
   , Member Since2019 "アンジュ・カトリーナ"   "Ange_Katrina_"   1099996270947528704
   , Member Since2019 "リゼ・ヘルエスタ"       "Lize_Helesta"    1107935587271467008
   , Member Since2019 "愛園愛美"               "manami_aizono"   1107868757156745216
   , Member Since2019 "三枝明那"               "333akina"        1110711077468168192
   , Member Since2019 "鈴原るる"               "lulu_suzuhara"   1107968804447903744
   , Member Since2019 "雪城眞尋"               "MahiroYukishiro" 1120529071585107968
   , Member Since2019 "エクス・アルビオ"       "Ex_Albio"        1125700985660198912
   , Member Since2019 "レヴィ・エリファ"       "Levi_E_2434"     1126062450107768832
   , Member Since2019 "葉山舞鈴"               "Hayama_Marin"    1140588169491935233
   , Member Since2019 "ニュイ・ソシエール"     "Nui_Sociere"     1140590504158957568
   , Member Since2019 "葉加瀬冬雪"             "Hakase_Fuyuki"   1144540365854167041
   , Member Since2019 "加賀美ハヤト"           "H_KAGAMI2434"    1141971886688989184
   , Member Since2019 "夜見れな"               "rena_yorumi"     1144541744639250432
   , Member Since2019 "黛灰"                   "mayuzumi_X"      1144142348873392129
   , Member Since2019 "アルス･アルマル"        "ars_almal"       1144129307637239809
   , Member Since2019 "相羽ういは"             "AibaUiha"        1143824438040748032
   , Member Since2019 "エリー・コニファー"     "Eli_Conifer"     1158352011990990850
   , Member Since2019 "天宮こころ"             "amamiya_kokoro"  1146300489832837125
   , Member Since2019 "ラトナ・プティ"         "ratna_petit"     1144558351788859393
   , Member Since2019 "早瀬走"                 "SouHayase"       1173907942799601664
   , Member Since2019 "健屋花那"               "sukosuko_sukoya" 1146679484575170560
   , Member Since2019 "シェリン・バーガンディ" "ShellinBurgundy" 1163457518439284738
   , Member Since2019 "星川サラ"               "Sara_Hoshikawa"  1184071904245583874
   , Member Since2019 "山神カルタ"             "Karuta_Yamagami" 1165929239436120064
   , Member Since2019 "フミ"                   "FumiVirtual"     1162577611228233729
   , Member Since2019 "えま★おうがすと"        "emma_august_"    1162592961223245826
   , Member Since2019 "ルイス・キャミー"       "Luis_Cammy"      1164061334763274241
   , Member Since2019 "魔使マオ"               "matsukai_mao"    1186939016244940800
   , Member Since2019 "グウェル・オス・ガール" "Gwelu_os_gar"    1184381092041646080
   , Member Since2019 "不破湊"                 "Fuwa_Minato"     1184354588213698561
   , Member Since2019 "白雪巴"                 "Tomoe_Shirayuki" 1198882148389773313
   , Member Since2019 "来栖夏芽"               "kurusu72me"      1208331322696949760
   , Member Since2019 "ましろ"                 "mashiro2434"     1208326229457133568
   , Member Since2019 "奈羅花"                 "Naraka_2434"     1208326128596766720
   , Member Since2019 "フレン・E・ルスタリオ"  "furen_2434"      1221657763954057217
   , Member Since2019 "メリッサ・キンレンカ"   "melissa_2434"    1221661362935328769
   , Member Since2019 "イブラヒム"             "honmono_ibrahim" 1221660522971463680
   , Member Since2019 "弦月藤士郎"             "1O46V"           1244254367806410753
   , Member Since2019 "甲斐田晴"               "Kaida_Haru"      1244433641251336192
   , Member Since2019 "長尾景"                 "kei_nagao2434"   1244548255913930761
   , Member Since2019 "空星きらめ"             "kirame_2434"     1276465818780356608
   -- , Member Since2019 "金魚坂めいろ"           "meiro_oO"        1276919889823842305
   , Member Since2019 "朝日南アカネ"           "Akane_Asahina__" 1290267681887621123
   , Member Since2019 "周央サンゴ"             "Suo_Sango"       1289807807558987777
   , Member Since2019 "東堂コハク"             "kohaku_todo"     1288413439816110081
   , Member Since2019 "北小路ヒスイ"           "Hisui_Kitakoji"  1289831706573250561
   , Member Since2019 "西園チグサ"             "Chigusa_24zono"  1289884632637349888
   , Member Since2019 "アクシア・クローネ"     "AXIA_96NE"       1414413931955638277
   , Member Since2019 "ローレン・イロアス"     "Lauren_iroas"    1414416987703300100
   , Member Since2019 "レオス・ヴィンセント"   "Leos_Vincent"    1409367400709984260
   , Member Since2019 "オリバー・エバンス"     "oliverD_23"      1414419155881627649
   , Member Since2019 "レイン・パターソン"     "Lain_Paterson"   1415634931418492935
   , Member Since2019 "天ヶ瀬むゆ"             "Muyu_Amagase"    1499215310834237444
   , Member Since2019 "先斗寧"                 "ponto_nei"       1498892400110243846
   , Member Since2019 "海妹四葉"               "Yotsuha_Umise"   1498921872326807553
   , Member Since2019 "壱百満天原サロメ"       "1000000lome"     1526881736214597632
   -- サブアカウント
   -- , Member Since2019 "ギバさぶ郎"             "GB___2"          1250653534221033473
   ]
in let listId = 1086023933881712640
in Group listId members
