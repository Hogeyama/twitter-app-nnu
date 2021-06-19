   let types = ./types.dhall
in let Gamers    = types.Gamers
in let Member    = types.MkMember
in let Group     = types.MkGroup Gamers
in let members =
   [
     -- Member Gamers "雪汝"         "setsuna2434"    1023138752850321408
     Member Gamers "椎名唯華"     "yuika_siina"    1022844567735824384
   , Member Gamers "魔界ノりりむ" "makaino_ririmu" 1022782812775100416
   , Member Gamers "笹木咲"       "saku_sasaki"    1012211447160455170
   , Member Gamers "本間ひまわり" "honmahimawari"  1011167857596493824
   -- , Member Gamers "闇夜乃モルル" "_rnrrdark"      1000747836365942784
   , Member Gamers "赤羽葉子"     "Youko_Akabane"  988489581367513088
   , Member Gamers "叶"           "Kanae_2434"     988101299106267138
   , Member Gamers "葛葉"         "Vamp_Kuzu"      965760241169088512
   ]
in let listId = 1086003363018428416
in Group listId members
