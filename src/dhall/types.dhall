   let Group  =
      < Nijisanji
      | Gamers
      | SEEDs
      | Since2019
      | Other : {label: Text}
      >
in { Nijisanji = Group.Nijisanji
   , Gamers    = Group.Gamers
   , SEEDs     = Group.SEEDs
   , Since2019 = Group.Since2019
   , Member =
      λ(group: Group)→λ(name: Text)→λ(screenName: Text)→λ(userId: Natural)→
        { exGroup = group
        , liverName = name
        , screenName = screenName
        , userId = userId
        }
   }
