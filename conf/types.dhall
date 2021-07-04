   let GroupLabel =
      < Nijisanji
      | Gamers
      | SEEDs
      | Since2019
      | Other : {label: Text}
      >
in let Member =
      { exGroup : GroupLabel
      , memberName : Text
      , screenName : Text
      , userId : Natural
      }
in let Group =
      { groupLabel : GroupLabel
      , listId : Natural
      , members : List Member
      }
in { Nijisanji = GroupLabel.Nijisanji
   , Gamers    = GroupLabel.Gamers
   , SEEDs     = GroupLabel.SEEDs
   , Since2019 = GroupLabel.Since2019
   , MkMember =
      λ(group: GroupLabel)→λ(name: Text)→λ(screenName: Text)→λ(userId: Natural)→
        { exGroup = group
        , memberName = name
        , screenName = screenName
        , userId = userId
        }
   , MkGroup =
      λ(group: GroupLabel)→λ(listId: Natural)→λ(members: List Member)→
        { groupLabel = group
        , listId = listId
        , members = members
        }
   }
