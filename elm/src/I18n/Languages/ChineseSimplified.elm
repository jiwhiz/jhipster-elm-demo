module I18n.Languages.ChineseSimplified exposing (translate)

import I18n.Phrases exposing(Phrase(..))

translate : Phrase -> String
translate phrase =
    case phrase of
        HomeTitle ->
            "欢迎, Java Hipster!"

        HomeSubtitle ->
            "这里是首页"

        HomeLoggedin username ->
            "您目前是以 [" ++ username ++ "] 账号登录."

        HomeQuestion ->
            "如果您有任何有关 JHipster 的问题, 可以查阅下列资源:"

        HomeLike ->
            "如果您喜欢 JHipster, 请记得给我们加星在"