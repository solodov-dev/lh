module Html.Internal where 
import GHC.Natural (Natural)

newtype Html = Html String
newtype DomNode = DomNode String
type Title = String


instance Semigroup DomNode where
  (<>) c1 c2 =
    DomNode (getInnerString c1 <> getInnerString c2)

instance Monoid DomNode where
  mempty = DomNode ""

el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

html_ :: Title -> DomNode -> Html
html_ title (DomNode content) = Html $ el "html" (el "head" (el "title" . escape $ title)) <> el "body" content

p_ :: String -> DomNode
p_ = DomNode . el "p" . escape

code_ :: String -> DomNode
code_ = DomNode . el "pre" . escape

h_ :: Natural -> String -> DomNode
h_ n = DomNode . el ("h" <> show n) . escape

list_ :: String -> [DomNode] -> DomNode
list_ listType = DomNode . el listType . concatMap (el "li" . getInnerString)

ol_ = list_ "ol"

ul_ = list_ "ul"

render_ :: Html -> String
render_ (Html html) = html

getInnerString :: DomNode -> String
getInnerString (DomNode s) = s

escape :: String -> String
escape =
  let escapeChar c =
        case c of
          '<' -> "&lt;"
          '>' -> "&gt;"
          '&' -> "&amp;"
          '"' -> "&quot;"
          '\'' -> "&#39;"
          _ -> [c]
   in concatMap escapeChar

empty_ :: DomNode
empty_ = DomNode ""
