import Html

main = putStrLn (render_ myhtml)

myhtml :: Html
myhtml = html_ "My page title" (append_ (h1_ "This is the test heading") (p_ "Followed by a test paragraph"))
