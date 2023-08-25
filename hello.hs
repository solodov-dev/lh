import Html

main = putStrLn (render_ myhtml)

myhtml :: Html
myhtml = html_ "My page title" (h_ 1 "This is the test heading" <> p_ "Followed by a test paragraph")
