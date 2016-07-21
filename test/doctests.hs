import Test.DocTest

main :: IO ()
main = doctest ["-ilib", "lib"]
