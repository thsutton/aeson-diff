import Build_doctests (flags, pkgs, module_sources)
import Test.DocTest

main :: IO ()
main = doctest $ flags ++ pkgs ++ module_sources
