import Language.Haskell.HLint3 (hlint)
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
  ideas <- hlint
    [ "."
    , "--ignore=Redundant do"
    , "--ignore=Use camelCase"
    , "--ignore=Redundant $"
    , "--ignore=Use &&"
    , "--ignore=Use &&&"
    , "--ignore=Use const"
    , "--ignore=Use >=>"
    , "--ignore=Use ."
    , "--ignore=Use unless"
    , "--ignore=Use if"
    , "--ignore=Use fewer imports"
    , "--ignore=Use list comprehension"
    , "--ignore=Unnecessary hiding" -- Interferes with cross-version compatibility
    , "--ignore=Use <$>"
    , "--ignore=Reduce duplication" --TODO: Re-enable this test
    , "--ignore=Use list comprehension"
    , "--cpp-define=USE_TEMPLATE_HASKELL"
    ]
  if null ideas then exitSuccess else exitFailure
