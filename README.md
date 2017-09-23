# reader

optparse-applicative-reader is a haskell library to provide additional
readers for the optparse-applicative.

## Simple example

````haskell
import Control.Applicative (many)
import Control.Monad (forM_)
import Options.Applicative
import Options.Applicative.Builder.Reader

opts :: ParserInfo [NamedValue]
opts = info (many $ argument namedValue idm) idm

main :: IO ()
main = execParser opts >>= \nvs -> do
  forM_ nvs $ \nv ->
    putStrLn $ "name: " ++ fst nv ++ "  value: " ++ snd nv
````

```
$ example-exe departure="New York" arrival="Hong Kong"
name: departure  value: New York
name: arrival  value: Hong Kong
$
```