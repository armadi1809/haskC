module Compile (compile) where

import qualified Codegen as Cg
import qualified Emit as Em
import qualified Lexer as Lex
import qualified Parser as Pars

compile :: FilePath -> IO ()
compile filename = do
  let srcFile = filename ++ ".i"
      destFile = filename ++ ".s"
  src <- readFile srcFile
  let toksE = Lex.lexer src
  case toksE of
    Left err -> putStrLn err
    Right toks -> case Pars.parse Pars.parseProgram toks of
      Nothing -> putStrLn "Error Parsing the Program"
      Just ast -> Em.emit destFile $ Cg.gen ast