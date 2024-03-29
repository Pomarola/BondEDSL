module Main (main) where

import Control.Exception ( catch, IOException )
import Control.Monad.Except ( when, MonadError(throwError), MonadTrans(lift), MonadIO(liftIO) )
import Data.Char ( isSpace )
import Data.List ( nub, intercalate, isPrefixOf, isSuffixOf )
import Prelude hiding ( print, exp )
import System.Console.Haskeline ( getInputLine, defaultSettings, runInputT, InputT )
import System.IO ( hPutStrLn, stderr, hPrint )
import Control.Monad.Catch (MonadMask)
import System.Exit ( exitWith, ExitCode(ExitFailure) )

import           Parse
import           Sugar
import           Eval
import           State
import           MonadBnd
import           Errors

main :: IO ()
main = do
  runOrFail (runInputT defaultSettings repl)

iname, iprompt :: String
iname = "Bond Calculator"
iprompt = "BC> "

runOrFail :: Bnd a -> IO a
runOrFail m = do
  r <- runBnd m
  case r of
    Left err -> do
      liftIO $ hPrint stderr err
      exitWith (ExitFailure 1)
    Right v -> return v

repl :: (MonadBnd m, MonadMask m) => InputT m ()
repl = do
       liftIO $ putStrLn
         (  "Entorno interactivo de "
         ++ iname
         ++ ".\n"
         ++ "Escriba :? para recibir ayuda.")
       loop
  where loop = do
           minput <- getInputLine iprompt
           case minput of
               Nothing -> return ()
               Just "" -> loop
               Just x -> do
                       c <- liftIO $ interpretCommand x
                       b <- lift $ catchErrors $ handleCommand c
                       maybe loop (`when` loop) b

data Command = Compile CompileForm
              | Browse
              | Quit
              | Help
              | Noop

data CompileForm = CompileInteractive  String
                  | CompileFile         String

-- | Parser simple de comando interactivos
interpretCommand :: String -> IO Command
interpretCommand x
  =  if ":" `isPrefixOf` x then
       do  let  (cmd,t')  =  break isSpace x
                t         =  dropWhile isSpace t'
           --  find matching commands
           let  matching  =  filter (\ (Cmd cs _ _ _) -> any (isPrefixOf cmd) cs) commands
           case matching of
             []  ->  do  putStrLn ("Comando desconocido `" ++ cmd ++ "'. Escriba :? para recibir ayuda.")
                         return Noop
             [Cmd _ _ f _]
                 ->  do  return (f t)
             _   ->  do  putStrLn ("Comando ambigüo, podría ser " ++
                                   intercalate ", " ([ head cs | Cmd cs _ _ _ <- matching ]) ++ ".")
                         return Noop

     else
       return (Compile (CompileInteractive x))

handleCommand ::  MonadBnd m => Command -> m Bool
handleCommand cmd = case cmd of
       Quit   ->  return False
       Noop   ->  return True
       Help   ->  printBnd (helpTxt commands) >> return True
       Browse ->  do
                      e <- getEnv
                      printBnd "Entorno de Bonos:"
                      printBnd (unlines (reverse (nub (map show e))))
                      p <- getPortfolios
                      printBnd "Entorno de Portfolios:"
                      printBnd (unlines (reverse (nub (map show p))))
                      return True
       Compile c ->
                  do  case c of
                          CompileInteractive e -> compilePhrase e
                          CompileFile f        -> compileFile f
                      return True

data InteractiveCommand = Cmd [String] String (String -> Command) String

commands :: [InteractiveCommand]
commands =
  [ Cmd [":browse"] "" (const Browse) "Ver los nombres en scope"
  , Cmd [":load"]
        "<file>"
        (Compile . CompileFile)
        "Cargar un programa desde un archivo"
  -- , Cmd [":print"] "<exp>" Print "Imprime un término y sus ASTs"
  , Cmd [":quit"]       ""       (const Quit) "Salir del intérprete"
  , Cmd [":help", ":?"] ""       (const Help) "Mostrar esta lista de comandos"
  ]

helpTxt :: [InteractiveCommand] -> String
helpTxt cs
  =  "Lista de comandos:  Cualquier comando puede ser abreviado a :c donde\n" ++
     "c es el primer caracter del nombre completo.\n\n" ++
     "<expr>                  Evaluar la expresión\n" ++
     "def <var> = <bond>      Definir un bono\n" ++
     "portfolio <var> = [<int> <var>]      Definir un portafolio\n" ++
     unlines (map (\ (Cmd c a _ d) ->
                   let  ct = intercalate ", " (map (++ if null a then "" else " " ++ a) c)
                   in   ct ++ replicate ((24 - length ct) `max` 2) ' ' ++ d) cs)

checkExtension:: String -> Bool
checkExtension f = ".bnd" `isSuffixOf` f

loadFile ::  MonadBnd m => String -> m [DefOrExp]
loadFile filename = do
      x <- liftIO $ catch (readFile filename)
                (\e -> do let err = show (e :: IOException)
                          hPutStrLn stderr ("No se pudo abrir el archivo " ++ filename ++ ": " ++ err)
                          return "")
      parseIO defs_parse x

compileFile ::  MonadBnd m => FilePath -> m ()
compileFile f = let filename = reverse (dropWhile isSpace (reverse f)) in
    if not (checkExtension filename)
      then failBnd ("El archivo " ++ filename ++ " no tiene extension .bnd")
    else do
      printBnd ("Abriendo "++f++"...")
      defs <- loadFile filename
      mapM_ handleDefOrExp defs

compilePhrase ::  MonadBnd m => String -> m ()
compilePhrase x = do
    x' <- parseIO def_or_exp_parse x
    handleDefOrExp x'

parseIO :: MonadBnd m => (String -> ParseResult a) -> String -> m a
parseIO p x = case p x of
  Failed e -> throwError (Error e)
  Ok r -> return r

handleDefOrExp :: MonadBnd m => DefOrExp -> m ()
handleDefOrExp (Def v sb) = do
  b <- convert sb
  case b of
    Just b' -> addDef (v, b')
    Nothing -> failBnd ("No se pudo convertir " ++ v ++ " a bono.")

handleDefOrExp (Portfolio v ps) = do
  addPortfolio (v, ps)

handleDefOrExp (Eval exp) = do
  b <- eval exp
  setDate oldDate
  unsetPrice
  if b then return () else failBnd "La evaluacion de la expresion fallo."