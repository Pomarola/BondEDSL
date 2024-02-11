module Main where

import           Control.Exception              ( catch
                                                , IOException
                                                )
import           Control.Monad.Except
import           Data.Char
import           Data.List
import           Data.Maybe
import           Prelude                 hiding ( print, exp )
import           System.Console.Haskeline
import qualified Control.Monad.Catch           as MC
import           System.Environment
import           System.IO               hiding ( print )

import           Common
import           Parse
import           Sugar
import           Eval
import           State
import           MonadBnd

---------------------
--- Interpreter
---------------------

main :: IO ()
main = runOrFail (runInputT defaultSettings main')

main' :: InputT IO ()
main' = do
  args <- lift getArgs
  repl args
  -- readevalprint args

iname, iprompt :: String
iname = "Bond Calculator"
iprompt = "CC> "

-- ioExceptionCatcher :: IOException -> IO (Maybe a)
-- ioExceptionCatcher _ = return Nothing

runOrFail :: Bnd a -> IO a
runOrFail m = do
  r <- runBnd m
  case r of
    Left err -> do
      liftIO $ hPrint stderr err
      exitWith (ExitFailure 1)
    Right v -> return v

repl :: (MonadFD4 m, MonadMask m) => [FilePath] -> InputT m ()
repl args = do
       lift $ setInter True
       lift $ catchErrors $ mapM_ compileFile args
       s <- lift get
       when (inter s) $ liftIO $ putStrLn
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

handleCommand ::  MonadFD4 m => Command -> m Bool
handleCommand cmd = case cmd of
       Quit   ->  return False
       Noop   ->  return True
       Help   ->  printBnd (helpTxt commands) >> return True
       Browse ->  do  printBnd (unlines (reverse (nub (map show getEnv))))
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
     "<expr>                  evaluar la expresión\n" ++
     "let <var> = <expr>      definir una variable\n" ++
     unlines (map (\ (Cmd c a _ d) ->
                   let  ct = intercalate ", " (map (++ if null a then "" else " " ++ a) c)
                   in   ct ++ replicate ((24 - length ct) `max` 2) ' ' ++ d) cs)

-- checkExtension:: String -> Bool
-- checkExtension = ".cc" `isSuffixOf` reverse (dropWhile isSpace (reverse s))

-- compileFiles :: MonadBnd m => [String] -> InputT m ()
-- compileFiles xs s =
--   foldM (\s x -> compileFile (s { inter = False }) x) s xs

loadFile ::  MonadFD4 m => FilePath -> m [DefOrExp]
loadFile f = do
    let filename = reverse(dropWhile isSpace (reverse f))
    x <- liftIO $ catch (readFile filename)
               (\e -> do let err = show (e :: IOException)
                         hPutStrLn stderr ("No se pudo abrir el archivo " ++ filename ++ ": " ++ err)
                         return "")
    setLastFile filename
    parseIO filename defs_parse x

compileFile ::  MonadFD4 m => FilePath -> m ()
compileFile f = do
    i <- getInter
    setInter False
    when i $ printFD4 ("Abriendo "++f++"...")
    defs <- loadFile f
    foldM handleDefOrExp defs
    setInter i

compilePhrase ::  MonadFD4 m => String -> m ()
compilePhrase x = do
    x' <- parseIO "<interactive>" def_or_exp_parse x
    handleDefOrExp x'

parseIO :: MonadBnd m => String -> (String -> ParseResult a) -> String -> InputT m a
parseIO f p x = lift $ case p x of
  Failed e -> throwError (Error e)
  Ok r -> return r

handleDefOrExp :: MonadBnd m => DefOrExp -> m ()
handleDefOrExp (Def v sb) = do
  c <- convert sb
  -- case c of
  --   Just c' -> return (state { env = (v, c') : env state })
  --   Nothing -> do
  --     lift $ putStrLn ("No se pudo convertir " ++ v ++ " a bono.")
  --     return state

handleDefOrExp (Eval exp) = do
  c <- eval exp
  -- return state