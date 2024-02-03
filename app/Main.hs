module Main where

import           Control.Exception              ( catch
                                                , IOException
                                                )
import           Control.Monad.Except
import           Data.Char
import           Data.List
import           Data.Maybe
import           Prelude                 hiding ( print )
import           System.Console.Haskeline
import qualified Control.Monad.Catch           as MC
import           System.Environment
import           System.IO               hiding ( print )

import           Common
import           Parse
import           Sugar
import           Eval

---------------------
--- Interpreter
---------------------

main :: IO ()
main = runInputT defaultSettings main'

main' :: InputT IO ()
main' = do
  args <- lift getArgs
  readevalprint args (S True [])

iname, iprompt :: String
iname = "Bond Calculator"
iprompt = "CC> "

ioExceptionCatcher :: IOException -> IO (Maybe a)
ioExceptionCatcher _ = return Nothing

data State = S
  { inter :: Bool -- True, si estamos en modo interactivo.
    , env :: Env  -- Entorno con variables globales y su valor
  }

--  read-eval-print loop
readevalprint :: [String] -> State -> InputT IO ()
readevalprint args state@(S inter env) =
  let rec st = do
        mx <- MC.catch
          (if inter then getInputLine iprompt else lift $ fmap Just getLine)
          (lift . ioExceptionCatcher)
        case mx of
          Nothing -> return ()
          Just "" -> rec st
          Just x  -> do
            c   <- interpretCommand x
            st' <- handleCommand st c
            maybe (return ()) rec st'
  in  do
        -- state' <- compileFiles args state -- ver si va
        when inter $ lift $ putStrLn
          (  "Intérprete de "
          ++ iname
          ++ ".\n"
          ++ "Escriba :? para recibir ayuda."
          )
        --  enter loop
        rec state { inter = True } -- state' ver si va

data Command = Compile CompileForm
              -- | Print String
              -- | Recompile
              | Browse
              | Quit
              | Help
              | Noop
              -- | FindType String

data CompileForm = CompileInteractive  String
                  | CompileFile         String

interpretCommand :: String -> InputT IO Command
interpretCommand x = lift $ if isPrefixOf ":" x
  then do
    let (cmd, t') = break isSpace x
    let t         = dropWhile isSpace t'
    --  find matching commands
    let matching = filter (\(Cmd cs _ _ _) -> any (isPrefixOf cmd) cs) commands
    case matching of
      [] -> do
        putStrLn
          ("Comando desconocido `" ++ cmd ++ "'. Escriba :? para recibir ayuda."
          )
        return Noop
      [Cmd _ _ f _] -> do
        return (f t)
      _ -> do
        putStrLn
          (  "Comando ambigüo, podría ser "
          ++ concat (intersperse ", " [ head cs | Cmd cs _ _ _ <- matching ])
          ++ "."
          )
        return Noop
  else return (Compile (CompileInteractive x))

handleCommand :: State -> Command -> InputT IO (Maybe State)
handleCommand state@(S inter env) cmd = case cmd of
  Quit   -> lift $ when (not inter) (putStrLn "!@#$^&*") >> return Nothing
  Noop   -> return (Just state)
  Help   -> lift $ putStr (helpTxt commands) >> return (Just state)
  Browse -> lift $ do
    putStr (unlines (reverse (nub (map show env))))
    return (Just state)
  Compile c -> do
    state' <- case c of
      CompileInteractive s -> compilePhrase state s
      CompileFile        f -> compileFile state f -- check extension aca
    return (Just state')
  -- Print s ->
  --   let s' = reverse (dropWhile isSpace (reverse (dropWhile isSpace s)))
  --   in  printPhrase s' >> return (Just state)

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
helpTxt cs =
  "Lista de comandos:  Cualquier comando puede ser abreviado a :c donde\n"
    ++ "c es el primer caracter del nombre completo.\n\n"
    ++ "<expr>                  evaluar la expresión\n"
    ++ "def <var> = <expr>      definir una variable\n"
    ++ unlines
         (map
           (\(Cmd c a _ d) ->
             let
               ct =
                 concat
                   (intersperse ", "
                                (map (++ if null a then "" else " " ++ a) c)
                   )
             in  ct ++ replicate ((24 - length ct) `max` 2) ' ' ++ d
           )
           cs
         )

-- checkExtension:: String -> Bool
-- checkExtension = ".cc" `isSuffixOf` reverse (dropWhile isSpace (reverse s))

compileFiles :: [String] -> State -> InputT IO State
compileFiles xs s =
  foldM (\s x -> compileFile (s { inter = False }) x) s xs

compileFile :: State -> String -> InputT IO State
compileFile state f = do
  lift $ putStrLn ("Abriendo " ++ f ++ "...")
  let f' = reverse (dropWhile isSpace (reverse f))
  x <- lift $ Control.Exception.catch
    (readFile f')
    (\e -> do
      let err = show (e :: IOException)
      hPutStr stderr
              ("No se pudo abrir el archivo " ++ f' ++ ": " ++ err ++ "\n")
      return ""
    )
  defs <- parseIO f' (defs_parse) x
  maybe (return state) (foldM handleDefOrExp state) defs


compilePhrase :: State -> String -> InputT IO State
compilePhrase state x = do
  x' <- parseIO "<interactive>" def_or_exp_parse x
  maybe (return state) (handleDefOrExp state) x'

-- printPhrase :: String -> InputT IO ()
-- printPhrase x = do
--   x' <- parseIO "<interactive>" def_or_exp_parse x
--   maybe (return ()) (printStmt . fmap (\y -> (y, conversion y))) x'

-- printStmt :: Stmt (LamTerm, Term) -> InputT IO ()
-- printStmt stmt = lift $ do
--   let outtext = case stmt of
--         Def x (_, e) -> "def " ++ x ++ " = " ++ render (printTerm e)
--         Eval (d, e) ->
--           "LamTerm AST:\n"
--             ++ show d
--             ++ "\n\nTerm AST:\n"
--             ++ show e
--             ++ "\n\nSe muestra como:\n"
--             ++ render (printTerm e)
--   putStrLn outtext

parseIO :: String -> (String -> ParseResult a) -> String -> InputT IO (Maybe a)
parseIO f p x = lift $ case p x of
  Failed e -> do
    putStrLn (f ++ ": " ++ e)
    return Nothing
  Ok r -> return (Just r)

handleDefOrExp :: State -> DefOrExp -> InputT IO State
handleDefOrExp state (Def v sc) = do
  -- c' <- eval c
  -- return (state { env = (v, c') : env state })
  c <- convert (env state) sc
  case c of
    Just c' -> return (state { env = (v, c') : env state })
    Nothing -> do
      lift $ putStrLn ("No se pudo convertir " ++ v ++ " a contrato.")
      return state

handleDefOrExp state (Eval exp) = do
  c <- eval (env state) exp
  return state