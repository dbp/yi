{-# LANGUAGE NoMonomorphismRestriction, NamedFieldPuns, DoAndIfThenElse #-}
--import Yi

-- Preamble
import Yi.Prelude hiding ((%=))
import Prelude ()

import Yi.Config.Simple
import Yi.FuzzyOpen
import Yi.Command(buildRun)


import System.Directory
import System.FilePath

main :: IO ()
main = configMain defaultEmacsConfig setup

setup :: ConfigM ()
setup = do
  setFrontendPreferences ["cocoa", "vte", "vty"]
  fontSize %= Just 9

  globalBindKeys globalBindings
  evaluator %= publishedActionsEvaluator
  publishAction "createDirectory" yiCreateDirectory


globalBindings = choice
   [
     ctrlCh 'p' ?>>! fuzzyOpen,
     (metaCh 'g' ?>>! gotoLn),
     super (spec KRight) ?>>! (maybeMoveB Line Forward)
   ]

yiCreateDirectory :: YiM ()
yiCreateDirectory = do
    BufferFileInfo{bufInfoFileName} <- withEditor $ withBuffer0 bufInfoB
    let dir = takeDirectory bufInfoFileName
    exists <- io $ doesDirectoryExist dir
    if not exists
    then do
            io $ createDirectoryIfMissing True dir
            withEditor $ printMsg $ "Created directory '" ++ dir ++ "'."
    else withEditor $ printMsg $ "Directory already exists!"

compileLatex = do
    mfilename <- withEditor $ withBuffer0 (gets file)
    case mfilename of
        Just filename -> buildRun "pdflatex" ["--file-line-error", "--interaction=nonstopmode", filename] (const $ return ())
        Nothing -> return ()
