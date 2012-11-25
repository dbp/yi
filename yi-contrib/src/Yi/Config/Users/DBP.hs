{-# LANGUAGE NoMonomorphismRestriction, NamedFieldPuns, DoAndIfThenElse #-}
--import Yi

-- Preamble
import Yi.Prelude hiding ((%=))
import Prelude ()

import Yi.Config.Simple
import Yi.Command(buildRun)
import Yi.Mode.Latex(latexMode3)
import Yi.File(fwriteE)
import Yi.Buffer.HighLevel(fillParagraph)

import Yi.Style.Library

import System.Directory
import System.FilePath

import Control.Exception (catch, SomeException) -- to try to start graphical frontend and, on failing, start vty
import Graphics.UI.Gtk (initGUI) -- for testing presence of GUI

main :: IO ()
main = do
    hasGUI <- catch (initGUI >> return True) ((const (return False))::(SomeException -> IO Bool))
    if hasGUI
    then configMain defaultEmacsConfig setupGUI
    else configMain defaultEmacsConfig setupVty


setupGUI :: ConfigM ()
setupGUI = do
  setFrontend "pango"
  setup

setupVty :: ConfigM ()
setupVty = do
  setFrontend "vty"
  setup

setup :: ConfigM ()
setup = do
  fontSize %= Just 9

  theme %= darkBlueTheme

  globalBindKeys globalBindings
  evaluator %= publishedActionsEvaluator
  publishAction "createDirectory" yiCreateDirectory

  publishAction "reformatParagraph" reformatParagraph

  addMode latexMode3
  publishAction "compileLatex" compileLatex
  modeBindKeys latexMode3 (ctrlCh 'c' ?>> ctrlCh 'c' ?>>! compileLatex)
   
   


globalBindings = choice
   [
     metaCh 'g' ?>>! gotoLn
   , ctrl (spec KRight) ?>>! nextWordB
   , ctrl (spec KLeft) ?>>! prevWordB
   , ctrl (spec KBS) ?>>! deleteWordBackward
   ]

deleteWordBackward = deleteRegionB =<< regionOfPartNonEmptyB unitWord Backward

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

reformatParagraph :: YiM ()
reformatParagraph = withEditor $ withBuffer0 fillParagraph

compileLatex = do
    mfilename <- withEditor $ withBuffer0 (gets file)
    -- get location for output
    BufferFileInfo{bufInfoFileName} <- withEditor $ withBuffer0 bufInfoB
    let dir = takeDirectory bufInfoFileName
    -- save file if needed
    fwriteE
    case mfilename of
        Just filename -> buildRun "/usr/bin/pdflatex" ["--output-directory", dir, "--file-line-error", 
                                                       "--interaction=nonstopmode", filename] (const $ return ())
        Nothing -> return ()
