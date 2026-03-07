module Hattier.Parser (parseFileToAST, defaultDiagOpts, defaultParserOpts, ParseFileToAstError(..)) where

import GHC.Parser (parseModuleNoHaddock)
import GHC.Parser.Lexer (unP, initParserState, ParseResult(..), mkParserOpts, getPsErrorMessages, ParserOpts)
import GHC.Types.SrcLoc (Located, mkRealSrcLoc)
import GHC.Hs (HsModule, GhcPs)
import GHC.Utils.Error (DiagOpts(..))
import GHC.Utils.Outputable (defaultSDocContext, ppr, showSDocUnsafe)
import Control.Exception (catch, IOException)
import System.IO.Error (isDoesNotExistError)
import qualified GHC.Data.FastString as FS
import qualified GHC.Data.StringBuffer as SB
import qualified GHC.Data.EnumSet as EnumSet
import qualified GHC.Unit.Module.Warnings as Warnings

-- | Possible parseFileToAST errors
data ParseFileToAstError
  = FileDoesNotExist FilePath
  | FileReadError FilePath String
  | ParseFailed String
  deriving (Show, Eq)

-- | Default DiagOpts to construct defaultParserOpts with
defaultDiagOpts :: DiagOpts
defaultDiagOpts = DiagOpts
  { diag_warning_flags = EnumSet.empty
  , diag_fatal_warning_flags = EnumSet.empty
  , diag_custom_warning_categories = Warnings.emptyWarningCategorySet
  , diag_fatal_custom_warning_categories = Warnings.emptyWarningCategorySet
  , diag_warn_is_error = False
  , diag_reverse_errors = False
  , diag_max_errors = Nothing
  , diag_ppr_ctx = defaultSDocContext
  }

-- | Default ParserOpts to give to parseFileToAST
defaultParserOpts :: ParserOpts
defaultParserOpts = mkParserOpts 
  EnumSet.empty   -- permitted language extensions enabled
  defaultDiagOpts -- diagnostic options
  []              -- Supported Languages and Extensions 
  False           -- are safe imports on?
  False           -- keeping Haddock comment tokens
  True            -- keep regular comment tokens
  True            -- line/column update internal position

-- | Get the AST for a Haskell file
parseFileToAST :: FilePath -> ParserOpts -> IO (Either ParseFileToAstError (Located (HsModule GhcPs)))
parseFileToAST filePath parserOpts = do
  file <- catch
      (Right <$> readFile filePath)
      (\e -> return $ Left $ handleIOError filePath e)
  case file of
    Left err -> return $ Left err
    Right fileContents -> do
      let srcLoc = mkRealSrcLoc (FS.mkFastString filePath) 1 1
      let stringBuffer = SB.stringToStringBuffer fileContents
      let pstate = initParserState parserOpts stringBuffer srcLoc
      case unP parseModuleNoHaddock pstate of
        POk _ ast -> return $ Right ast
        PFailed failedState ->
          return $ Left $ ParseFailed (showSDocUnsafe (ppr (getPsErrorMessages failedState)))

-- | Convert IOException to ParseFileToAstError
handleIOError :: FilePath -> IOException -> ParseFileToAstError
handleIOError filePath err
  | isDoesNotExistError err = FileDoesNotExist filePath
  | otherwise = FileReadError filePath (show err)
