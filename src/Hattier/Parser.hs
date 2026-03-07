module Hattier.Parser (parseFileToAST, parseTextToAST, defaultDiagOpts, defaultParserOpts, ParseTextToAstError(..)) where

import GHC.Parser (parseModuleNoHaddock)
import GHC.Parser.Lexer (unP, initParserState, ParseResult(..), mkParserOpts, getPsErrorMessages, ParserOpts)
import GHC.Types.SrcLoc (mkRealSrcLoc, unLoc)
import GHC.Hs (HsModule, GhcPs)
import GHC.Utils.Error (DiagOpts(..))
import GHC.Utils.Outputable (defaultSDocContext, ppr, showSDocUnsafe)
import Control.Exception (IOException, try)
import System.IO.Error (isDoesNotExistError)
import qualified GHC.Data.FastString as FS
import qualified GHC.Data.StringBuffer as SB
import qualified GHC.Data.EnumSet as EnumSet
import qualified GHC.Unit.Module.Warnings as Warnings
import Data.Text as T hiding (show)
import Data.Text.IO as TIO

-- | Possible parseTextToAST errors
data ParseTextToAstError
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

-- | Get the AST for a Haskell snippet as 'Text'
parseTextToAST :: Text -> ParserOpts -> Either ParseTextToAstError (HsModule GhcPs)
parseTextToAST src parserOpts = do
  -- The dummy string "" here is fine since we can throw away the RealSrcLoc below with 'unLoc'
  let srcLoc = mkRealSrcLoc (FS.mkFastString "") 1 1 
  let stringBuffer = SB.stringToStringBuffer (T.unpack src)
  let pstate = initParserState parserOpts stringBuffer srcLoc
  case unP parseModuleNoHaddock pstate of
    POk _ ast -> Right (unLoc ast)
    PFailed failedState ->
      Left $ ParseFailed (showSDocUnsafe (ppr (getPsErrorMessages failedState)))

-- | Get the AST for a Haskell file
parseFileToAST :: FilePath -> ParserOpts -> IO (Either ParseTextToAstError (HsModule GhcPs))
parseFileToAST path opts = do
  result <- try (TIO.readFile path)
  case result of
    Left err  -> return $ Left (handleIOError path err)
    Right src -> return $ parseTextToAST src opts

-- | Convert IOException to ParseFileToAstError
handleIOError :: FilePath -> IOException -> ParseTextToAstError
handleIOError filePath err
  | isDoesNotExistError err = FileDoesNotExist filePath
  | otherwise = FileReadError filePath (show err)
