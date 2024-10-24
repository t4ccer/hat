-- | Lexicon code generator targeting Haskell source code
module Hat.Lexicon.Printer.Haskell (generateLexicon) where

import Control.Monad (forM_, unless, void, when)
import Control.Monad.State (MonadState, State, execState, gets, modify')
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Char8 qualified as Char8
import Data.Char (toUpper)
import Data.Foldable (Foldable (foldl'), traverse_)
import Data.List (nub, uncons)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import System.IO (IOMode (WriteMode), hClose, openFile)

import Hat.Lexicon.Schema

firstToUpper :: ByteString -> Builder
firstToUpper bs =
  case Char8.uncons bs of
    Nothing -> mempty
    Just (first, rest) -> Builder.char8 (toUpper first) <> Builder.byteString rest

intercalate' :: Monoid m => m -> [m] -> m
intercalate' _ [] = mempty
intercalate' _ [m] = m
intercalate' x (m : ms) = m <> x <> intercalate' x ms

idToModule :: ByteString -> Builder
idToModule = intercalate' "." . map firstToUpper . Char8.split '.'

unionSuffix :: ByteString
unionSuffix = "Kind"

builderToStrict :: Builder -> ByteString
builderToStrict = ByteString.toStrict . Builder.toLazyByteString

data PrinterState = PrinterState
  { imports :: Set ByteString
  , output :: Builder
  , bootOutput :: Builder
  , adHocDefs :: Map ByteString Definition
  , lexiconId :: ByteString
  }

newtype Printer a = Printer (State PrinterState a)
  deriving newtype (Functor, Applicative, Monad, MonadState PrinterState)

generateAdHocType :: ByteString -> Definition -> Printer ()
generateAdHocType name def = modify' (\p -> p {adHocDefs = Map.insert name def p.adHocDefs})

addImport :: ByteString -> Printer ()
addImport imp = modify' (\p -> p {imports = Set.insert imp p.imports})

append :: Builder -> Printer ()
append b = modify' $ \p ->
  PrinterState
    { output = p.output <> b
    , bootOutput = p.bootOutput
    , imports = p.imports
    , adHocDefs = p.adHocDefs
    , lexiconId = p.lexiconId
    }

prepend :: Builder -> Printer ()
prepend b = modify' $ \p ->
  PrinterState
    { output = b <> p.output
    , bootOutput = p.bootOutput
    , imports = p.imports
    , adHocDefs = p.adHocDefs
    , lexiconId = p.lexiconId
    }

appendBoot :: Builder -> Printer ()
appendBoot b = modify' $ \p ->
  PrinterState
    { output = p.output
    , bootOutput = p.bootOutput <> b
    , imports = p.imports
    , adHocDefs = p.adHocDefs
    , lexiconId = p.lexiconId
    }

runPrinter :: Lexicon -> (Builder, Builder)
runPrinter lexicon =
  let
    Printer p = printLexicon lexicon
    res = execState p (PrinterState mempty mempty mempty mempty lexicon.lexiconId)
   in
    (res.output, res.bootOutput)

unsnocRef :: ByteString -> ByteString -> (ByteString, ByteString)
unsnocRef lexiconId ref =
  case Char8.split '#' ref of
    [mainName] -> unsnocRef lexiconId (lexiconId <> "#" <> mainName)
    ["", name] -> unsnocRef lexiconId (lexiconId <> "#" <> name)
    [namespace, name] -> (namespace, name)
    _ -> error ("Invalid ref: " <> show ref)

refToCanonical :: ByteString -> ByteString -> ByteString
refToCanonical lexiconId ref =
  case Char8.split '#' ref of
    [mainName] -> mainName <> "#" <> last (Char8.split '.' mainName)
    ["", name] -> lexiconId <> "#" <> name
    [_namespace, _name] -> ref
    _ -> error ("Invalid ref: " <> show ref)

printRef :: ByteString -> Printer ()
printRef ref = do
  lexiconId <- gets $ \s -> s.lexiconId
  case Char8.split '#' ref of
    [mainName] -> do
      addImport $ builderToStrict $ idToModule mainName
      append (idToModule mainName <> "." <> firstToUpper (last $ Char8.split '.' mainName))
    ["", name] -> append $ firstToUpper name
    [namespace, name] -> do
      if namespace == lexiconId
        then do
          append $ firstToUpper name
        else do
          addImport $ builderToStrict $ idToModule namespace
          append (idToModule namespace <> "." <> firstToUpper name)
    _ -> error ("Invalid ref: " <> show ref)

printType :: NonEmpty ByteString -> Definition -> Printer ()
printType _binding (DefinitionRef ref) = printRef ref
printType _binding (DefinitionInteger _int) = append "Integer"
printType _binding (DefinitionString _str) = append "Data.Text.Text"
printType binding (DefinitionArray arr) = do
  append "["
  printType binding arr.items
  append "]"
printType _binding (DefinitionBoolean _) = append "Bool"
printType binding def@(DefinitionUnion _) = do
  let unionName = builderToStrict (foldMap firstToUpper binding <> Builder.byteString unionSuffix)
  generateAdHocType unionName def
  append $ Builder.byteString unionName
printType _binding DefinitionUnknown = append "Data.Aeson.Value"
printType _binding (DefinitionBlob _) = append "Data.ByteString.ByteString"
printType _binding (DefinitionBytes _) = append "Data.ByteString.ByteString"
printType _binding (DefinitionCidLink _) = append "Data.Text.Text"
printType binding (DefinitionObject _) = error (show binding <> " has type object")
printType binding (DefinitionQuery _) = error (show binding <> " has type query")
printType binding (DefinitionProcedure _) = error (show binding <> " has type procedure")
printType binding (DefinitionToken _) = error (show binding <> " has type token")
printType binding (DefinitionSubscription _) = error (show binding <> " has type subscription")
printType binding (DefinitionRecord _) = error (show binding <> " has type recrod")

keywordNames :: Set ByteString
keywordNames = Set.fromList ["type", "default", "data", "newtype", "module", "id", "mconcat", "fmap", "v"]

sanitizeField :: ByteString -> ByteString
sanitizeField fieldName = fieldName <> if fieldName `Set.member` keywordNames then "'" else mempty

isFieldOptional :: DefObject -> ByteString -> Bool
isFieldOptional obj fieldName = fieldName `elem` obj.nullable || fieldName `notElem` obj.required

printField :: ByteString -> DefObject -> ByteString -> Definition -> Printer ()
printField objName obj fieldName def = do
  append $ Builder.byteString $ sanitizeField fieldName
  append " :: "
  append $
    if isFieldOptional obj fieldName
      then "Maybe "
      else mempty
  printType (objName :| [fieldName]) def

printObjectFields :: Builder -> ByteString -> DefObject -> Printer ()
printObjectFields indent objName obj =
  case map (uncurry (printField objName obj)) $ Map.toList obj.properties of
    [] -> pure ()
    (f : fs) -> do
      append " "
      f
      append "\n"

      forM_ fs $ \f' -> do
        append indent
        append ", "
        f'
        append "\n"

datatype :: Builder -> ByteString -> Printer ()
datatype kind name = do
  append (kind <> " " <> firstToUpper name <> " = " <> firstToUpper name <> "\n")
  appendBoot ("data " <> firstToUpper name <> "\n\n")

newtype' :: ByteString -> Printer ()
newtype' = datatype "newtype"

data' :: ByteString -> Printer ()
data' = datatype "data"

uniqueSuffix :: ByteString -> [ByteString] -> [ByteString]
uniqueSuffix lexiconId refs =
  let (namespaces, heads) = unzip $ map (unsnocRef lexiconId) refs
   in if length (nub heads) == length heads
        then map (builderToStrict . printRefIdent) heads
        else
          uniqueSuffix lexiconId $
            zipWith (\ns h -> ns <> builderToStrict (firstToUpper h)) namespaces heads
  where
    printRefIdent :: ByteString -> Builder
    printRefIdent ref =
      case Char8.split '#' ref of
        [mainName] -> firstToUpper (last $ Char8.split '.' mainName)
        ["", name] -> firstToUpper name
        [_namespace, name] -> firstToUpper name
        _ -> error ("Invalid ref: " <> show ref)

printBootJson :: ByteString -> Printer ()
printBootJson name = do
  appendBoot "instance Data.Aeson.FromJSON "
  appendBoot $ firstToUpper name
  appendBoot "\n\n"

  appendBoot "instance Data.Aeson.ToJSON "
  appendBoot $ firstToUpper name
  appendBoot "\n\n"

  appendBoot "\n"

needsUtf8 :: Definition -> Bool
needsUtf8 (DefinitionBytes _) = True
needsUtf8 (DefinitionBlob _) = True
needsUtf8 _ = False

forNotFirstM :: Monad m => m () -> [a] -> (a -> m b) -> m ()
forNotFirstM _ [] _ = pure ()
forNotFirstM h (a : as) f = do
  void $ f a
  forM_ as ((h >>) . f)

printToJsonNewtype :: ByteString -> Printer ()
printToJsonNewtype name = do
  append "instance Data.Aeson.ToJSON "
  append $ firstToUpper name
  append " where \n"
  append "  toJSON ("
  append $ firstToUpper name
  append " get"
  append $ firstToUpper name
  append ") = Data.Aeson.toJSON get"
  append $ firstToUpper name
  append "\n"
  append "  toEncoding ("
  append $ firstToUpper name
  append " get"
  append $ firstToUpper name
  append ") = Data.Aeson.toEncoding get"
  append $ firstToUpper name
  append "\n"

printToJsonFieldList :: DefObject -> Printer ()
printToJsonFieldList obj = do
  append "    [ "
  forNotFirstM (append "    , ") (Map.toList obj.properties) $ \(fieldName, def) -> do
    append "Data.Aeson.Key.fromString \""
    append $ Builder.byteString fieldName
    append "\" Data.Aeson."
    append $ case (isFieldOptional obj fieldName, needsUtf8 def) of
      (False, False) -> ".= "
      (True, False) -> ".?= "
      (False, True) -> ".= Data.Text.Encoding.decodeUtf8 "
      (True, True) -> ".?= fmap Data.Text.Encoding.decodeUtf8 "
    append $ Builder.byteString $ sanitizeField fieldName
    append "\n"
  append "    ]\n"

appendInstances :: ByteString -> Printer ()
appendInstances name = do
  append "  deriving (Show, Read, Eq, Ord)\n\n"
  appendBoot $ "instance Show " <> firstToUpper name <> "\n"
  appendBoot $ "instance Read " <> firstToUpper name <> "\n"
  appendBoot $ "instance Eq " <> firstToUpper name <> "\n"
  appendBoot $ "instance Ord " <> firstToUpper name <> "\n"

printDefinition :: CanonicalName -> Definition -> Printer ()
printDefinition (CanonicalName name _) (DefinitionObject obj) = do
  data' name
  append "  {"
  printObjectFields "  " name obj
  append "  }\n"
  appendInstances name

  printBootJson name
  append "instance Data.Aeson.FromJSON "
  append $ firstToUpper name
  append " where \n"
  append "  parseJSON = Data.Aeson.withObject \""
  append $ firstToUpper name
  append "\" $ \\v -> do\n"
  forM_ (Map.toList obj.properties) $ \(fieldName, def) -> do
    append "    "
    append $ Builder.byteString $ sanitizeField fieldName
    append " <- "

    when (needsUtf8 def) $ do
      when (isFieldOptional obj fieldName) (append "fmap ")
      append "Data.Text.Encoding.encodeUtf8 <$> "

    if isFieldOptional obj fieldName
      then append "v Data.Aeson..:?"
      else append "v Data.Aeson..:"
    append " Data.Aeson.Key.fromString \""
    append $ Builder.byteString fieldName
    append "\"\n"

  append "    pure $ "
  append $ firstToUpper name
  append " "
  forM_ (Map.keys obj.properties) $ \fieldName -> do
    append $ Builder.byteString $ sanitizeField fieldName
    append " "
  append "\n\n"

  append "instance Data.Aeson.ToJSON "
  append $ firstToUpper name
  append " where \n"

  append "  toJSON ("
  append $ firstToUpper name
  forM_ (Map.keys obj.properties) $ \fieldName -> do
    append " "
    append $ Builder.byteString $ sanitizeField fieldName
  append ") = Data.Aeson.Object $ mconcat\n"
  printToJsonFieldList obj

  append "  toEncoding ("
  append $ firstToUpper name
  forM_ (Map.keys obj.properties) $ \fieldName -> do
    append " "
    append $ Builder.byteString $ sanitizeField fieldName
  append ") = Data.Aeson.pairs $ mconcat\n"
  printToJsonFieldList obj
  append "\n"

  appendBoot $ Builder.byteString name
  appendBoot "'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => "
  appendBoot $ firstToUpper name
  appendBoot " -> kv\n"

  append $ Builder.byteString name
  append "'AesonFields :: (Data.Aeson.KeyValueOmit e kv, Monoid kv) => "
  append $ firstToUpper name
  append " -> kv\n"
  append $ Builder.byteString name
  append "'AesonFields ("
  append $ firstToUpper name
  forM_ (Map.keys obj.properties) $ \fieldName -> do
    append " "
    append $ Builder.byteString $ sanitizeField fieldName
  append ") = mconcat\n"
  printToJsonFieldList obj

  append "\n\n"
printDefinition (CanonicalName name _) (DefinitionString _str) = do
  newtype' name
  append ("  { get" <> firstToUpper name <> " :: Data.Text.Text\n")
  append "  }\n"
  appendInstances name

  printBootJson name
  append "instance Data.Aeson.FromJSON "
  append $ firstToUpper name
  append " where \n"
  append "  parseJSON = Data.Aeson.withText \""
  append $ firstToUpper name
  append "\" $ pure . "
  append $ firstToUpper name
  append "\n\n"

  printToJsonNewtype name

  append "\n\n"
printDefinition (CanonicalName name _) (DefinitionInteger _int) = do
  newtype' name
  append ("  { get" <> firstToUpper name <> " :: Integer\n")
  append "  }\n"
  appendInstances name

  printBootJson name
  append "instance Data.Aeson.FromJSON "
  append $ firstToUpper name
  append " where \n"
  append "  parseJSON = ("
  append $ firstToUpper name
  append " <$>) . Data.Aeson.parseJSON"
  append "\n\n"

  printToJsonNewtype name

  append "\n\n"
printDefinition (CanonicalName name _) (DefinitionBoolean _bool) = do
  newtype' name
  append ("  { get" <> firstToUpper name <> " :: Bool\n")
  append "  }\n"
  appendInstances name

  printBootJson name
  append "instance Data.Aeson.FromJSON "
  append $ firstToUpper name
  append " where \n"
  append "  parseJSON = Data.Aeson.withBool \""
  append $ firstToUpper name
  append "\" $ pure . "
  append $ firstToUpper name
  append "\n\n"
  printToJsonNewtype name

  append "\n\n"
printDefinition (CanonicalName name _) (DefinitionArray arr) = do
  newtype' name
  append ("  { get" <> firstToUpper name <> " :: [")
  printType (name :| []) arr.items
  append "]\n"
  append "  }\n"
  appendInstances name

  printBootJson name
  append "instance Data.Aeson.FromJSON "
  append $ firstToUpper name
  append " where \n"
  append "  parseJSON = fmap "
  append $ firstToUpper name
  append " . Data.Aeson.parseJSON\n\n"
  printToJsonNewtype name
printDefinition _ DefinitionUnknown = pure ()
printDefinition (CanonicalName name _) (DefinitionUnion union) = do
  appendBoot ("data " <> firstToUpper name <> "\n\n")
  append ("data " <> firstToUpper name <> "\n")

  let variant ref suffix = do
        append $ Builder.byteString name -- common name prefix
        append $ Builder.byteString suffix
        append " "
        printRef ref
        append "\n"
  lexiconId <- gets $ \s -> s.lexiconId
  case zip union.refs (uniqueSuffix lexiconId union.refs) of
    [] -> pure ()
    ((ref, suffix) : refs) -> do
      append "  = "
      variant ref suffix
      forM_ refs $ \(ref', suffix') -> do
        append "  | "
        variant ref' suffix'
  appendInstances name

  printBootJson name
  append "instance Data.Aeson.FromJSON "
  append $ firstToUpper name
  append " where \n"
  append "  parseJSON = Data.Aeson.withObject \""
  append $ firstToUpper name
  append "\" $ \\v -> do\n"
  append "    v Data.Aeson..: Data.Aeson.Key.fromString \"$type\" >>= \\case\n"
  forM_ (zip union.refs (uniqueSuffix lexiconId union.refs)) $ \(ref, suffix) -> do
    append "      \""
    append $ Builder.byteString (refToCanonical lexiconId ref)
    append "\" -> "
    append $ Builder.byteString name
    append $ Builder.byteString suffix
    append " <$> Data.Aeson.parseJSON (Data.Aeson.Object v)\n"
  append "      _ -> fail \"Invalid type\"\n\n"

  append "instance Data.Aeson.ToJSON "
  append $ firstToUpper name
  append " where \n"
  append "  toJSON = \\case\n"
  forM_ (zip union.refs (uniqueSuffix lexiconId union.refs)) $ \(ref, suffix) -> do
    append "      "
    append $ Builder.byteString name
    append $ Builder.byteString suffix
    append " v -> Data.Aeson.Object \n"
    append "        (("
    append "Data.Aeson.Key.fromString \"$type\" Data.Aeson..= \""
    append $ Builder.byteString (refToCanonical lexiconId ref)
    append "\") <> ("
    case Char8.split '#' $ refToCanonical lexiconId ref of
      [path, obj] -> do
        append $ intercalate' "." (map firstToUpper $ Char8.split '.' path)
        append "."
        append $ Builder.byteString obj
        append "'AesonFields v))\n"
      _ -> error "unreachable"
  append "  toEncoding = \\case\n"
  forM_ (zip union.refs (uniqueSuffix lexiconId union.refs)) $ \(ref, suffix) -> do
    append "      "
    append $ Builder.byteString name
    append $ Builder.byteString suffix
    append " v -> Data.Aeson.pairs \n"
    append "        (("
    append "Data.Aeson.Key.fromString \"$type\" Data.Aeson..= \""
    append $ Builder.byteString (refToCanonical lexiconId ref)
    append "\") <> ("
    case Char8.split '#' $ refToCanonical lexiconId ref of
      [path, obj] -> do
        append $ intercalate' "." (map firstToUpper $ Char8.split '.' path)
        append "."
        append $ Builder.byteString obj
        append "'AesonFields v))\n"
      _ -> error "unreachable"

  append "\n\n"
printDefinition (CanonicalName name _) (DefinitionRef ref) = do
  newtype' name
  append ("  { get" <> firstToUpper name <> " :: ")
  printRef ref
  append "\n"
  append "  }\n\n"
-- FIXME: JSON instances
printDefinition name (DefinitionRecord r) = printDefinition name r.record
printDefinition _ (DefinitionBlob _) = pure ()
printDefinition (CanonicalName name isMain) (DefinitionQuery q) = do
  unless isMain $ error "Figure out what to do"

  result <- case q.output of
    Nothing -> pure "()"
    Just out -> case out.schema of
      Nothing -> pure "()"
      Just outSchema -> do
        let resTy = CanonicalName (name <> "Result") False
        printDefinition resTy outSchema
        pure $ firstToUpper resTy.name

  encoding <- case q.output of
    Nothing -> pure "Servant.API.JSON" -- doesn't matter as we return 204 (NoContent??)
    Just out -> case out.encoding of
      "application/json" -> pure "Servant.API.JSON"
      "application/jsonl" -> pure "" -- FIXME: Add JSONL
      "*/*" -> pure "" -- FIXME
      "application/vnd.ipld.car" -> pure "" -- FIXME: https://atproto.com/specs/repository#car-file-serialization
      other -> error $ "unsupported encoding: " <> show other <> "on" <> show name

  append "type "
  append $ firstToUpper name
  append " = \""
  lexiconId <- gets $ \s -> s.lexiconId
  append $ Builder.byteString lexiconId
  append "\" Servant.API.:> "
  append "Servant.API.Header \"Authorization\" Data.Text.Text "

  case q.parameters of
    Nothing -> pure ()
    Just (DefParameters params) ->
      forM_ (Map.toList params.properties) $ \(paramName, paramDef) -> do
        append "Servant.API.:> Servant.API.QueryParam' '[Servant.API.Strict, "
        if paramName `elem` params.required
          then append "Servant.API.Strict"
          else append "Servant.API.Optional"
        append "] \""
        append $ Builder.byteString paramName
        append "\" "

        let printParam = \case
              DefinitionString _ -> append "Data.Text.Text"
              DefinitionBoolean _ -> append "Bool"
              DefinitionInteger _ -> append "Integer"
              DefinitionArray arr -> do
                append "["
                printParam arr.items
                append "]"
              _ -> error "malformed schema"
        printParam paramDef
        append " "

  append "Servant.API.:> Servant.API.Get '["
  append encoding
  append "] "
  append result
  append "\n"

-- error $ show name <> "\n" <> show q
printDefinition _ (DefinitionProcedure _) = pure ()
printDefinition _ (DefinitionToken _) = pure ()
printDefinition _ (DefinitionBytes _) = pure ()
printDefinition _ (DefinitionCidLink _) = pure ()
printDefinition _ (DefinitionSubscription _) = pure ()

data CanonicalName = CanonicalName
  { name :: ByteString
  , isMain :: Bool
  }
  deriving stock (Show, Eq, Ord)

printDefs :: ByteString -> Printer ()
printDefs lexiconId = do
  lexiconDefs <- gets $ \s -> s.adHocDefs

  let
    moduleName = last $ Char8.split '.' lexiconId
    canonicalName "main" =
      case Map.lookup moduleName lexiconDefs of
        Just _ -> CanonicalName (moduleName <> "Main") True
        Nothing -> CanonicalName moduleName True
    canonicalName name = CanonicalName name False

  modify' (\p -> p {adHocDefs = mempty})
  if Map.null lexiconDefs
    then pure ()
    else do
      traverse_ (\(k, v) -> printDefinition (canonicalName k) v) $ Map.toList lexiconDefs
      printDefs lexiconId

printLexicon :: Lexicon -> Printer ()
printLexicon (Lexicon lexiconId lexiconDefs) = do
  appendBoot ("module " <> idToModule lexiconId <> " where\n\n")
  appendBoot "import qualified Data.Aeson \n\n"

  modify' (\p -> p {adHocDefs = lexiconDefs})
  printDefs lexiconId

  prepend "\n"
  imports <- gets $ \s -> s.imports
  forM_ imports $ \imp ->
    prepend ("import {-# SOURCE #-} qualified " <> Builder.byteString imp <> "\n")
  prepend "import qualified Data.Aeson\n"
  prepend "import qualified Data.Aeson.Key\n"
  prepend "import qualified Servant.API\n"
  prepend "import qualified Data.Text\n"
  prepend "import qualified Data.Text.Encoding\n"
  prepend "import qualified Data.ByteString\n"

  prepend ("module " <> idToModule lexiconId <> " where\n\n")
  prepend "{-# LANGUAGE DuplicateRecordFields #-}\n\n"
  prepend "{-# LANGUAGE NamedFieldPuns #-}\n"
  prepend "{-# LANGUAGE OverloadedRecordDot #-}\n"
  prepend "{-# LANGUAGE DataKinds #-}\n"
  prepend "{-# LANGUAGE TypeOperators #-}\n"
  prepend "{-# LANGUAGE NoFieldSelectors #-}\n"
  prepend "{-# LANGUAGE LambdaCase #-}\n"

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ [] = pure ([], [])
partitionM f (x : xs) = do
  res <- f x
  (as, bs) <- partitionM f xs
  pure ([x | res] ++ as, [x | not res] ++ bs)

traverseDir :: FilePath -> (FilePath -> IO x) -> IO ()
traverseDir root action = go root
  where
    go dirPath = do
      isDir <- doesDirectoryExist dirPath
      if isDir
        then do
          names <- listDirectory dirPath
          let paths = map (dirPath </>) names
          (dirPaths, filePaths) <- partitionM doesDirectoryExist paths
          traverse_ action filePaths
          traverse_ go dirPaths
        else void $ action dirPath

unsnoc :: [a] -> Maybe ([a], a)
unsnoc xs = (\(hd, tl) -> (reverse tl, hd)) <$> uncons (reverse xs)

-- | Generate tree of Haskell modules corresponding to the lexicon.
generateLexicon ::
  -- | Input directory with lexicon JSON files.
  FilePath ->
  -- | Output directory for Haskell modules.
  FilePath ->
  IO ()
generateLexicon sourceDir destDir = do
  traverseDir sourceDir $ \jsonFp -> do
    Aeson.eitherDecodeFileStrict' @Lexicon jsonFp >>= \case
      Right lexicon -> do
        let modPath = Char8.split '.' (builderToStrict $ idToModule lexicon.lexiconId)
        case unsnoc modPath of
          Nothing -> undefined
          Just (paths, modName) -> do
            let topDir = foldl' (\fp acc -> fp </> Char8.unpack acc) destDir paths
            createDirectoryIfMissing True topDir
            let hsFile = topDir </> Char8.unpack modName <> ".hs"
            outHsFile <- openFile hsFile WriteMode
            outHsBootFile <- openFile (hsFile <> "-boot") WriteMode
            let (out, outBoot) = runPrinter lexicon
            Builder.hPutBuilder outHsFile out
            Builder.hPutBuilder outHsBootFile outBoot
            hClose outHsFile
            hClose outHsBootFile
            putStrLn ("  " <> hsFile <> " ... OK")
      Left err -> putStrLn ("  " <> jsonFp <> " ... ERR: " <> show err)
