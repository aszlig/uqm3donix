import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import System.Environment (getProgName, getArgs)
import System.FilePath ((</>), (<.>), splitDirectories, splitExtension)

import Data.Char (toLower)
import Data.Binary.Get
import Data.Word (Word8, Word32)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.List (nubBy, isPrefixOf)
import Data.Function (on)
import Data.Int (Int64)

import Control.Applicative (pure, (<$>), (<*>), (<*))
import Control.Monad (unless)

import Text.Printf (printf)

import Codec.Archive.Tar (write)
import qualified Codec.Archive.Tar.Entry as T

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as CB

data OperaControl = NextBlock | EndTree | NoOp
    deriving (Show)

data OperaFileType = Plain | Directory
    deriving (Show)

data OperaHeader = OperaHeader
    { operaControl :: OperaControl
    , operaFileType :: OperaFileType
    , operaFileSize :: Word32
    , operaFileName :: FilePath
    , operaCopies :: Word32
    , operaStartBlock :: Word32
    } deriving (Show)

data OperaFile = OperaFile
    { fileHeader :: OperaHeader
    , fileData :: Either [OperaFile] B.ByteString
    } deriving (Show)

skipL :: Int64 -> Get ()
skipL n = getLazyByteString n >> return ()

seekTo :: Integral a => a -> Get ()
seekTo = (>>=) (fmap fromIntegral bytesRead) . fmap skipL . (-) . fromIntegral

skipToBlock :: Integral a => a -> Get ()
skipToBlock = seekTo . (* 2048)

ensure8 :: Word8 -> Get ()
ensure8 x = getWord8 >>= flip unless (fail "Invalid magic") . (== x)

ensureMagic :: Get ()
ensureMagic = mapM_ ensure8 [0x01, 0x5a, 0x5a, 0x5a, 0x5a, 0x5a, 0x01]

getOperaString :: Int -> Get B.ByteString
getOperaString = fmap (B.takeWhile (/= 0)) . getLazyByteString . fromIntegral

getControl :: Get OperaControl
getControl = getWord8 >>= convert
  where convert 0x00 = return NoOp
        convert 0xC0 = return EndTree
        convert 0x40 = return NextBlock
        convert what = fail $ "Unknown control value " ++ show what

getFileType :: Get OperaFileType
getFileType = getWord8 >>= convert
  where convert 0x02 = return Plain
        convert 0x06 = return Plain
        convert 0x07 = return Directory
        convert what = fail $ "Unknown attribute " ++ show what

getFileHeader :: Get OperaHeader
getFileHeader = OperaHeader <$> getControl
                            <*  skipL 2  <*> getFileType
                            <*  skipL 12 <*> getWord32be
                            <*  skipL 12 <*> (CB.unpack <$> getOperaString 32)
                            <*> getWord32be
                            <*> getWord32be

getOperaData :: OperaHeader -> Get B.ByteString
getOperaData header = skipToBlock (operaStartBlock header) >>
    getLazyByteString (fromIntegral $ operaFileSize header)

skipCopies :: OperaHeader -> Get ()
skipCopies = skipL . (* 4) . fromIntegral . operaCopies

getOperaFile :: B.ByteString -> Get OperaFile
getOperaFile bs = getFileHeader >>= \header -> skipCopies header >>
    OperaFile <$> pure header <*> handleType (operaFileType header) header
  where
    handleType Directory =
        pure . Left . getDir bs . fromIntegral . operaStartBlock
    handleType Plain =
        pure . Right . flip runGet bs . getOperaData

getSuperBlock :: B.ByteString -> Int
getSuperBlock = runGet $ lookAhead ensureMagic >> seekTo (0x64::Int) >>
    fromIntegral <$> getWord32be

getOperaFiles :: B.ByteString -> Int -> [OperaFile] -> Get [OperaFile]
getOperaFiles bs block files = getOperaFile bs >>= \file ->
    case operaControl . fileHeader $ file of
         NoOp      -> getOperaFiles bs block       (file : files)
         NextBlock -> skipToBlock (block + 1) >> skipL 20 >>
             getOperaFiles bs (block + 1) (file : files)
         EndTree   -> return (file : files)

getDir :: B.ByteString -> Int -> [OperaFile]
getDir bs block = flip runGet bs $
    skipToBlock block >> skipL 20 >> getOperaFiles bs block []

readFiles :: B.ByteString -> [OperaFile]
readFiles bs = getDir bs $ getSuperBlock bs

(~=) :: String -> String -> Bool
a ~= b = map toLower a == map toLower b

findName :: String -> [OperaFile] -> Maybe OperaFile
findName name = listToMaybe . filter ((~= name) . operaFileName . fileHeader)

findFileFull :: [String] -> OperaFile -> Maybe OperaFile
findFileFull [] file = Just file
findFileFull (name : rest) dir =
    case fileData dir of
         Left  sub -> findName name sub >>= findFileFull rest
         Right _   -> Just dir

findFile :: [String] -> [OperaFile] -> Maybe OperaFile
findFile (first : rest) files = findName first files >>= findFileFull rest
findFile _              _     = Nothing

findFiles :: [[String]] -> [OperaFile] -> [OperaFile]
findFiles ms ofs = mapMaybe (`findFile` ofs) ms

duckify :: [String] -> [String] -> [[String]]
duckify [] _ = []
duckify (b:bs) es = map (\e -> ["duckart", b <.> e]) es ++ duckify bs es

findDuckFiles :: [OperaFile] -> [OperaFile]
findDuckFiles = let printShip :: Integer -> String
                    printShip = printf "ship%02d"
                    ships = map printShip [0..24]
                    bases = ["intro", "victory", "spin"] ++ ships
                    exts = ["aif", "duk", "frm", "hdr", "tbl"]
                    in findFiles $ duckify bases exts

toTarEntry :: T.TarPath -> OperaFile -> T.Entry
toTarEntry tp file =
    case fileData file of
         Left  _  -> T.directoryEntry tp
         Right bs -> T.fileEntry tp . CB.fromChunks $ B.toChunks bs

forceToTarPath :: Bool -> FilePath -> T.TarPath
forceToTarPath dir fp =
    either panic id $ T.toTarPath dir fp
  where
    panic = error "Panic! Tar path seems to be too long!"

createParents :: [String] -> [T.Entry]
createParents = map (T.directoryEntry . forceToTarPath True)

toDuckTarEntries :: OperaFile -> [T.Entry]
toDuckTarEntries file =
    createParents parents ++ [toTarEntry tarpath file]
  where
    parents = scanl1 (</>) . init $ splitDirectories filename

    filename = getFilename . splitExtension $ getOperaLowerFilename file
    tarpath = forceToTarPath False filename

    getOperaLowerFilename = map toLower . operaFileName . fileHeader

    prefix = "content" </> "addons" </> "3dovideo"
    getFilename ("spin",      ext) = prefix </> "spins"  </> "spin"    <.> ext
    getFilename ("intro",     ext) = prefix </> "intro"  </> "intro"   <.> ext
    getFilename ("victory",   ext) = prefix </> "ending" </> "victory" <.> ext
    getFilename (name,        ext)
        | "ship" `isPrefixOf` name = prefix </> "spins"  </> name      <.> ext
        | otherwise                = prefix </> "misc"   </> name      <.> ext

removeDuplicates :: [T.Entry] -> [T.Entry]
removeDuplicates = nubBy ((==) `on` T.entryTarPath)

resetTimeStamps :: [T.Entry] -> [T.Entry]
resetTimeStamps = map (\e -> e { T.entryTime = 720572400 })

createDuckTar :: [OperaFile] -> B.ByteString
createDuckTar = write . resetTimeStamps . removeDuplicates .
    concatMap toDuckTarEntries . findDuckFiles

extractFiles :: FilePath -> [OperaFile] -> IO ()
extractFiles out = B.writeFile out . createDuckTar

main :: IO ()
main =
    getArgs >>= run
  where
    run [iso, out] = extractFiles out =<< fmap readFiles (B.readFile iso)
    run _          = getProgName >>= usage >> exitFailure

    usage pn = mapM (hPutStrLn stderr)
        [ "Usage: " ++ pn ++ " CDIMAGE OUTFILE"
        , "Put videos from a 3DO Star Control II CD image into a TAR archive."
        , "The TAR archive has fixed timestamps so it's suitable for Nix."
        ]
