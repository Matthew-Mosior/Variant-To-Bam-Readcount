{-=VariantToBamReadcount (vtbr): A Haskell-based solution=-}
{-=for creating mgibed (bam-readcount input)=-} 
{-=files from transformed ensembl-vep output.=-}
{-=Author: Matthew Mosior=-}
{-=Version: 1.0=-}
{-=Synopsis:  This Haskell Script will take in=-} 
{-=a transformed .vep file and will create a=-}
{-=mgibed file.=-}

{-Imports-}

import Data.List as DL
import Data.List.Extra as DLE
import Data.List.Split as DLS
import Data.Ord as DO
import Data.Tuple as DT
import System.Console.GetOpt as SCG
import System.Directory as SD
import System.Environment as SE
import System.Exit as SX
import System.IO as SIO
import System.IO.Temp as SIOT
import System.Process as SP

{---------} 

{-Custom CML Option Datatype.-}

data Flag
    = Verbose               -- -v
    | Version               -- -V -?
    | InFormat String       -- -I
    | OutFormat String      -- -O
    | OutputFile String     -- -o 
    | GzipIn                -- -g
    | GzipOut               -- -G
    | Help                  -- --help
    deriving (Eq,Ord,Show)

{-----------------------------}

{-Option Description function relating to datatype above.-}

--options -> This function will
--describe flags.
options :: [OptDescr Flag]
options =
    [ Option ['v']     ["verbose"]             (NoArg Verbose)                "Output on stderr.",
      Option ['V','?'] ["version"]             (NoArg Version)                "Show version number.",
      Option ['I']     ["InFormat"]            (ReqArg InFormat "IN")         "The format of the input file.",
      Option ['O']     ["OutFormat"]           (ReqArg OutFormat "OUT")       "The format of the output file.",
      Option ['o']     ["OutputFile"]          (ReqArg OutputFile "OUTFILE")  "The output file.", 
      Option ['g']     ["GzipIn"]              (NoArg GzipIn)                 "Gzipped input file?",
      Option ['G']     ["GzipOut"]             (NoArg GzipOut)                "Gzipped output file?",
      Option []        ["help"]                (NoArg Help)                   "Print this help message."
    ]

{---------------------------------------------------------}

{-Custom bool functions for Flag Datatype.-}

--isOutputFile -> This function will
--test for OutputFile flag.
isOutputFile :: Flag -> Bool
isOutputFile (OutputFile _) = True
isOutputFile _              = False

--isInFormat -> This function will
--test for InFormat flag.
isInFormat :: Flag -> Bool
isInFormat (InFormat _) = True
isInFormat _            = False

--IsOutFormat -> This function will
--test for OutFormat flag.
isOutFormat :: Flag -> Bool
isOutFormat (OutFormat _) = True
isOutFormat _             = False

--isGzipOut -> This function will
--test for the GzipOut flag.
isGzipOut :: Flag -> Bool
isGzipOut GzipOut = True
isGzipOut _       = False

{------------------------------------------}

{-Custom extraction functions for Flag Datatype.-}

--extractOutputFile -> This function will
--extract the string associated with 
--OutputFile.
extractOutputFile :: Flag -> String
extractOutputFile (OutputFile x) = x

--extractInFormat -> This function will
--extract the string associated with
--InFormat.
extractInFormat :: Flag -> String
extractInFormat (InFormat x) = x

--extractOutFormat -> This function will
--extract the string associated with
--OutFormat.
extractOutFormat :: Flag -> String
extractOutFormat (OutFormat x) = x

{------------------------------------------------}

{-compilerOpts-related functions-}

--checkInFormat -> This function will
--check the format of IN string.
checkInFormat :: String -> Bool
checkInFormat [] = False
checkInFormat xs = if xs == "vcf" || xs == "vep"
                   || xs == "tvcf" || xs == "tvep"
                       then True
                       else False

--checkOutFormat -> This function will
--check the format of OUT string.
checkOutFormat :: String -> Bool
checkOutFormat [] = False
checkOutFormat xs = if xs == "mgibed"
                        then True
                        else False

--checkInOutFormats -> This function will
--check the formats of the IN and OUT string.
checkInOutFormats :: String -> String -> Bool
checkInOutFormats [] [] = False
checkInOutFormats [] _  = False
checkInOutFormats _  [] = False
checkInOutFormats xs ys = if (xs == "vep" && ys == "mgibed")
                          || (xs == "tvep" && ys == "mgibed") 
                          || (xs == "vcf" && ys == "mgibed")
                          || (xs == "tvcf" && ys == "mgibed")
                              then True 
                              else False

{--------------------}

{-Function to correctly parse the flags.-}

--compilerOpts -> This function will
--parse incoming command line arguments.
compilerOpts :: [String] -> IO ([Flag],String)
compilerOpts argv =
    case getOpt Permute options argv of
        (args,file,[]) ->
            if DL.elem Help args
                then do hPutStrLn stderr (greeting ++ SCG.usageInfo header options)
                        SX.exitWith SX.ExitSuccess
                else if DL.elem Version args
                    then do hPutStrLn stderr (version ++ SCG.usageInfo header options)
                            SX.exitWith SX.ExitSuccess
                    else if (DL.length (DL.filter (isInFormat) args) < 1)
                        then do hPutStrLn stderr (inerror ++ formats ++ SCG.usageInfo header options)
                                SX.exitWith (SX.ExitFailure 1)
                        else if (DL.length (DL.filter (isInFormat) args) > 0) &&
                                (not (checkInFormat (extractInFormat (DL.head (DL.filter (isInFormat) args)))))
                            then do hPutStrLn stderr (inferror ++ formats ++ SCG.usageInfo header options)
                                    SX.exitWith (SX.ExitFailure 1)
                            else if (DL.length (DL.filter (isOutFormat) args) < 1)
                                then do hPutStrLn stderr (outerror ++ formats ++ SCG.usageInfo header options)
                                        SX.exitWith (SX.ExitFailure 1)
                                else if (DL.length (DL.filter (isOutFormat) args) > 0) &&
                                        (not (checkOutFormat (extractOutFormat (DL.head (DL.filter (isOutFormat) args)))))
                                    then do hPutStrLn stderr (outferror ++ formats ++ SCG.usageInfo header options)
                                            SX.exitWith (SX.ExitFailure 1)
                                    else if (DL.length (DL.filter (isInFormat) args) < 1) &&
                                            (DL.length (DL.filter (isOutFormat) args) < 1)
                                        then do hPutStrLn stderr (inerror ++ outerror ++ SCG.usageInfo header options) 
                                                SX.exitWith (SX.ExitFailure 1)
                                        else if (DL.length (DL.filter (isInFormat) args) > 0) &&
                                                (DL.length (DL.filter (isOutFormat) args) > 0) &&
                                                (not (checkInOutFormats (extractInFormat (DL.head (DL.filter (isInFormat) args))) 
                                                                        (extractOutFormat (DL.head (DL.filter (isOutFormat) args)))))
                                            then do hPutStrLn stderr (inoutmismatch ++ inoutmappings ++ SCG.usageInfo header options)
                                                    SX.exitWith (SX.ExitFailure 1)
                                            else if (DL.length (DL.filter (isGzipOut) args) > 0) &&
                                                    (DL.length (DL.filter (isOutputFile) args) < 1) 
                                                then do hPutStrLn stderr (gziperror ++ SCG.usageInfo header options)
                                                        SX.exitWith (ExitFailure 1)
                                                else if DL.length file > 1
                                                    then do hPutStrLn stderr (flerror ++ greeting ++ github ++ SCG.usageInfo header options)
                                                            SX.exitWith (SX.ExitFailure 1)
                                                    else return (DL.nub args, DL.concat file)
        (_,_,errors) -> do
            hPutStrLn stderr (DL.concat errors ++ SCG.usageInfo header options)
            SX.exitWith (SX.ExitFailure 1)
        where
            greeting       = "Variant to bam-readcount, Copyright (c) 2019 Matthew Mosior.\n"
            header         = "Usage: vtbr [-vV?IoOgG] [file]"
            version        = "Variant to bam-readcount (VTB), Version 1.0.\n"
            github         = "Please see https://github.com/Matthew-Mosior/Variant-to-bam-readcount.\n"
            flerror        = "Incorrect number of input files:  Please provide one input file.\n" 
            inerror        = "Please provide an input format (-I).\n"
            outerror       = "Please provide an output format (-O).\n"
            inferror       = "Input format not recognized.\n" 
            outferror      = "Output format not recognized.\n"
            gziperror      = "OutputFile argument (-o) necessary to use GzipOut argument (-G).\n"
            formats        = "Accepted input formats are vcf, vep, tvcf and tvep.\nThe accepted output format is mgibed.\n"
            inoutmismatch  = "Please provide an appropriate input/output mapping.\n"
            inoutmappings  = "Appropriate mappings are: vep -> mgibed, vcf -> mgibed, tvep -> mgibed and tvcf -> mgibed.\n"

{----------------------------------------}

{-Vep or vcf pipeline function.-}

--mgiBedPipeline -> This function will
--help decide which pipeline the script
--will follow.
mgiBedPipeline :: [Flag] -> (String,String)
mgiBedPipeline []      = ([],[])
mgiBedPipeline options = if instring == "vep" && outstring == "mgibed"
                               then ("vep","mgibed")
                               else if instring == "tvep" && outstring == "mgibed"
                                   then ("tvep","mgibed")
                                   else if instring == "vcf" && outstring == "mgibed"
                                       then ("vcf","mgibed")
                                       else ("tvcf","mgibed")
    where
        --Local definitions.--
        instring  = extractInFormat (DL.head (DL.filter (isInFormat) options))
        outstring = extractOutFormat (DL.head (DL.filter (isOutFormat) options))   
        ----------------------  

{-------------------------------}

{-Shared General Utility Functions.-}

--lineFeed -> This function will
--read the file in and split on
--whitespace, returning a list
--of lists.
lineFeed :: String -> [[String]]
lineFeed [] = []
lineFeed xs = DL.map DL.words (DL.lines xs)

--mapNotLast -> This function will
--work like the traditional map
--function in Data.List, but not
--map to the last element of a list.
mapNotLast :: (a -> a) -> [a] -> [a]
mapNotLast fn []     = []
mapNotLast fn [x]    = [x]
mapNotLast fn (x:xs) = fn x : mapNotLast fn xs

--tuplifyTwo -> This function will
--turn a list of two elements into
--a two-tuple.
tuplifyTwo :: [a] -> (a,a)
tuplifyTwo [x,y] = (x,y)

--tuplifyThree -> This function will
--turn a list of three elements into
--a triplet.
tuplifyThree :: [a] -> (a,a,a)
tuplifyThree [x,y,z] = (x,y,z)

--singleunnest -> This function will
--unnest a list.
singleunnest :: [a] -> a
singleunnest [a] = a

{-----------------------------------}

{-General Utility Functions (vep/tvep).-}

--onlyDataVepBool -> This function will
--return True for only lines of the 
--file that contain tab-delimited 
--information.
onlyDataVepBool :: [String] -> Bool
onlyDataVepBool xs = not (DL.head xs == "##"
                       || DL.head xs == "#Uploaded_variation")

--onlyDataTvepBool -> This function will
--return True for only lines of the
--file that contain tab-delimited
--information.
onlyDataTvepBool :: [String] -> Bool
onlyDataTvepBool xs = not (DL.head xs == "##"
       || DL.any (\x -> x == "#Uploaded_variation") xs)

--onlyPoundSignBool -> This function will
--return True for only lines of the 
--file that contains the initial 
--header lines.
onlyPoundSignBool :: [String] -> Bool
onlyPoundSignBool xs = DL.head xs == "##"
                    || DL.head xs == "#"

--onlyDataVepGrabber -> This function will 
--grab only lines of the file that 
--contain tab-delimited information.
onlyDataVepGrabber :: [[String]] -> [[String]]
onlyDataVepGrabber [] = []
onlyDataVepGrabber xs = DL.filter (onlyDataVepBool) xs

--onlyDataTvepGrabber -> This function will
--grab only lines of the file that
--contain tab-delimited information.
onlyDataTvepGrabber :: [[String]] -> [[String]]
onlyDataTvepGrabber [] = []
onlyDataTvepGrabber xs = DL.filter (onlyDataTvepBool) xs

--onlyPoundSignGrabber -> This function will
--grab only lines of file that contain
--the initial header lines.
onlyPoundSignGrabber :: [[String]] -> [[String]]
onlyPoundSignGrabber [] = []
onlyPoundSignGrabber xs = DL.filter (onlyPoundSignBool) xs

--parseUploadedVariation -> This function will
--parse the #Uploaded variation field 
--(field containing information needed).
parseUploadedVariation :: [String] -> [(String,Int,(String,String))]
parseUploadedVariation []     = []
parseUploadedVariation (x:xs) = [(\(a,b,c) -> (a,read b,tuplifyTwo (DLS.splitOn "/" c))) 
                                 (tuplifyThree (DLS.splitOn "_" x))] ++ (parseUploadedVariation xs) 

--bamReadcountFormatVep -> This function will
--decipher the appropriate fields for a typical
--bam-readcount input.
bamReadcountFormatVep :: [(String,Int,(String,String))] -> [(String,String,String,String,String)]
bamReadcountFormatVep [] = []
bamReadcountFormatVep (x:xs) = [smallBamReadcountFormatVep x] ++ (bamReadcountFormatVep xs)
    where
        --Nested function definitions.--
        --smallBamReadcountFormatVep
        smallBamReadcountFormatVep :: (String,Int,(String,String)) -> (String,String,String,String,String)
        smallBamReadcountFormatVep (a,b,(c,d)) = (a,
                                                  show ((\(aa,bb) -> aa) (detectRefVsAltVep b c d)),
                                                  show ((\(aa,bb) -> bb) (detectRefVsAltVep b c d)),
                                                  c,
                                                  d) 
        --detectRefVsAltVep
        detectRefVsAltVep :: Int -> String -> String -> (Int,Int)
        detectRefVsAltVep a b c = --Insertion.
                                  if b == "-"
                                      then (a - 1,a)
                                      --Deletion.
                                      else if c == "-" 
                                          then (a,a + (DL.length b) - 1)
                                          --Default.
                                          else (a,a)
        --------------------------------

{----------------------------}

{-General utility functions (vcf/tvcf).-}

--onlyInfoBool -> This function will
--return True for only lines that 
--contain the "##INFO" fields.
onlyInfoBool :: [String] -> Bool
onlyInfoBool xs = DL.isInfixOf "##INFO" (DL.head xs) 

--onlyInfoGrabber -> This function will
--grab only lines of file that contain
--the initial header lines.
onlyInfoGrabber :: [[String]] -> [[String]]
onlyInfoGrabber [] = []
onlyInfoGrabber xs = DL.filter (onlyInfoBool) xs

--onlyMetadataBool -> This function will
--return true for only lines that 
--metadata.
onlyMetadataBool :: String -> Bool
onlyMetadataBool xs = DL.isPrefixOf "##" xs

--onlyMetadataGrabber -> This function will
--grab only lines of the file that contain
--all header lines.
onlyMetadataGrabber :: [String] -> [String]
onlyMetadataGrabber [] = []
onlyMetadataGrabber xs = DL.filter (onlyMetadataBool) xs

--onlyDataVcfBool -> This function will
--return True for only lines of the
--file that contain tab-delimited
--information.
onlyDataVcfBool :: [String] -> Bool
onlyDataVcfBool xs = not (DL.any (\x -> DL.isPrefixOf "##" x) xs || DL.any (\x -> DL.isPrefixOf "#" x) xs)

--onlyDataVcfGrabber -> This function will
--grab only lines of the file that
--contain tab-delimited information.
onlyDataVcfGrabber :: [[String]] -> [[String]]
onlyDataVcfGrabber [] = []
onlyDataVcfGrabber xs = DL.filter (onlyDataVcfBool) xs

--bamReadcountFormatVcf -> This function will
--decipher the appropriate fields for a typical
--bam-readcount input.
bamReadcountFormatVcf :: [(String,Int,(String,String))] -> [(String,String,String,String,String)]
bamReadcountFormatVcf [] = []
bamReadcountFormatVcf (x:xs) = [smallBamReadcountFormatVcf x] ++ (bamReadcountFormatVcf xs)
    where
        --Nested function definitions.--
        --smallBamReadcountFormatVcf
        smallBamReadcountFormatVcf :: (String,Int,(String,String)) -> (String,String,String,String,String)
        smallBamReadcountFormatVcf (a,b,(c,d)) = (a,
                                                  show ((\(aa,bb,cc,dd) -> aa) (detectRefVsAltVcf b c d)),
                                                  show ((\(aa,bb,cc,dd) -> bb) (detectRefVsAltVcf b c d)),
                                                  (\(aa,bb,cc,dd) -> cc) (detectRefVsAltVcf b c d),
                                                  (\(aa,bb,cc,dd) -> dd) (detectRefVsAltVcf b c d))
        --detectRefVsAltVcf
        detectRefVsAltVcf :: Int -> String -> String -> (Int,Int,String,String)
        detectRefVsAltVcf a b c = --Insertion.
                                  if DL.length b < DL.length c 
                                      then (a,a + 1,"-",DL.tail c)
                                      --Deletion.
                                      else if DL.length b > DL.length c
                                          then (a + 1,a + (DL.length (b DL.\\ c)),DL.tail b,"-")
                                          --Default.
                                          else (a,a,b,c)

{----------------------------------}

{-Printing functions.-}

--tempFileCreation -> This function will
--print the file to stdout using
--readProcess of the unix tool cat.
catFile :: [[String]] -> IO ()
catFile [] = return ()
catFile xs = do
    --Open a temporary file.
    (tempfile,temph) <- SIOT.openTempFile "." "temp.txt"
    --mapNotLast newlines in xs.
    let intercalatedxs = DL.concat (DL.concat (mapNotLast (++ ["\n"]) xs))
    --Add intercalatedxs to temp.txt.
    hPutStrLn temph intercalatedxs
    --Close the temporary file's handle.
    hClose temph
    --Print out the contents of tempfile to the screen using cat unix tool.
    (_,_,_,ph) <- SP.createProcess (SP.proc "cat" [tempfile])
    ec <- SP.waitForProcess ph
    case ec of
        SX.ExitSuccess   -> do SP.readProcess "rm" [tempfile] []
                               return ()
        SX.ExitFailure _ -> do error "Could not cat file."
                               SP.readProcess "rm" [tempfile] []
                               return ()

--noGzipPrintFile -> This function will
--print the file, not gzipped.
noGzipPrintFile :: [Flag] -> [[String]] -> IO ()
noGzipPrintFile [] [] = return ()
noGzipPrintFile [] _  = return ()
noGzipPrintFile _  [] = return ()
noGzipPrintFile opts xs = do
    --Grab just "OUTFILE".
    let outfile = DL.head (DL.filter (isOutputFile) opts)
    --Extract the string from FilterFields.
    let outfilestring = extractOutputFile outfile
    --mapNotLast newlines in xs.
    let intercalatedxs = DL.concat (DL.concat (mapNotLast (++ ["\n"]) xs))
    --Write the output to the user-specified filename.
    SIO.writeFile outfilestring intercalatedxs
               
--gzipPrintFile -> This function will
--will print the file, but gzipped.
gzipPrintFile :: [Flag] -> [[String]] -> IO String
gzipPrintFile [] [] = return []
gzipPrintFile [] _  = return []
gzipPrintFile _  [] = return []
gzipPrintFile opts xs = do
    --Grab just "OUTFILE".
    let outfile = DL.head (DL.filter (isOutputFile) opts)
    --Extract the string from FilterFields.
    let outfilestring = extractOutputFile outfile
    --mapNotLast newlines in xs.
    let intercalatedxs = DL.concat (DL.concat (mapNotLast (++ ["\n"]) xs))
    --Write the output to the user-specified filename.
    SIO.writeFile outfilestring intercalatedxs
    --Gzip outfile.
    SP.readProcess "gzip" [outfilestring] []

 
{---------------------}

{-VTB Specific Functions.-}

--processArgsAndFilesVepMgibed -> This function will
--walk through each of the command-line
--arguments and files provided by the user.
processArgsAndFilesVepMgibed :: ([Flag],String) -> IO ()
processArgsAndFilesVepMgibed ([],[]) = return ()
processArgsAndFilesVepMgibed (options,inputfile) = do
    --Check to see if inputfile is gzip compressed.
    if DL.elem GzipIn options
        then do --Decompress the file.
                SP.readProcess "gunzip" [inputfile] []
                --Read the decompressed file.
                gunzippedfile <- SIO.readFile (DLE.dropSuffix ".gz" inputfile)
                --Apply lineFeed function to gunzippedfile.
                let gprocessedfile = lineFeed gunzippedfile
                --Grab only data lines of gprocessedfile.
                let gdataonly = onlyDataVepGrabber gprocessedfile
                --Grab only the #Uploaded_variation column from gdataonly.
                let guploadedvariation = DL.map (DL.head) gdataonly
                --Keep only elements that start with "chr" in guploadedvariation.
                let gchronly = DL.filter (\x -> DL.isPrefixOf "chr" x) guploadedvariation
                --Begin to parse gchronly.
                let gparsedchronly = parseUploadedVariation gchronly
                --Put gparsedchronly into bam-readcount input format.
                let ginitialbamreadcount = bamReadcountFormatVep gparsedchronly
                --Turn ginitialbamreadcount into a list of lists.
                let gfinalbamreadcount = DL.map (\(a,b,c,d,e) -> [a,b,c,d,e]) ginitialbamreadcount
                --mapNotLast tabs to gfinalbamreadcount.
                let gprintbamreadcount = DL.map (mapNotLast (++ "\t")) gfinalbamreadcount
                --Print the file to stdout (cat) or to a file.
                if DL.length (DL.filter (isOutputFile) options) > 0
                    --Check to see if outfile is to be gzipped.
                    then if DL.elem GzipOut options
                        then do
                            _ <- gzipPrintFile options gprintbamreadcount
                            return ()
                        else noGzipPrintFile options gprintbamreadcount
                else catFile gprintbamreadcount
 
        else do --Read in the file.
                readinputfile <- SIO.readFile inputfile
                --Apply lineFeed function to inputfile.
                let processedfile = lineFeed readinputfile
                --Grab only data lines of processedfile.
                let dataonly = onlyDataVepGrabber processedfile
                --Grab only the #Uploaded_variation column from dataonly.
                let uploadedvariation = DL.map (DL.head) dataonly
                --Keep only elements that start with "chr" in uploadedvariation.
                let chronly = DL.filter (\x -> DL.isPrefixOf "chr" x) uploadedvariation
                --Begin to parse chronly.
                let parsedchronly = parseUploadedVariation chronly
                --Put parsedchronly into bam-readcount input format.
                let initialbamreadcount = bamReadcountFormatVep parsedchronly
                --Turn initialbamreadcount into a list of lists.
                let finalbamreadcount = DL.map (\(a,b,c,d,e) -> [a,b,c,d,e]) initialbamreadcount
                --mapNotLast tabs to finalbamreadcount.
                let printbamreadcount = DL.map (mapNotLast (++ "\t")) finalbamreadcount
                --Print the file to stdout (cat) or to a file.
                if DL.length (DL.filter (isOutputFile) options) > 0
                    --Check to see if outfile is to be gzipped.
                    then if DL.elem GzipOut options
                        then do
                            _ <- gzipPrintFile options printbamreadcount
                            return ()
                        else noGzipPrintFile options printbamreadcount
                else catFile printbamreadcount

--processArgsAndContentsVepMgibed -> This function will
--walk through each of the command-line
--arguments and files provided by the user.
processArgsAndContentsVepMgibed :: ([Flag],String) -> IO ()
processArgsAndContentsVepMgibed ([],[]) = return ()
processArgsAndContentsVepMgibed (options,content) = do
    --Apply lineFeed function to content.
    let processedfile = lineFeed content
    --Grab only data lines of processedfile.
    let dataonly = onlyDataVepGrabber processedfile
    --Grab only the #Uploaded_variation column from dataonly.
    let uploadedvariation = DL.map (DL.head) dataonly
    --Keep only elements that start with "chr" in uploadedvariation.
    let chronly = DL.filter (\x -> DL.isPrefixOf "chr" x) uploadedvariation
    --Begin to parse chronly.
    let parsedchronly = parseUploadedVariation chronly
    --Put parsedchronly into bam-readcount input format.
    let initialbamreadcount = bamReadcountFormatVep parsedchronly
    --Turn initialbamreadcount into a list of lists.
    let finalbamreadcount = DL.map (\(a,b,c,d,e) -> [a,b,c,d,e]) initialbamreadcount
    --mapNotLast tabs to finalbamreadcount.
    let printbamreadcount = DL.map (mapNotLast (++ "\t")) finalbamreadcount
    --Print the file to stdout (cat) or to a file.
    if DL.length (DL.filter (isOutputFile) options) > 0
        --Check to see if outfile is to be gzipped.
        then if DL.elem GzipOut options
            then do
                _ <- gzipPrintFile options printbamreadcount
                return ()
            else noGzipPrintFile options printbamreadcount
    else catFile printbamreadcount

--processArgsAndFilesTvepMgibed -> This function will
--walk through each of the command-line
--arguments and files provided by the user.
processArgsAndFilesTvepMgibed :: ([Flag],String) -> IO ()
processArgsAndFilesTvepMgibed ([],[]) = return ()
processArgsAndFilesTvepMgibed (options,inputfile) = do
    --Check to see if inputfile is gzip compressed.
    if DL.elem GzipIn options
        then do --Decompress the file.
                SP.readProcess "gunzip" [inputfile] []
                --Read the decompressed file.
                gunzippedfile <- SIO.readFile (DLE.dropSuffix ".gz" inputfile)
                --Apply lineFeed function to gunzippedfile.
                let gprocessedfile = lineFeed gunzippedfile
                --Grab the index of the #Uploaded_variation column.
                let guploadedvariationindex = singleunnest (DL.nub (DL.concat (DL.map (\x -> DL.elemIndices "#Uploaded_variation" x) gprocessedfile)))
                --Grab only data lines of gprocessedfile.
                let gdataonly = onlyDataTvepGrabber gprocessedfile
                --Grab only the #Uploaded_variation column from gdataonly.
                let guploadedvariation = DL.map (\x -> x DL.!! (guploadedvariationindex)) gdataonly
                --Keep only elements that start with "chr" in guploadedvariation.
                let gchronly = DL.filter (\x -> DL.isPrefixOf "chr" x) guploadedvariation
                --Begin to parse gchronly.
                let gparsedchronly = parseUploadedVariation gchronly
                --Put gparsedchronly into bam-readcount input format.
                let ginitialbamreadcount = bamReadcountFormatVep gparsedchronly
                --Turn ginitialbamreadcount into a list of lists.
                let gfinalbamreadcount = DL.map (\(a,b,c,d,e) -> [a,b,c,d,e]) ginitialbamreadcount
                --mapNotLast tabs to gfinalbamreadcount.
                let gprintbamreadcount = DL.map (mapNotLast (++ "\t")) gfinalbamreadcount
                --Print the file to stdout (cat) or to a file.
                if DL.length (DL.filter (isOutputFile) options) > 0
                    --Check to see if outfile is to be gzipped.
                    then if DL.elem GzipOut options
                        then do
                            _ <- gzipPrintFile options gprintbamreadcount
                            return ()
                        else noGzipPrintFile options gprintbamreadcount
                else catFile gprintbamreadcount                

        else do --Read in the file.
                readinputfile <- SIO.readFile inputfile
                --Apply lineFeed function to inputfile.
                let processedfile = lineFeed readinputfile
                --Grab the index of the #Uploaded_variation column.
                let uploadedvariationindex = singleunnest (DL.nub (DL.concat (DL.map (\x -> DL.elemIndices "#Uploaded_variation" x) processedfile)))
                --Grab only data lines of processedfile.
                let dataonly = onlyDataTvepGrabber processedfile
                --Grab only the #Uploaded_variation column from dataonly.
                let uploadedvariation = DL.map (\x -> x DL.!! (uploadedvariationindex)) dataonly
                --Keep only elements that start with "chr" in uploadedvariation.
                let chronly = DL.filter (\x -> DL.isPrefixOf "chr" x) uploadedvariation
                --Begin to parse chronly.
                let parsedchronly = parseUploadedVariation chronly
                --Put parsedchronly into bam-readcount input format.
                let initialbamreadcount = bamReadcountFormatVep parsedchronly
                --Turn initialbamreadcount into a list of lists.
                let finalbamreadcount = DL.map (\(a,b,c,d,e) -> [a,b,c,d,e]) initialbamreadcount
                --mapNotLast tabs to finalbamreadcount.
                let printbamreadcount = DL.map (mapNotLast (++ "\t")) finalbamreadcount
                --Print the file to stdout (cat) or to a file.
                if DL.length (DL.filter (isOutputFile) options) > 0
                    --Check to see if outfile is to be gzipped.
                    then if DL.elem GzipOut options
                        then do
                            _ <- gzipPrintFile options printbamreadcount
                            return ()
                        else noGzipPrintFile options printbamreadcount
                else catFile printbamreadcount
           
--processArgsAndContentsTvepMgibed -> This function will
--walk through each of the command-line
--arguments and files provided by the user.
processArgsAndContentsTvepMgibed :: ([Flag],String) -> IO ()
processArgsAndContentsTvepMgibed ([],[]) = return ()
processArgsAndContentsTvepMgibed (options,content) = do
    --Apply lineFeed function to content.
    let processedfile = lineFeed content
    --Grab the index of the #Uploaded_variation column.
    let uploadedvariationindex = singleunnest (DL.nub (DL.concat (DL.map (\x -> DL.elemIndices "#Uploaded_variation" x) processedfile)))
    --Grab only data lines of processedfile.
    let dataonly = onlyDataTvepGrabber processedfile
    --Grab only the #Uploaded_variation column from dataonly.
    let uploadedvariation = DL.map (\x -> x DL.!! (uploadedvariationindex)) dataonly
    --Keep only elements that start with "chr" in uploadedvariation.
    let chronly = DL.filter (\x -> DL.isPrefixOf "chr" x) uploadedvariation
    --Begin to parse chronly.
    let parsedchronly = parseUploadedVariation chronly
    --Put parsedchronly into bam-readcount input format.
    let initialbamreadcount = bamReadcountFormatVep parsedchronly
    --Turn initialbamreadcount into a list of lists.
    let finalbamreadcount = DL.map (\(a,b,c,d,e) -> [a,b,c,d,e]) initialbamreadcount
    --mapNotLast tabs to finalbamreadcount.
    let printbamreadcount = DL.map (mapNotLast (++ "\t")) finalbamreadcount
    --Print the file to stdout (cat) or to a file.
    if DL.length (DL.filter (isOutputFile) options) > 0
        --Check to see if outfile is to be gzipped.
        then if DL.elem GzipOut options
            then do
                _ <- gzipPrintFile options printbamreadcount
                return ()
            else noGzipPrintFile options printbamreadcount
    else catFile printbamreadcount

--processArgsAndFilesVcfMgibed -> This function will
--walk through each of the command-line
--arguments and files provided by the user.
processArgsAndFilesVcfMgibed :: ([Flag],String) -> IO ()
processArgsAndFilesVcfMgibed ([],[]) = return ()
processArgsAndFilesVcfMgibed (options,inputfile) = do
    --Check to see if inputfile is gzip compressed.
    if DL.elem GzipIn options
        then do --Decompress the file.
                SP.readProcess "gunzip" [inputfile] []
                --Read the decompressed file.
                gunzippedfile <- SIO.readFile (DLE.dropSuffix ".gz" inputfile)
                --Apply lines to gunzippedfile.
                let gprocessedfile = lineFeed gunzippedfile
                --Grab only the data lines in gprocessedfile.
                let gdataonly = onlyDataVcfGrabber gprocessedfile 
                --Grab only chr, pos, ref and alt columns of gdataonly.
                let gparseddataonly = DL.map (\x -> (x DL.!! 0,read (x DL.!! 1) :: Int,(x DL.!! 3,x DL.!! 4))) gdataonly
                --Put gparseddataonly into bam-readcount input format.
                let ginitialbamreadcount = bamReadcountFormatVcf gparseddataonly
                --Turn ginitialbamreadcount into a list of lists.
                let gfinalbamreadcount = DL.map (\(a,b,c,d,e) -> [a,b,c,d,e]) ginitialbamreadcount
                --mapNotLast tabs to gfinalbamreadcount.
                let gprintbamreadcount = DL.map (mapNotLast (++ "\t")) gfinalbamreadcount
                --Print the file to stdout (cat) or to a file.
                if DL.length (DL.filter (isOutputFile) options) > 0
                    --Check to see if outfile is to be gzipped.
                    then if DL.elem GzipOut options
                        then do
                            _ <- gzipPrintFile options gprintbamreadcount
                            return ()
                        else noGzipPrintFile options gprintbamreadcount
                else catFile gprintbamreadcount

        else do --Read in the file.
                readinputfile <- SIO.readFile inputfile
                --Apply lines to readinputfile.
                let processedfile = lineFeed readinputfile
                --Grab only the data lines in processedfile.
                let dataonly = onlyDataVcfGrabber processedfile
                --Grab only chr, pos, ref and alt columns of dataonly.
                let parseddataonly = DL.map (\x -> (x DL.!! 0,read (x DL.!! 1) :: Int,(x DL.!! 3,x DL.!! 4))) dataonly
                --Put parseddataonly into bam-readcount input format.
                let initialbamreadcount = bamReadcountFormatVcf parseddataonly
                --Turn initialbamreadcount into a list of lists.
                let finalbamreadcount = DL.map (\(a,b,c,d,e) -> [a,b,c,d,e]) initialbamreadcount
                --mapNotLast tabs to finalbamreadcount.
                let printbamreadcount = DL.map (mapNotLast (++ "\t")) finalbamreadcount
                --Print the file to stdout (cat) or to a file.
                if DL.length (DL.filter (isOutputFile) options) > 0
                    --Check to see if outfile is to be gzipped.
                    then if DL.elem GzipOut options
                        then do
                            _ <- gzipPrintFile options printbamreadcount
                            return ()
                        else noGzipPrintFile options printbamreadcount
                else catFile printbamreadcount

--processArgsAndContentsVcfMgibed -> This function will
--walk through each of the command-line
--arguments and files provided by the user.
processArgsAndContentsVcfMgibed :: ([Flag],String) -> IO ()
processArgsAndContentsVcfMgibed ([],[]) = return ()
processArgsAndContentsVcfMgibed (options,content) = do
    --Apply lineFeed function to content.
    let processedfile = lineFeed content
    --Grab only the data lines in processedfile.
    let dataonly = onlyDataVcfGrabber processedfile
    --Grab only chr, pos, ref and alt columns of dataonly.
    let parseddataonly = DL.map (\x -> (x DL.!! 0,read (x DL.!! 1) :: Int,(x DL.!! 3,x DL.!! 4))) dataonly
    --Put parseddataonly into bam-readcount input format.
    let initialbamreadcount = bamReadcountFormatVcf parseddataonly
    --Turn initialbamreadcount into a list of lists.
    let finalbamreadcount = DL.map (\(a,b,c,d,e) -> [a,b,c,d,e]) initialbamreadcount
    --mapNotLast tabs to finalbamreadcount.
    let printbamreadcount = DL.map (mapNotLast (++ "\t")) finalbamreadcount
    --Print the file to stdout (cat) or to a file.
    if DL.length (DL.filter (isOutputFile) options) > 0
        --Check to see if outfile is to be gzipped.
        then if DL.elem GzipOut options
            then do
                _ <- gzipPrintFile options printbamreadcount
                return ()
            else noGzipPrintFile options printbamreadcount
    else catFile printbamreadcount

--processArgsAndFilesTvcfMgibed -> This function will
--walk through each of the command-line
--arguments and files provided by the user.
processArgsAndFilesTvcfMgibed :: ([Flag],String) -> IO ()
processArgsAndFilesTvcfMgibed ([],[]) = return ()
processArgsAndFilesTvcfMgibed (options,inputfile) = do
    --Check to see if inputfile is gzip compressed.
    if DL.elem GzipIn options
        then do --Decompress the file.
                SP.readProcess "gunzip" [inputfile] []
                --Read the decompressed file.
                gunzippedfile <- SIO.readFile (DLE.dropSuffix ".gz" inputfile)
                --Apply lines to gunzippedfile.
                let gprocessedfile = lineFeed gunzippedfile
                --Grab only the data lines in gprocessedfile.
                let gdataonly = onlyDataVcfGrabber gprocessedfile
                --Grab only chr, pos, ref and alt columns of gdataonly.
                let gparseddataonly = DL.map (\x -> (x DL.!! 0,read (x DL.!! 1) :: Int,(x DL.!! 3,x DL.!! 4))) gdataonly
                --Put gparseddataonly into bam-readcount input format.
                let ginitialbamreadcount = bamReadcountFormatVcf gparseddataonly
                --Turn ginitialbamreadcount into a list of lists.
                let gfinalbamreadcount = DL.map (\(a,b,c,d,e) -> [a,b,c,d,e]) ginitialbamreadcount
                --mapNotLast tabs to gfinalbamreadcount.
                let gprintbamreadcount = DL.map (mapNotLast (++ "\t")) gfinalbamreadcount
                --Print the file to stdout (cat) or to a file.
                if DL.length (DL.filter (isOutputFile) options) > 0
                    --Check to see if outfile is to be gzipped.
                    then if DL.elem GzipOut options
                        then do
                            _ <- gzipPrintFile options gprintbamreadcount
                            return ()
                        else noGzipPrintFile options gprintbamreadcount
                else catFile gprintbamreadcount

        else do --Read in the file.
                readinputfile <- SIO.readFile inputfile
                --Apply lineFeed function to inputfile.
                let processedfile = lineFeed readinputfile
                --Grab only the data lines in processedfile.
                let dataonly = onlyDataVcfGrabber processedfile
                --Grab only chr, pos, ref and alt columns of dataonly.
                let parseddataonly = DL.map (\x -> (x DL.!! 0,read (x DL.!! 1) :: Int,(x DL.!! 3,x DL.!! 4))) dataonly
                --Put parseddataonly into bam-readcount input format.
                let initialbamreadcount = bamReadcountFormatVcf parseddataonly
                --Turn initialbamreadcount into a list of lists.
                let finalbamreadcount = DL.map (\(a,b,c,d,e) -> [a,b,c,d,e]) initialbamreadcount
                --mapNotLast tabs to finalbamreadcount.
                let printbamreadcount = DL.map (mapNotLast (++ "\t")) finalbamreadcount
                --Print the file to stdout (cat) or to a file.
                if DL.length (DL.filter (isOutputFile) options) > 0
                    --Check to see if outfile is to be gzipped.
                    then if DL.elem GzipOut options
                        then do
                            _ <- gzipPrintFile options finalbamreadcount
                            return ()
                        else noGzipPrintFile options finalbamreadcount
                else catFile finalbamreadcount
 
--processArgsAndContentsTvcfMgibed -> This function will
--walk through each of the command-line
--arguments and files provided by the user.
processArgsAndContentsTvcfMgibed :: ([Flag],String) -> IO ()
processArgsAndContentsTvcfMgibed ([],[]) = return ()
processArgsAndContentsTvcfMgibed (options,content) = do
    --Apply lineFeed function to content.
    let processedfile = lineFeed content
    --Grab only the data lines in processedfile.
    let dataonly = onlyDataVcfGrabber processedfile
    --Grab only chr, pos, ref and alt columns of dataonly.
    let parseddataonly = DL.map (\x -> (x DL.!! 0,read (x DL.!! 1) :: Int,(x DL.!! 3,x DL.!! 4))) dataonly
    --Put parseddataonly into bam-readcount input format.
    let initialbamreadcount = bamReadcountFormatVcf parseddataonly
    --Turn initialbamreadcount into a list of lists.
    let finalbamreadcount = DL.map (\(a,b,c,d,e) -> [a,b,c,d,e]) initialbamreadcount
    --mapNotLast tabs to finalbamreadcount.
    let printbamreadcount = DL.map (mapNotLast (++ "\t")) finalbamreadcount
    --Print the file to stdout (cat) or to a file.
    if DL.length (DL.filter (isOutputFile) options) > 0
        --Check to see if outfile is to be gzipped.
        then if DL.elem GzipOut options
            then do
                _ <- gzipPrintFile options finalbamreadcount
                return ()
            else noGzipPrintFile options finalbamreadcount
    else catFile finalbamreadcount

{-------------------------}

{-Main function.-}

main :: IO ()
main = do
--Get command line arguments.
    (args,files) <- SE.getArgs >>= compilerOpts
    --See if files is null.
    if null files
        then do --Get stdin.
                contents <- SIO.getContents
                --mgibed parsing pipeline?
                if (fst (mgiBedPipeline args) == "vep" && snd (mgiBedPipeline args) == "mgibed") 
                    then do --Run args and contents through processArgsAndContentsVepTvep.
                           processArgsAndContentsVepMgibed (args,contents)
                    else if (fst (mgiBedPipeline args) == "tvep" && snd (mgiBedPipeline args) == "mgibed")
                        then do --Run args and contents through processArgsAndContentsTvepVep.
                                processArgsAndContentsTvepMgibed (args,contents)
                        else if (fst (mgiBedPipeline args) == "vcf" && snd (mgiBedPipeline args) == "mgibed")
                            then do --Run args and contents through processArgsAndContentsVcfTvcf.
                                    processArgsAndContentsVcfMgibed (args,contents)
                            else do --Run args and contents through processArgsAndContentsTvcfVcf.
                                    processArgsAndContentsTvcfMgibed (args,contents) 

        else do --mgibed parsing pipeline?
                if (fst (mgiBedPipeline args) == "vep" && snd (mgiBedPipeline args) == "mgibed")
                    then do --Run args and contents through processArgsAndFilesVepTvep.
                            processArgsAndFilesVepMgibed (args,files)
                    else if (fst (mgiBedPipeline args) == "tvep" && snd (mgiBedPipeline args) == "mgibed")
                        then do --Run args and contents through processArgsAndFilesTvepVep.
                                processArgsAndFilesTvepMgibed (args,files)
                        else if (fst (mgiBedPipeline args) == "vcf" && snd (mgiBedPipeline args) == "mgibed")
                            then do --Run args and contents through processArgsAndFilesVcfTvcf.
                                    processArgsAndFilesVcfMgibed (args,files)
                            else do --Run args and contents through processArgsAndFilesTvcfVcf.
                                    processArgsAndFilesTvcfMgibed (args,files)

{----------------}
