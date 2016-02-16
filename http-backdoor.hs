import Network.Socket
import Control.Concurrent
import Network (withSocketsDo)
import Data.List (isInfixOf)
import Data.List.Split(splitOn)
import System.Environment
import Data.Word
import System.IO
import Data.Char (toUpper,chr,isSpace)
import System.Process
import Data.Tuple
import Control.Exception
import Data.Maybe

main = withSocketsDo$do
	-- | get port number from command line and cast to a Num to be used in SockAddrInet
	args<-getArgs 
	let port = fromIntegral (read $ head args ::Int)

	-- | initialize socket
	sock<-socket AF_INET Stream 0
	setSocketOption sock ReuseAddr 1
	bind sock (SockAddrInet port iNADDR_ANY)
	listen sock 5

	-- | pass socket to infinitely recursive mainLoop function, and pass to
	-- | the finally function which will catch ctrl-c exceptions and runs function
	-- | fun to close the socket safely
	finally (mainLoop sock) (fun sock)
	where
		fun::Socket->IO ()
		fun sock = do
				close sock

mainLoop::Socket->IO ()
mainLoop sock = do
	conn<-accept sock
	-- | fork new connections from multiple clients to make our server concurrent
	forkIO (runConn conn)
	mainLoop sock

runConn :: (Socket, SockAddr) -> IO ()	
runConn (sock, _) = do
	-- | convert socket to a Handle for file reading purposes, and set handle options
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    hSetEncoding hdl utf8

    -- | get clients HTTP request
    s<-hGetLine hdl
    putStrLn s

    -- | construct a response (header + body)
    response<- if (isValidRequest s) then parseRequest s else return("HTTP/1.1 404 Not Found\r\n\r\n")

    -- | send to client through handle and close handle
    hPutStrLn hdl response
    hClose hdl

-- | checks if HTTP request is valid
isValidRequest::String->Bool
isValidRequest s = if (head$words s)=="GET" && isInfixOf "HTTP/1.1" s && isInfixOf "exec" (trim2 s) then True else False

-- | parses Request, and constructs a proper response in which the body
-- | is the output of the system command present in the URL
parseRequest::String->IO String
parseRequest s = do
	l<-cmdOutput
	if trim2 (fst(resourceDir)) == "/exec/" then finalResponse l
	else return ("HTTP/1.1 404 Not Found\r\n\r\n")
	where 
		loc = urlDecode$reverse (drop 9 (reverse(drop 4 s)))
		resourceDir = splitAt (slshloc (tail (fromMaybe "" loc))) (fromMaybe "" loc)
		finalCmd = snd resourceDir
		cmdOutput = parseCommand (finalCmd)
		finalResponse s = do
			let size = length s
			let f = "HTTP/1.1 200 OK\r\nContent-Length: "++show size++"\r\n\r\n"++s
			return f
		slshloc s = (length$head (splitOn "/" s)) + 2

-- | copypasta from rosetta code as an alternative to using a url parsing library
urlDecode :: String -> Maybe String
urlDecode [] = Just []
urlDecode ('%':xs) =
  case xs of
    (a:b:xss) ->
      urlDecode xss
      >>= return . ((chr . read $ "0x" ++ [a,b]) :)
    _ -> Nothing
urlDecode ('+':xs) = urlDecode xs >>= return . (' ' :)
urlDecode (x:xs) = urlDecode xs >>= return . (x :)

-- | calls linux command and returns either the stderr or stdout
parseCommand::String->IO String
parseCommand s = do
	(_,out,err)<-readCreateProcessWithExitCode (shell s) ""
	if length out == 0 then return err else return out

trim2::[Char]->[Char]
trim2 (x:xs) = if x == ' ' then trim2 xs else x:trim2 xs
trim2 [] = []