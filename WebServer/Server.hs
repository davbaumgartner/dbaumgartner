module WebServer.Server (runServer
    			, URLParams
				, POSTParams
				, Render
	) where

import Network 
import qualified Network.Socket as Sock
import qualified Text.Regex as Regex
import Text.Regex.Posix
import System.IO
import System.Locale
import Control.Monad
import Data.Functor
import Data.List.Split
import Data.Maybe
import Data.IORef
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Format
import Data.MIME.Types
import Network.HTTP.Base (urlDecode)

type HTTPHeader = [String]
type Request = String
type Response = String
type RequestURL = String
type RequestMethod = String
type URLParams = [String]
type POSTParams = [(String, String)]
type Render = String
type URLRoute = String
type View = (URLParams -> POSTParams -> Render)
type Route = [(RequestURL, RequestMethod, View)]

tuplify :: [String] -> (String,String)
tuplify [a] = (a,a)
tuplify [x,y] = (x, y)

urlMatch :: RequestURL -> String -> Bool
urlMatch query url = query =~ url :: Bool

getParams :: RequestURL -> URLRoute -> URLParams
getParams query definition = 
	if (dat /= []) then
        head dat
	else []
	where
		dat = maybeToList $ Regex.matchRegex ( Regex.mkRegex definition ) query

renderRoute :: RequestURL -> RequestMethod -> POSTParams -> Route -> Render
renderRoute requrl reqmethod params routes = 
	if length matchingurl >= 1 then do head matchingurl
	else head [f (getParams requrl name) params | (name, method, f) <- routes, "/404" `urlMatch` name]
	where 
		matchingurl = [f (getParams requrl name) params | (name, method, f) <- routes, 
							requrl `urlMatch` name && ((method == reqmethod) || (method == "*"))]


mkResponse :: Render -> URLRoute -> Response
mkResponse content query_url =
        "HTTP/1.1 200 OK\nContent-length: " ++ contentlenght ++ 
            "\nContent-type: " ++ contenttype ++ 
            "\nContent-encoding: " ++ contentencoding ++ "\n\n" ++ content
    where
        contentlenght = show $ (length content + 10)
        contenttype = case (fst mime) of
            Just ctype -> ctype
            Nothing -> "text/html"
        contentencoding = case (snd mime) of
            Just cencoding -> cencoding
            Nothing -> "utf-8"
        mime = guessType (defaultmtd) False query_url

getQueryURL :: Request -> HTTPHeader
getQueryURL hhead = 
	take 2 $ words hhead

parseHeader :: Request -> HTTPHeader
parseHeader request = 
	getQueryURL $ ( lines request !! 0 )

parseParams :: Request -> POSTParams
parseParams query = 
	if (postline =~ "(&?[A-Za-z0-9-]+=[^&]+)+" :: Bool) then
		map tuplify (map (splitOn "=") (splitOn "&" postline))
	else []
	where 
		postline = last (splitOn "\n" query)

parseRequest :: Socket -> Route -> IO ()
parseRequest requ routes = do
	answer <- Sock.recv requ 1024
	print $ parseHeader answer
	Sock.send requ (mkResponse (response answer) (queryurl answer))
	Sock.sClose requ
	where
        queryurl aw = parseHeader aw !! 1
        response aw = renderRoute (parseHeader aw !! 1) (parseHeader aw !! 0) (parseParams aw) routes

startConn :: Socket -> Route -> IO ()
startConn socket routes = forever $ do
		(handle, addr) <- Sock.accept socket
		parseRequest handle routes

runServer :: PortNumber -> Route -> IO ()
runServer port routes = withSocketsDo $ do
	sock <- listenOn $ PortNumber port
	startConn sock routes
