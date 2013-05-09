import Data.Char
import System.IO
import Data.Maybe
import WebServer.Server
import WebServer.Templates
import Data.String.Utils (join)
import System.IO.Unsafe (unsafePerformIO)

menu =	[("urls", [[("uri","/"),("title","Ici")]
		])]

getHomeR :: URLParams -> POSTParams -> Render
getHomeR _ _ = 
	renderFile lvars dvars "templates/index.html"
	where 
		lvars = [("ptitle","David Baumgartner")
				,("pcontent","<p><span class=\"big\">La photographie</span>,<span class=\"right\">c'est avant tout un sentiment&nbsp; une émotion, figée à jamais en une image.</span></p>")
				,("pimagenbfirst","http://i.imgur.com/ia20r9p.jpg")]
		dvars = menu ++ images
		images = [("imagesnb", [
				[("title","Milan noir"),("src","http://i.imgur.com/L4ONSRK.jpg")],
				[("title","Lézard vert"),("src","http://i.imgur.com/Imedogg.jpg")],
				[("title","Underwater"),("src","http://i.imgur.com/qB1VrHw.jpg")],
				[("title","Underwater 2"),("src","http://i.imgur.com/NtO271G.jpg")]
			])]

getAssetR :: URLParams -> POSTParams -> Render
getAssetR get _ = 
	unsafePerformIO filec
	where
		filec = readFile ("assets/" ++ (join "/" get))

routes = [("/$", "GET", getHomeR),
		  ("/assets/([a-z]+)/([a-z]+\\.[a-z]+)", "GET", getAssetR)]

main :: IO () 
main = do
    putStrLn "Starting server on port 3000"
    runServer 3000 routes
