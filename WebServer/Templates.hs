module WebServer.Templates (
		  renderString
		, renderFile
		, basePage
		, basePage'
	) where
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Applicative hiding (many, (<|>))
import System.Environment
import Control.Monad
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.String.Utils as SUtils

type LocalVars = [(String, String)]
type DBVars = [(String, [[(String, String)]])]

data Template = Variable LocalVars DBVars String
			  | HTML LocalVars DBVars String
			  | DBVariable LocalVars DBVars String String String

instance Show Template where show = showParsed

{-
LocalVars :: [(String, String)]
LocalVars =  [("name", "dab")
			 ,("frite", "patate")]
DBVars :: [(String, [[(String, String)]])]
DBVars = [("frite", [[("title","Qu'est-ce que le lorem ipsum ?"), ("text","Le Lorem Ipsum est simplement du faux texte employé dans la composition et la mise en page avant impression. Le Lorem Ipsum est le faux texte standard de l'imprimerie depuis les années 1500, quand un peintre anonyme assembla ensemble des morceaux de texte pour réaliser un livre spécimen de polices de texte. Il n'a pas fait que survivre cinq siècles, mais s'est aussi adapté à la bureautique informatique, sans que son contenu n'en soit modifié. Il a été popularisé dans les années 1960 grâce à la vente de feuilles Letraset contenant des passages du Lorem Ipsum, et, plus récemment, par son inclusion dans des applications de mise en page de texte, comme Aldus PageMaker.")], [("title","D'ou vient-il ?"), ("text","Contrairement à une opinion répandue, le Lorem Ipsum n'est pas simplement du texte aléatoire. Il trouve ses racines dans une oeuvre de la littérature latine classique datant de 45 av. J.-C., le rendant vieux de 2000 ans. Un professeur du Hampden-Sydney College, en Virginie, s'est intéressé à un des mots latins les plus obscurs, consectetur, extrait d'un passage du Lorem Ipsum, et en étudiant tous les usages de ce mot dans la littérature classique, découvrit la source incontestable du Lorem Ipsum. Il provient en fait des sections 1.10.32 et 1.10.33 du (Des Suprêmes Biens et des Suprêmes Maux) de Cicéron. Cet ouvrage, très populaire pendant la Renaissance, est un traité sur la théorie de l'éthique. Les premières lignes du Lorem Ipsum, proviennent de la section 1.10.32.")]])]
-}

readLocalVars :: LocalVars -> String -> LocalVars
readLocalVars lvars name = [l|l<-lvars, (fst l)==name]

readDBVars :: DBVars -> String -> DBVars
readDBVars dvars name = [l|l<-dvars, (fst l)==name]

readExpr :: LocalVars -> DBVars -> String -> String
readExpr lvars dvars input = case parse (parseAll lvars dvars) "" input of
    Left err -> "[ERROR] " ++ show err
    Right val -> SUtils.join "" (map showParsed val)

parseAll :: LocalVars -> DBVars -> Parser [Template]
parseAll lvars dvars = many ((parseVariable lvars dvars) <|> (parseDBVariable lvars dvars) <|> (parseHTML lvars dvars))

parseHTML :: LocalVars -> DBVars -> Parser Template
parseHTML lvars dvars = HTML lvars dvars <$> many1 (noneOf "{[" <|> try (char '{' <* notFollowedBy (char '{')) <|> try (char '[' <* notFollowedBy (char '[')))

parseVariable :: LocalVars -> DBVars -> Parser Template
parseVariable lvars dvars = Variable lvars dvars <$> (string "{{ " *> many (oneOf (['a'..'z']++['A'..'Z'])) <* string " }}")

parseDBVariable :: LocalVars -> DBVars -> Parser Template
parseDBVariable lvars dvars = do
	_ <- string "[[ "
	originalname <- many (oneOf (['a'..'z']++['A'..'Z']))
	_ <- string " as "
	newname <- many (oneOf (['a'..'z']++['A'..'Z']))
	_ <- string " ]]("
	content <- many (noneOf ")")
	_ <- string ")"
	return $ DBVariable lvars dvars originalname newname content

showParsed :: Template -> String
showParsed (DBVariable lvars dvars originalname newname content) = 
	SUtils.join "" [parseLocal a|a<-cvar]
	where 
		parseLocal :: [(String, String)] -> String
		parseLocal var = 
			parseLocal' var content
			where
				newstring :: String
				newstring = 
					SUtils.replace originalname newname content
				mkname :: (String, String) -> String	
				mkname rvar =
					"{{ "++(SUtils.join "." [newname, fst rvar])++" }}"
				parseLocal' :: [(String, String)] -> String -> String
				parseLocal' [] str = 
					str
				parseLocal' (cvar:ovar) str = 
					parseLocal' ovar (SUtils.replace (mkname cvar) (snd cvar) str)
		cvar :: [[(String, String)]]
		cvar = 
			if length match > 0 then
				snd (match !! 0)
			else
				error ("No db variable matching ``" ++ originalname ++ "'' is defined")
			where 
				match = readDBVars dvars originalname
showParsed (HTML lvars dvars x) = x
showParsed (Variable lvars dvars x) = 
	if length matching > 0 then
		snd $ matching !! 0
	else
		error ("No variable matching ``" ++ x ++ "'' is defined")
	where
		matching = readLocalVars lvars x

-- Read an parse from string
renderString :: LocalVars -> DBVars -> String -> String
renderString lvars dvars string = readExpr lvars dvars string

-- Read an parse from file
renderFile :: LocalVars -> DBVars -> String -> String
renderFile lvars dvars fname = 
	unsafePerformIO ma
	where 
		ma = do
			content <- readFile fname
			return $ renderString lvars dvars content

basePage :: [String] -> [String] -> String -> String -> String
basePage csss jss title content = 
	"<!doctype html><html><head><meta charset='utf-8'/><title>"++title ++"</title>"++
		(SUtils.join "" ["<link href='"++x++"' rel='stylesheet' />"|x<-csss])++
		(SUtils.join "" ["<script src='"++x++"' type='text/javascript'></script>"|x<-jss])++
	"</head><body>"++
		content++
	"</body></html>"

basePage' :: String -> String -> String
basePage' title content = basePage [] [] title content

-- Read an parse from stdin
readFromStd :: String -> IO ()
readFromStd f =
	if f /= "quit" then
		prompt
	else
		putStrLn "> Bye."
	where 
		prompt = do
			putStrLn $ "> " ++ renderString [] [] f
			nsl <- getLine
			readFromStd nsl

main :: IO ()
main = do 
	args <- getArgs
	if length args >= 3 then
		putStrLn $ renderString (read (args !! 1) :: LocalVars) (read (args !! 2) :: DBVars) (args !! 0) 
	else 
		readFromStd "Welcome."