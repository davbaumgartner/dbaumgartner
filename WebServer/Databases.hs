module Database ( connectTo
				, DBScheme
				, Datetime
				, SQLField
	) where

import qualified Database.SQLite
import qualified Data.String.Utils as SUtils

data Datetime = Date Int Int Int
			  | Time Int Int Int 
			  | Datetime Int Int Int Int Int Int Int
			  | Timestamp Int

data SQLField = SQLString String String
			   | SQLChar String Char
			   | SQLInt String Int
			   | SQLDecimal String Float
			   | SQLBool String Bool
			   | SQLDate String Date
			   | SQLTime String Time
			   | SQLDatetime String Datetime
			   | SQLTimestamp String Timestamp
			   | SQLBinary String String
			   | SQLSerial String Int

type DBTableName = String

type DBName = String

type DBScheme = (String, String, String, String, String, String)

type DBStruct = [SQLField]

type DBValues = [DBStruct]

connect :: DBScheme -> IO SQLiteHandle
connectTo ("sqlite", filename, _, _, _, _) = Database.SQLite.openConnection filename
connectTo ("mysql", host, port, user, password, dbname) = error "This isn't implemented at this time."
connectTo ("pgsql", host, port, user, password, dbname) = error "This isn't implemented at this time."
connectTo ("tempdb", _, _, _, _, _) = error "This isn't implemented at this time."

createTable :: DBTableName -> DBStruct -> Bool
createTable tablename datastruct = 
	length (execQuery "CREATE TABLE '" ++ (destStruct datastruct) ++ "';") == 0 

insert :: DBTableName -> DBValues -> IO DBStruct
insertInto tablename datavalues = do
	if tableexists then
		exinsert
	else
		createTable tablename datavalues !! 0;
		exinsert
	where
		tableexists = length (execQuery "SHOW TABLES LIKE '" ++ tablename ++ "';") == 1 
		exinsert = execQuery ("INSERT INTO '" ++ tablename ++ "' VALUES (" ++  (SUtils.join "), (" (SUtils.join ", " (destValues))) ")")