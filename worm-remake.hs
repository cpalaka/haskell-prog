import Data.Char(isSpace, toLower,isDigit)
import Data.List.Split(splitOn)
import Data.List (isInfixOf,nub)
import System.Directory (doesFileExist,getPermissions, readable)
import System.FilePath.Posix ((</>))
import Control.Monad(filterM)
import Data.String
import Text.Regex.Posix

location1::FilePath
location1 = "/etc/hosts"

location2::FilePath
location2 = "/etc/ssh/ssh_config"

location3::FilePath
location3 = "/etc/ssh/ssh_known_hosts"

location4::FilePath
location4 = ".ssh/config"

location5::FilePath
location5 = ".ssh/authorized_keys"

location6::FilePath
location6 = ".ssh/known_hosts"

passwdloc::FilePath
passwdloc = "/etc/passwd"

main = do
	-- | get user home directories from /etc/passwd
	-- | usersloc will contain list of all users home directories  
	pw<-readFile passwdloc
	let usersloc = getUserLocations (lines pw) 

	-- | read /etc/hosts file and get output in list of strings in etchostsOP
	file<-readFile location1
	p1<-getPermissions location1
	let etchostsOP = if readable p1 then getHostNameFrometchosts file else [""]

	-- | sshconfigfilepaths will contain paths to .ssh/config files for each valid user
	-- | availablesshconfig will filter out the filepaths which dont have actually point to a file
	-- | a is a list of list of strings (each trimmed non comment line from each valid sshconfig filepath)
	-- | we map getHostNameFromuserconfig to each list of strings in a, and collect the hostnames in userconfigOP
	let sshconfigfilepaths = map (</>location4) usersloc
	availablesshconfig<-filterM doesFileExist sshconfigfilepaths
	configcontents<-mapM readFile availablesshconfig
	let a = map (trim'.removeComments.lines) configcontents--
	let userconfigOP = concat$map getHostNameFromuserconfig a

	-- | collect hostnames in etcconfigOP after checking whether global etc/ssh/ssh_config file exists
	exist<- doesFileExist location2
	etcconfigfile<-if exist then readFile location2 else return ""
	let b = (trim'.removeComments.lines) etcconfigfile
	let etcconfigOP = getHostNameFromuserconfig b

	-- | In a similar fashion to the ~/.ssh/config file, we check if the users authorized_keys
	-- | file exists, then we collect the list of list of strings in c
	-- | we have 3 outputs for the authorized keys file, one which checks for hostnames in comments,
	-- | one, in the from option, and one in the permitopen option
	let authkeysfilepaths = map (</>location5) usersloc
	availableauthkeys<-filterM doesFileExist authkeysfilepaths
	authkeyscontents<-mapM readFile availableauthkeys
	let c = map (trim'.removeComments.lines) authkeyscontents
	let authkeysOP1 = concat$map getHostNameFromAKcomments c
	let authkeysOP2 = map getHostNameFromAKfromopt c 
	let authkeysOP3 = map getHostNameFromAKpermit c

	-- | we find hostnames from knownhosts global file in the same way as the global config file,
	-- | calling getHostNameFromGlobalKH on the list of strings in the file, to output a list of hostnames
	exist1<-doesFileExist location3
	khcontents <- if exist1 then readFile location3 else return ""
	let d = (trim'.removeComments.lines) khcontents
	let globalknownhostsOP = getHostNameFromGlobalKH d


	-- | We find hostnames from the knownhosts local user file, just like we found hostnames for the
	-- | local config file. getHostNameFromGlobalKH is reused here.      
	let knownhostsfilepaths = map (</>location6) usersloc
	availableknownhosts<-filterM doesFileExist knownhostsfilepaths
	knownhostscontents<-mapM readFile availableknownhosts
	let e = map (trim'.removeComments.lines) knownhostscontents
	let userKHOP = concat$map getHostNameFromGlobalKH e
  	
  	-- | collect all the hostnames and perform some final filtering:
  	-- | check if any IP addresses are included, and check if there are empty strings
	let wholeanswer = filter (\str->if length str == 0 || checkIfIp str then False else True) (userKHOP++globalknownhostsOP++(concat authkeysOP3)++(concat authkeysOP2)++authkeysOP1++etchostsOP++userconfigOP++etcconfigOP)
	
	-- | nub will remove all duplicates in the list of hostnames
	mapM_ putStrLn (nub wholeanswer)

-- | i used this function to check if the string is an IP because haskell regex's are crap
checkIfIp::String->Bool
checkIfIp s = if any (id) (map checkIfNum (splitOn "." s)) then True else False
	where
		checkIfNum s = all isDigit s

getHostNameFromGlobalKH::[String]->[String]
getHostNameFromGlobalKH l = map finalCheck (concat$map sepByComma [ head (words x)  |x<-removeMarkers nohash])
	where
		nohash = filter (\str->if head str == '|' then False else True) l
		removeMarkers l = map f l
		f s
			| isInfixOf "@revoked" s = drop 9 s
			| isInfixOf "@cert-authority" s = drop 16 s
			| otherwise = s
		sepByComma s = if isInfixOf "," s then splitOn "," s else [s]
		--check for !, ?, and [] and remove/modify as necessary
		finalCheck s
			| isInfixOf "?" s = ""
			| isInfixOf "!" s = tail s
			| isInfixOf "[" s = head (splitOn "]:" (tail s))
			| otherwise = s

getHostNameFromAKpermit::[String]->[String]
getHostNameFromAKpermit l = filter periodExist (concat$map colonSplit (filter clearEmpty (map clean (filter colonExist (filter periodExist (concat$map commaSplit (filter doesPermitOpenExist (concat$map words l))))))))
	where
		doesPermitOpenExist l = isInfixOf "permitopen=" (map toLower l)
		colonExist = isInfixOf ":"
		clean s
			| isInfixOf "from=" s                       = ""
			| isInfixOf "permitopen=\"" (map toLower s) = drop 12 s
			| isInfixOf "\"" s                          = filter (\ch->if ch=='\"' then False else True) s
			| otherwise                                 = s
		clearEmpty s = if length s==0 then False else True
		
getHostNameFromAKfromopt::[String]->[String]
getHostNameFromAKfromopt l = map removeExcl (filter isValidHostName (map clean (filter periodExist (concat$map commaSplit (filter doesFromOptExist (concat$map words l))))))
	where 
		doesFromOptExist x = isInfixOf "from=" (map toLower x)
		clean s
			| isInfixOf "permitopen" s ||isInfixOf ":" s = ""
			| isInfixOf "from=\"" (map toLower s)        = drop 6 s
			| isInfixOf "\"" s                           = filter (\ch->if ch=='\"' then False else True) s
			| otherwise                                  = s
		isValidHostName s = if isInfixOf "?" s ||length s==0 then False else True

getHostNameFromAKcomments::[String]->[String]
getHostNameFromAKcomments l = map f (filter (\s->if last s == '=' then False else True) (map last (map words l)))
	where
		f s
			| isInfixOf "@" s = last (splitOn "@" s)
			| otherwise = s

getHostNameFromuserconfig::[String]->[String]
getHostNameFromuserconfig l = map removeExcl (filter hostnamecheck (concat x) ++ filter hostnamecheck (concat a))
	where
		x = map words (filter (\str->isInfixOf "hostname" (map toLower str)) l)
		a = map words (filter (\str->isInfixOf "host " (map toLower str)) l)
		hostnamecheck::String->Bool
		hostnamecheck s
			| (map toLower s) == "hostname" || (map toLower s) == "host" = False
			| s == "*" = False
			| isInfixOf "?" s = False
			| otherwise = True

removeExcl::String->String
removeExcl l = if head l == '!' then tail l else l

-- | takes string from etc/hosts and returns all hostnames
getHostNameFrometchosts::String->[String]
getHostNameFrometchosts file = foldl (\l m->l++drop 1 m) [] (map words (removeComments$lines file))

-- | takes list of lines in /etc/passwd and returns list of valid user directories (basically all the ones which are in /home/)
getUserLocations::[String]->[String]
getUserLocations l = getValidLocations [ x!!5 |x<-map colonSplit l]
	where getValidLocations x = filter (\l->isInfixOf "home" l) x

periodExist::String->Bool
periodExist = isInfixOf "."

-- | split a string delimited by colons
colonSplit::String->[String]
colonSplit = splitOn ":"

-- | split a string delimited by commas
commaSplit::String->[String]
commaSplit = splitOn ","

-- | remove all strings which start with '#'
removeComments::[String]->[String]
removeComments p = filter (\l->if (head$trim l) == '#' then False else True) p

-- | trims starting whitespace for given string (used only in removeComments)
trim::String->String
trim "" = "#"
trim l = dropWhile isSpace l

-- | trims whitespace of every string in given list
trim'::[String]->[String]
trim' = map (dropWhile isSpace)