import Network.CGI
import Text.XHtml
 
page :: String -> String -> Html
page path method = body << [h1 << "Hello World!", h2 << ("You made this request at path " ++
		path ++ " with a " ++ method ++ " method")]
 
cgiMain :: CGI CGIResult
cgiMain = do 
	path <- pathInfo
	method <- requestMethod
	let page' = page path method
	output $ renderHtml page'
 
main :: IO ()
main = runCGI $ handleErrors cgiMain
