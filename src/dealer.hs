import Network.CGI
import Text.XHtml
import qualified Data.Text as Data.Text
import Blackwood.Bridge
import Blackwood.Deal
import System.Random
 
getChunks :: String -> [String]
getChunks s = map Data.Text.unpack $ Data.Text.splitOn (Data.Text.pack "/") (Data.Text.pack s)

page :: String -> String -> Html
page path method = body << paragraph << result
    where chunks = getChunks path
          seed   = read (chunks !! 1) :: Int
          board  = deal . shuffle deck . mkStdGen $ seed
          result 
            | (length chunks) == 2 = show board
            | (length chunks) == 3 = show $ getHand (read $ chunks !! 2) board

cgiMain :: CGI CGIResult
cgiMain = do 
	path <- pathInfo
	method <- requestMethod
	page' <- return (page path method)
	output $ renderHtml page'
 
main :: IO ()
main = runCGI $ handleErrors cgiMain
