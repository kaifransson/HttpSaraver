import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import qualified Data.ByteString.Char8     as BS
import           Data.Void
import           Data.Word
import           Foreign.C.Types
import           Network.Socket            hiding (recv, recvFrom, send,
                                            sendAll, sendTo)
import           Network.Socket.ByteString

port :: PortNumber
port = 24782

localhost :: HostAddress
localhost = tupleToHostAddress (127, 0, 0, 1)

serverAddress :: SockAddr
serverAddress = SockAddrInet port localhost

getSocket :: IO Socket
getSocket = do
    s <- socket AF_INET Stream defaultProtocol
    setSocketOption s ReuseAddr 1
    bind s serverAddress
    listen s maxListenQueue
    return s

successMessage :: BS.ByteString
successMessage = BS.pack "HTTP/1.0 200 OK\nContent-type: text/html\n\n<H1>Success!</H1>"

waitForFin :: Socket -> IO ()
waitForFin conn = do
    msg <- recv conn (2^10)
    unless (BS.null msg) (waitForFin conn)

handleRequest :: Async Socket -> IO ()
handleRequest t = do
    conn <- wait t
    sendAll conn successMessage
    shutdown conn ShutdownSend
    waitForFin conn
    close conn

waitForRequests :: Socket -> IO ()
waitForRequests sock = do
    withAsyncBound (fst <$> accept sock) handleRequest
    waitForRequests sock

main :: IO ()
main = do
    sock <- getSocket
    withAsyncBound (waitForRequests sock) (const $ void getChar >> close sock)
