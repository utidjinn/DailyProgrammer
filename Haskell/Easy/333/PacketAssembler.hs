import qualified Data.Map.Strict as Map
import System.IO
import Data.List
import Data.Maybe
import Control.Monad
import Text.Printf

main = do
    buildPacketMap Map.empty

buildPacketMap :: Map.Map Int [Packet] -> IO (Map.Map Int [Packet])
buildPacketMap mp = do
    end <- isEOF
    if end
        then return mp
        else do
            packetLine <- getLine
            let args = words packetLine
            let packet = createPacketFromStrings args
            let newM = insertWithPacket packet mp
            -- Will always have an entry
            let ps = fromJust $ Map.lookup (messageId packet) newM
            when (length ps == totalPacketsInMessage packet) (mapM_ print (sort ps))
            doBuild <- buildPacketMap newM
            return doBuild

{-|
    Representation of a Packet
    Packet MessageId PacketId TotalPacketsInMessage PacketData
-}
data Packet = Packet {
    messageId :: Int,
    packetId :: Int,
    totalPacketsInMessage :: Int,
    pData :: String
} deriving (Eq)
instance Show Packet where
    show p = printf "%4d    %-3d %-3d %s" (messageId p) (packetId p) (totalPacketsInMessage p) (pData p)
instance Ord Packet where
    (<=) p p2 = packetId p <= packetId p2

createPacketFromStrings :: [String] -> Packet
createPacketFromStrings (s1:s2:s3:ss) =
    Packet (read s1 :: Int) (read s2 :: Int) (read s3 :: Int) (unwords ss)

-- | Updates a map with the packet
insertWithPacket :: Packet -> Map.Map Int [Packet] -> Map.Map Int [Packet]
insertWithPacket p mp =
    Map.insertWith insertWithFunction (messageId p) [p] mp
    where insertWithFunction p p2 = (head p):p2
