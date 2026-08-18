{-# LANGUAGE DuplicateRecordFields #-}

module Main (main) where

import           Control.Exception             (bracket)
import           Data.Bits                     ((.|.))
import           Data.ByteString               (ByteString, packCString)
import           Data.Coerce                   (coerce)
import           Data.Int                      (Int32)
import           Data.Vector           as V
import           Data.Vector()
import           Foreign.C
import           Foreign.C.ConstPtr            (ConstPtr(..))
import           Foreign.Marshal.Alloc         (alloca)
import           Foreign.Marshal.Array         (advancePtr)
import           Foreign.Ptr                   (Ptr)
import           Foreign.Storable              (peek)
import qualified RGFW                  as RGFW
import qualified Vulkan.Core10         as Vk
import           Vulkan.Zero                   (zero)

height :: Int32
height = 400
width :: Int32
width = 800

main :: IO ()
main = withRGFW "rgfw instance title" 0 $ \_ -> do
           exts <- alloca $ \extension_count -> do
               exts <- RGFW.rGFW_getRequiredInstanceExtensions_Vulkan extension_count
               cexts <- peek extension_count
               putStr $ show cexts
               putStrLn " extensions required:"
               vexts <- processExtensions cexts exts V.empty
               putStrLn $ show vexts
               return vexts
           Vk.withInstance (zero {Vk.enabledExtensionNames = exts}) Nothing bracket $ \i -> do
               putStrLn $ show i
               withWindow "test window" 0 0 width height ((fromIntegral (RGFW.unwrapRGFW_windowFlags_enum RGFW.RGFW_windowCenter)) .|. (fromIntegral (RGFW.unwrapRGFW_windowFlags_enum RGFW.RGFW_windowNoResize))) $ \window -> do
                   res <- RGFW.rGFW_window_createSurface_Vulkan window i nullPtr
                   ret <- gameloop window 0
                   putStr "gameloop returned with code: "
                   putStrLn $ show ret


processExtensions :: CSize -> Ptr (ConstPtr CChar) -> Vector ByteString -> IO (Vector ByteString)
processExtensions 0 _ extNames = return extNames
processExtensions count strs extNames = do
    str <- peek strs
    extName <- packCString (coerce str)
    processExtensions (count - 1) (advancePtr strs 1) $ V.snoc extNames extName

gameloop :: Ptr RGFW.RGFW_window -> RGFW.RGFW_bool  -> IO ()
gameloop window 0 = gameloop window =<< RGFW.rGFW_window_shouldClose window
gameloop _ _ = return ()

withWindow :: String -> Int32 -> Int32 -> Int32 -> Int32 -> RGFW.RGFW_windowFlags -> (Ptr RGFW.RGFW_window -> IO r) -> IO r
withWindow name x y w h flags io = withCString name $ \str -> do
                                       window <- RGFW.rGFW_createWindow (ConstPtr str) (RGFW.I32 x) (RGFW.I32 y) (RGFW.I32 w) (RGFW.I32 h) flags
                                       o0 <- io window
                                       RGFW.rGFW_window_close window
                                       return o0

withRGFW :: String -> RGFW.RGFW_initFlags -> (Int32 -> IO r) -> IO r
withRGFW title flags io = withCString title $ \str -> do
                              ret_code <- RGFW.rGFW_init (ConstPtr str) flags
                              o0 <- io $ fromIntegral ret_code
                              RGFW.rGFW_deinit
                              return o0