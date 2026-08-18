{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Exception     (bracket)
import Data.Bits             ((.|.), (.&.))
import Data.ByteString       (ByteString, packCString)
import Data.Coerce           (coerce)
import Data.Int              (Int32)
import Data.Vector           (Vector)
import Foreign.C
import Foreign.C.ConstPtr    (ConstPtr(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (advancePtr)
import Foreign.Ptr           (Ptr)
import Foreign.Storable      (peek)
import Unsafe.Coerce         (unsafeCoerce)
import Vulkan.Zero           (zero)

import qualified Data.Vector                      as V
import qualified RGFW                             as RGFW
import qualified Vulkan.Core10                    as Vk
import qualified Vulkan.Extensions.VK_KHR_surface as Vk

height :: Int32
height = 400
width :: Int32
width = 800

main :: IO ()
main = withRGFW "rgfw instance title" (fromIntegral $ RGFW.unwrapRGFW_initFlags_enum RGFW.RGFW_initVulkan) $ \_ -> do
           exts <- alloca $ \extension_count -> do
               exts <- RGFW.rGFW_getRequiredInstanceExtensions_Vulkan extension_count
               cexts <- peek extension_count
               putStr $ show cexts
               putStr " extensions required: "
               vexts <- processExtensions cexts exts V.empty
               putStrLn $ show vexts
               return vexts
           Vk.withInstance (zero {Vk.enabledExtensionNames = exts}) Nothing bracket $ \i -> do
               withWindow "test window" 0 0 width height ((fromIntegral (RGFW.unwrapRGFW_windowFlags_enum RGFW.RGFW_windowCenter)) .|. (fromIntegral (RGFW.unwrapRGFW_windowFlags_enum RGFW.RGFW_windowNoResize))) $ \window -> do
                   surface :: Vk.SurfaceKHR <- alloca $ \surfacePtr -> do
                       RGFW.rGFW_window_createSurface_Vulkan window (coerce $ Vk.instanceHandle i) surfacePtr
                       return . unsafeCoerce =<< peek surfacePtr
                   (_, pdevs) <- Vk.enumeratePhysicalDevices i
                   pdev <- pickPhysicalDevice pdevs surface
                   Vk.withDevice pdev zero Nothing bracket $ \dev -> do
                       qfprops <- Vk.getPhysicalDeviceQueueFamilyProperties pdev
                       queue <- Vk.getDeviceQueue dev (fromIntegral $ head $ getGraphicsQueues qfprops) 0
                       putStrLn $ show queue
                       ret <- gameloop window 0
                       putStr "gameloop returned with code: "
                       putStrLn $ show ret

pickPhysicalDevice :: Vector Vk.PhysicalDevice -> Vk.SurfaceKHR -> IO Vk.PhysicalDevice
pickPhysicalDevice pdevs surface = do
    validpdevs <- (V.filterM (\o -> isValidPhysicalDevice o surface) pdevs)
    pickPhysicalDevice' validpdevs V.empty

pickPhysicalDevice' :: Vector Vk.PhysicalDevice -> Vector Vk.PhysicalDevice -> IO Vk.PhysicalDevice
pickPhysicalDevice' pdevs opts = if V.null pdevs
                                 then do
                                     props <- Vk.getPhysicalDeviceProperties $ V.head opts
                                     putStr "Selected physical device of type: "
                                     putStrLn $ show props.deviceType
                                     return $ V.head opts
                                 else do
                                     props <- Vk.getPhysicalDeviceProperties $ V.head pdevs
                                     case props.deviceType of
                                         Vk.PHYSICAL_DEVICE_TYPE_DISCRETE_GPU -> pickPhysicalDevice' (V.tail pdevs) (V.cons (V.head pdevs) opts)
                                         Vk.PHYSICAL_DEVICE_TYPE_CPU -> pickPhysicalDevice' (V.tail pdevs) opts
                                         _ -> pickPhysicalDevice' (V.tail pdevs) (V.snoc opts (V.head pdevs))

-- returns list of queueFamilyIndex whick support a graphics pipeline
getGraphicsQueues :: Vector Vk.QueueFamilyProperties -> [Int]
getGraphicsQueues qfprops = getGraphicsQueues' qfprops 0 []

getGraphicsQueues' :: Vector Vk.QueueFamilyProperties -> Int -> [Int] -> [Int]
getGraphicsQueues' qfprops i is = if V.null qfprops then is else
                                  if zero /= (Vk.QUEUE_GRAPHICS_BIT .&. (V.head qfprops).queueFlags)
                                  then getGraphicsQueues' (V.tail qfprops) (i + 1) (i:is)
                                  else getGraphicsQueues' (V.tail qfprops) (i + 1) is

-- returns list of queueFamilyIndex which support presentation
getSurfaceSupport :: Vk.PhysicalDevice -> Vk.SurfaceKHR -> IO [Int]
getSurfaceSupport pdev surface = do
    qfprops <- Vk.getPhysicalDeviceQueueFamilyProperties pdev
    getSurfaceSupport' pdev surface (V.length qfprops - 1) []

getSurfaceSupport' :: Vk.PhysicalDevice -> Vk.SurfaceKHR -> Int -> [Int] -> IO [Int]
getSurfaceSupport' pdev surface i is = if i < 0 then return is else do
    support <- Vk.getPhysicalDeviceSurfaceSupportKHR pdev (fromIntegral i) surface
    if support
    then getSurfaceSupport' pdev surface (i - 1) (i:is)
    else getSurfaceSupport' pdev surface (i - 1) is

isValidPhysicalDevice :: Vk.PhysicalDevice -> Vk.SurfaceKHR-> IO Bool
isValidPhysicalDevice pdev surface = do
    qfprops <- Vk.getPhysicalDeviceQueueFamilyProperties pdev
    surfaceSupport <- getSurfaceSupport pdev surface
    if null $ getGraphicsQueues qfprops
    then return False
    else if null surfaceSupport
    then return False
    else return True

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