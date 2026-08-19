{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Exception     (bracket)
import Data.Bits             ((.|.), (.&.))
import Data.ByteString       (ByteString)
import Data.Coerce           (coerce)
import Data.Int              (Int32)
import Data.List             ((\\))
import Data.Vector           (Vector)
import Foreign.C
import Foreign.C.ConstPtr    (ConstPtr(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (advancePtr)
import Foreign.Ptr           (Ptr)
import Foreign.Storable      (peek)
import Unsafe.Coerce         (unsafeCoerce)
import Vulkan.CStruct.Extends (SomeStruct(..))
import Vulkan.Zero           (zero)

import qualified Data.ByteString                    as BS
import qualified Data.ByteString.Char8              as BSC
import qualified Data.Vector                        as V
import qualified RGFW                               as RGFW
import qualified Vulkan.Core10                      as Vk
import qualified Vulkan.Extensions.VK_KHR_surface   as Vk
import qualified Vulkan.Extensions.VK_KHR_swapchain as Vk

height :: Int32
height = 400
width :: Int32
width = 800

layers :: Vector ByteString
layers = V.fromList $ map BSC.pack ["VK_LAYER_KHRONOS_validation"]

extensions :: Vector ByteString
extensions = V.fromList $ map BSC.pack ["VK_KHR_swapchain"]

main :: IO ()
main = withRGFW "rgfw instance title" (fromIntegral $ RGFW.unwrapRGFW_initFlags_enum RGFW.RGFW_initVulkan) $ \_ -> do
    putStrLn $ show extensions
    exts <- alloca $ \extension_count -> do
        exts <- RGFW.rGFW_getRequiredInstanceExtensions_Vulkan extension_count
        cexts <- peek extension_count
        vexts <- processExtensions cexts exts V.empty
        return vexts
    Vk.withInstance (zero {Vk.enabledExtensionNames = exts, Vk.enabledLayerNames = layers}) Nothing bracket $ \i -> do
        withWindow "test window" 0 0 width height ((fromIntegral (RGFW.unwrapRGFW_windowFlags_enum RGFW.RGFW_windowCenter)) .|. (fromIntegral (RGFW.unwrapRGFW_windowFlags_enum RGFW.RGFW_windowNoResize))) $ \window -> do
            surface :: Vk.SurfaceKHR <- alloca $ \surfacePtr -> do
                _ <- RGFW.rGFW_window_createSurface_Vulkan window (coerce $ Vk.instanceHandle i) surfacePtr
                return . unsafeCoerce =<< peek surfacePtr
            (_, pdevs) <- Vk.enumeratePhysicalDevices i
            pdev <- pickPhysicalDevice pdevs surface
            qfprops <- Vk.getPhysicalDeviceQueueFamilyProperties pdev
            gqueueIndex <- return $ fromIntegral $ head $ getGraphicsQueues qfprops
            pqueueIndex <- return . fromIntegral . head =<< getSurfaceSupport pdev surface
            queueCreateInfos <- return $ [(zero :: Vk.DeviceQueueCreateInfo '[]) { Vk.queueFamilyIndex = gqueueIndex, Vk.queuePriorities = V.fromList [1.0]}]
                                           ++ if gqueueIndex == pqueueIndex then [] else [(zero :: Vk.DeviceQueueCreateInfo '[]) { Vk.queueFamilyIndex = pqueueIndex, Vk.queuePriorities = V.fromList [1.0]}]
            Vk.withDevice pdev (zero { Vk.queueCreateInfos = V.fromList $ map (SomeStruct) queueCreateInfos
                                     , Vk.enabledExtensionNames = extensions
                                     }) Nothing bracket $ \dev -> do
                gqueue <- Vk.getDeviceQueue dev gqueueIndex 0
                pqueue <- Vk.getDeviceQueue dev pqueueIndex 0
                Vk.withCommandPool dev (zero {Vk.queueFamilyIndex = gqueueIndex, Vk.flags = Vk.COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT}) Nothing bracket $ \gpool -> do
                    Vk.withCommandBuffers dev (zero {Vk.commandPool = gpool, Vk.level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY, Vk.commandBufferCount = 2}) bracket $ \gcbuffer -> do
                        putStrLn "made command buffer"
                        caps <- Vk.getPhysicalDeviceSurfaceCapabilitiesKHR pdev surface
                        putStrLn $ show caps
                        (_, forms) <- Vk.getPhysicalDeviceSurfaceFormatsKHR pdev surface
                        putStrLn $ show forms
                        Vk.withSwapchainKHR dev zero { Vk.clipped = True
                                                     , Vk.compositeAlpha = Vk.COMPOSITE_ALPHA_OPAQUE_BIT_KHR
                                                     , Vk.imageArrayLayers = 1
                                                     , Vk.imageColorSpace = (V.head forms).colorSpace
                                                     , Vk.imageExtent = caps.currentExtent
                                                     , Vk.imageFormat = (V.head forms).format -- TODO fetch the best format
                                                     , Vk.imageSharingMode = if length queueCreateInfos > 1 then Vk.SHARING_MODE_CONCURRENT else Vk.SHARING_MODE_EXCLUSIVE
                                                     , Vk.imageUsage = Vk.IMAGE_USAGE_COLOR_ATTACHMENT_BIT
                                                     , Vk.minImageCount = caps.minImageCount
                                                     , Vk.preTransform = Vk.SURFACE_TRANSFORM_IDENTITY_BIT_KHR
                                                     , Vk.surface = surface
                                                     } Nothing bracket $ \swapchain -> do
                            putStrLn "made swapchain"
                            (_, images) <- Vk.getSwapchainImagesKHR dev swapchain
                            putStrLn $ show images
                            withImageViews dev (V.map (\image -> (zero :: Vk.ImageViewCreateInfo '[]) { Vk.image = image
                                                                                                      , Vk.viewType = Vk.IMAGE_VIEW_TYPE_2D
                                                                                                      , Vk.subresourceRange = zero { Vk.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT
                                                                                                                                   , Vk.levelCount = Vk.REMAINING_MIP_LEVELS
                                                                                                                                   , Vk.layerCount = Vk.REMAINING_ARRAY_LAYERS
																   }
                                                                                                      , Vk.format = (V.head forms).format -- TODO fetch the best format
                                                                                                      }) images) Nothing $ \imageViews -> do
                                Vk.withRenderPass dev zero { Vk.subpasses = V.fromList [zero {Vk.pipelineBindPoint = Vk.PIPELINE_BIND_POINT_GRAPHICS}]
                                                           } Nothing bracket $ \pass -> do
                                    putStrLn "made pass"
                                    Vk.withFramebuffer dev zero { Vk.height = fromIntegral height
                                                                , Vk.width = fromIntegral width
                                                                , Vk.renderPass = pass
                                                                , Vk.layers = 1
                                                                } Nothing bracket $ \fb -> do
                                        putStrLn "made fb"
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

-- checks that a device has the requisite capabilities
isValidPhysicalDevice :: Vk.PhysicalDevice -> Vk.SurfaceKHR-> IO Bool
isValidPhysicalDevice pdev surface = do
    qfprops <- Vk.getPhysicalDeviceQueueFamilyProperties pdev
    surfaceSupport <- getSurfaceSupport pdev surface
    (_, caps) <- Vk.enumerateDeviceExtensionProperties pdev Nothing
    if null $ getGraphicsQueues qfprops
    then return False
    else if null surfaceSupport
    then return False
    else if [] /= ((V.toList extensions) \\ (V.toList $ V.map (\p -> p.extensionName) caps))
    then return False
    else return True

-- type conversion
processExtensions :: CSize -> Ptr (ConstPtr CChar) -> Vector ByteString -> IO (Vector ByteString)
processExtensions 0 _ extNames = return extNames
processExtensions count strs extNames = do
    str <- peek strs
    extName <- BS.packCString (coerce str)
    processExtensions (count - 1) (advancePtr strs 1) $ V.snoc extNames extName

gameloop :: Ptr RGFW.RGFW_window -> RGFW.RGFW_bool  -> IO ()
gameloop window 0 = gameloop window =<< RGFW.rGFW_window_shouldClose window
gameloop _ _ = return ()

withImageViews :: Vk.Device -> Vector (Vk.ImageViewCreateInfo '[]) -> Maybe Vk.AllocationCallbacks -> (Vector Vk.ImageView -> IO r) -> IO r
withImageViews dev infos alloc io = do
    imageViews <- mapM (\(info) -> Vk.createImageView dev info alloc) infos
    o0 <- io imageViews
    mapM (\imageView -> Vk.destroyImageView dev imageView alloc) imageViews
    return o0

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