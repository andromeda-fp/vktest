{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module VkTest.Util ( withRGFW
                   , withWindow
                   , withShaderModules
                   , withImageViews
                   , withSemaphores
                   , withFences
                   , pickPhysicalDevice
                   , processExtensions
                   , getGraphicsQueues
                   ) where

import Data.Bits             ((.&.))
import Data.ByteString       (ByteString)
import Data.Coerce           (coerce)
import Data.Int              (Int32)
import Data.List             ((\\))
import Data.Vector           (Vector)
import Foreign.C
import Foreign.C.ConstPtr    (ConstPtr(..))
import Foreign.Marshal.Array (advancePtr)
import Foreign.Ptr           (Ptr)
import Foreign.Storable      (peek)
import Vulkan.Zero           (zero)

import VkTest.Config (QueriedData(..))

import qualified VkTest.Config as Config

import qualified Data.ByteString                    as BS
import qualified Data.Vector                        as V
import qualified RGFW                               as RGFW
import qualified Vulkan.Core10                      as Vk
import qualified Vulkan.Extensions.VK_KHR_surface   as Vk

pickPhysicalDevice :: Vector Vk.PhysicalDevice -> Vk.SurfaceKHR -> IO (Vk.PhysicalDevice, QueriedData)
pickPhysicalDevice pdevs surface = do
    queriedDevs <- do
        qs <- V.mapM (\pdev -> (queryPhysicalDevice pdev surface)) pdevs
        return $ V.zip pdevs qs
    let validpdevs = V.filter isValidPhysicalDevice queriedDevs
    pickPhysicalDevice' validpdevs V.empty

pickPhysicalDevice' :: Vector (Vk.PhysicalDevice, QueriedData) -> Vector (Vk.PhysicalDevice, QueriedData) -> IO (Vk.PhysicalDevice, QueriedData)
pickPhysicalDevice' pdevs opts =
    if V.null pdevs
    then return $ (V.head opts)
    else do
        let pdev = V.head pdevs
        case (snd pdev).physicalDeviceProperties.deviceType of
            Vk.PHYSICAL_DEVICE_TYPE_DISCRETE_GPU -> pickPhysicalDevice' (V.tail pdevs) (V.cons pdev opts)
            Vk.PHYSICAL_DEVICE_TYPE_CPU -> pickPhysicalDevice' (V.tail pdevs) opts
            _ -> pickPhysicalDevice' (V.tail pdevs) (V.snoc opts pdev)

-- returns list of queueFamilyIndex whick support a graphics pipeline
getGraphicsQueues :: QueriedData -> [Int]
getGraphicsQueues q = getGraphicsQueues' q.queueFamilyProperties 0 []
getGraphicsQueues' :: Vector Vk.QueueFamilyProperties -> Int -> [Int] -> [Int]
getGraphicsQueues' qfprops i is =
    if V.null qfprops then is else
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

queryPhysicalDevice :: Vk.PhysicalDevice -> Vk.SurfaceKHR -> IO QueriedData
queryPhysicalDevice pdev surface = do
    qfprops <- Vk.getPhysicalDeviceQueueFamilyProperties pdev
    sSupport <- getSurfaceSupport pdev surface
    (_, extps) <- Vk.enumerateDeviceExtensionProperties pdev Nothing
    caps <- Vk.getPhysicalDeviceSurfaceCapabilitiesKHR pdev surface
    (_, forms) <- Vk.getPhysicalDeviceSurfaceFormatsKHR pdev surface
    props <- Vk.getPhysicalDeviceProperties pdev
    return QueriedData { surfaceCapabilities      = caps
                       , surfaceSupport           = sSupport
                       , surfaceFormats           = forms
                       , queueFamilyProperties    = qfprops
                       , extensionProperties      = extps
                       , physicalDeviceProperties = props
                       }

-- checks that a device has the requisite capabilities
isValidPhysicalDevice :: (Vk.PhysicalDevice, QueriedData) -> Bool
isValidPhysicalDevice (_, q) =
    if null $ getGraphicsQueues q
    then False
    else if null q.surfaceSupport
    then False
    else if [] /= ((V.toList Config.extensions) \\ (V.toList $ V.map (\p -> p.extensionName) q.extensionProperties))
    then False
    else True

-- type conversion
processExtensions :: CSize -> Ptr (ConstPtr CChar) -> Vector ByteString -> IO (Vector ByteString)
processExtensions 0 _ extNames = return extNames
processExtensions count strs extNames = do
    str <- peek strs
    extName <- BS.packCString (coerce str)
    processExtensions (count - 1) (advancePtr strs 1) $ V.snoc extNames extName

withFences :: Vk.Device -> Vector (Vk.FenceCreateInfo '[]) -> Maybe Vk.AllocationCallbacks -> (Vector Vk.Fence -> IO r) -> IO r
withFences dev infos alloc io = do
    fs <- mapM (\info -> Vk.createFence dev info alloc) infos
    o0 <- io fs
    _ <- mapM (\f -> Vk.destroyFence dev f alloc) fs
    return o0

withSemaphores :: Vk.Device -> Vector (Vk.SemaphoreCreateInfo '[]) -> Maybe Vk.AllocationCallbacks -> (Vector Vk.Semaphore -> IO r) -> IO r
withSemaphores dev infos alloc io = do
    ss <- mapM (\info -> Vk.createSemaphore dev info alloc) infos
    o0 <- io ss
    _ <- mapM (\s -> Vk.destroySemaphore dev s alloc) ss
    return o0

withImageViews :: Vk.Device -> Vector (Vk.ImageViewCreateInfo '[]) -> Maybe Vk.AllocationCallbacks -> (Vector Vk.ImageView -> IO r) -> IO r
withImageViews dev infos alloc io = do
    imageViews <- mapM (\info -> Vk.createImageView dev info alloc) infos
    o0 <- io imageViews
    _ <- mapM (\imageView -> Vk.destroyImageView dev imageView alloc) imageViews
    return o0

withShaderModules :: Vk.Device -> Vector (Vk.ShaderModuleCreateInfo '[]) -> Maybe Vk.AllocationCallbacks -> (Vector Vk.ShaderModule -> IO r) -> IO r
withShaderModules dev infos alloc io = do
    mods <- mapM (\info -> Vk.createShaderModule dev info alloc) infos
    o0 <- io mods
    _ <- mapM (\mod' -> Vk.destroyShaderModule dev mod' alloc) mods
    return o0

withWindow :: String -> Int32 -> Int32 -> Int32 -> Int32 -> RGFW.RGFW_windowFlags -> (Ptr RGFW.RGFW_window -> IO r) -> IO r
withWindow name x y w h flags io =
    withCString name $ \str -> do
        window <- RGFW.rGFW_createWindow (ConstPtr str) (RGFW.I32 x) (RGFW.I32 y) (RGFW.I32 w) (RGFW.I32 h) flags
        o0 <- io window
        RGFW.rGFW_window_close window
        return o0

withRGFW :: String -> RGFW.RGFW_initFlags -> (Int32 -> IO r) -> IO r
withRGFW title flags io =
    withCString title $ \str -> do
        ret_code <- RGFW.rGFW_init (ConstPtr str) flags
        o0 <- io $ fromIntegral ret_code
        RGFW.rGFW_deinit
        return o0