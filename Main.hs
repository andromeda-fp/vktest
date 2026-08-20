{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Exception      (bracket)
import Data.Bits              ((.|.), (.&.))
import Data.ByteString        (ByteString)
import Data.Coerce            (coerce)
import Data.Int               (Int32)
import Data.List              ((\\))
import Data.Vector            (Vector)
import Data.Word              (Word32)
import FIR                    (compileTo, runCompilationsTH)
import Foreign.C
import Foreign.C.ConstPtr     (ConstPtr(..))
import Foreign.Marshal.Alloc  (alloca)
import Foreign.Marshal.Array  (advancePtr)
import Foreign.Ptr            (Ptr)
import Foreign.Storable       (peek)
import Unsafe.Coerce          (unsafeCoerce)
import Vulkan.CStruct.Extends (SomeStruct(..))
import Vulkan.Zero            (zero)

import qualified Data.ByteString                    as BS
import qualified Data.ByteString.Char8              as BSC
import qualified Data.Vector                        as V
import qualified RGFW                               as RGFW
import qualified Shaders                            as Shaders
import qualified Vulkan.Core10                      as Vk
import qualified Vulkan.Core12                      as Vk
import qualified Vulkan.Core13                      as Vk
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

vert = $( runCompilationsTH [("Fragment Shader", compileTo Shaders.vertPath [] Shaders.vertex)] )
frag = $( runCompilationsTH [("Fragment Shader", compileTo Shaders.fragPath [] Shaders.fragment)] )

data Consts = Consts
    { title          :: String
    , vkApiVersion   :: Word32
    , framesInFlight :: Word32
    }
    deriving Show

consts =
    Consts
    { title          = "game title"
    , vkApiVersion   = Vk.API_VERSION_1_3
    , framesInFlight = 2
    }

-- for a given physical device and surface
data QueriedData = QueriedData
    { surfaceCapabilities      :: Vk.SurfaceCapabilitiesKHR
    , surfaceSupport           :: [Int]
    , surfaceFormats           :: Vector Vk.SurfaceFormatKHR
    , queueFamilyProperties    :: Vector Vk.QueueFamilyProperties
    , extensionProperties      :: Vector Vk.ExtensionProperties
    , physicalDeviceProperties :: Vk.PhysicalDeviceProperties
    }
    deriving Show

instanceConfig :: Vector ByteString -> Vk.InstanceCreateInfo '[]
instanceConfig exts =
    zero { Vk.applicationInfo = Just (zero :: Vk.ApplicationInfo) { Vk.apiVersion = consts.vkApiVersion }
         , Vk.enabledExtensionNames = exts
         }

deviceConfig :: Word32 -> Vk.DeviceCreateInfo '[Vk.PhysicalDeviceVulkan13Features]
deviceConfig gqueueIndex =
    zero { Vk.next = (vkFeatures13, ())
         , Vk.queueCreateInfos = V.singleton $ SomeStruct zero { Vk.queueFamilyIndex = gqueueIndex
                                                               , Vk.queuePriorities = V.singleton 1
                                                               }
         , Vk.enabledExtensionNames = extensions
         }

vkFeatures13 :: Vk.PhysicalDeviceVulkan13Features
vkFeatures13 =
    zero { Vk.synchronization2 = True
         , Vk.dynamicRendering = True
         }

commandPoolConfig :: Word32 -> Vk.CommandPoolCreateInfo '[]
commandPoolConfig gqueueIndex =
    zero { Vk.queueFamilyIndex = gqueueIndex
         , Vk.flags = Vk.COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
         }

commandBufferConfig :: Vk.CommandPool -> Vk.CommandBufferAllocateInfo
commandBufferConfig gpool =
    zero { Vk.commandPool = gpool
         , Vk.level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY
         , Vk.commandBufferCount = consts.framesInFlight
         }

swapchainConfig :: QueriedData -> Vk.SurfaceKHR -> Vk.SwapchainCreateInfoKHR '[]
swapchainConfig q surface =
    zero { Vk.compositeAlpha = Vk.COMPOSITE_ALPHA_OPAQUE_BIT_KHR
         , Vk.imageArrayLayers = 1
         , Vk.imageColorSpace = Vk.COLORSPACE_SRGB_NONLINEAR_KHR
         , Vk.imageExtent = q.surfaceCapabilities.currentExtent
         , Vk.imageFormat = (V.head q.surfaceFormats).format
         , Vk.imageUsage = Vk.IMAGE_USAGE_COLOR_ATTACHMENT_BIT
         , Vk.minImageCount = q.surfaceCapabilities.minImageCount
         , Vk.presentMode = Vk.PRESENT_MODE_FIFO_KHR
         , Vk.preTransform = Vk.SURFACE_TRANSFORM_IDENTITY_BIT_KHR
         , Vk.surface = surface
         }

imageViewConfigs :: Vector Vk.Image -> QueriedData -> Vector (Vk.ImageViewCreateInfo '[])
imageViewConfigs images q =
    V.map (\image -> (zero :: Vk.ImageViewCreateInfo '[]) { Vk.image = image
                                                          , Vk.viewType = Vk.IMAGE_VIEW_TYPE_2D
                                                          , Vk.subresourceRange = zero { Vk.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT
                                                                                       , Vk.levelCount = Vk.REMAINING_MIP_LEVELS
                                                                                       , Vk.layerCount = Vk.REMAINING_ARRAY_LAYERS
                                                                                       }
                                                          , Vk.format = (V.head q.surfaceFormats).format
                                                          }) images

pipelineLayoutConfig :: Vk.PipelineLayoutCreateInfo
pipelineLayoutConfig =
    zero

graphicsPipelineConfig :: QueriedData -> Vk.ShaderModule -> Vk.ShaderModule -> Vk.PipelineLayout -> Vk.GraphicsPipelineCreateInfo '[Vk.PipelineRenderingCreateInfo]
graphicsPipelineConfig q vertMod fragMod pipelineLayout =
    zero { Vk.next = (pipelineRenderingConfig q, ())
         , Vk.stages = V.map (SomeStruct) $ V.fromList [ zero { Vk.stage = Vk.SHADER_STAGE_VERTEX_BIT
                                                              , Vk.module' = vertMod
                                                              , Vk.name = "main"
                                                              }
                                                       , zero { Vk.stage = Vk.SHADER_STAGE_FRAGMENT_BIT
                                                              , Vk.module' = fragMod
                                                              , Vk.name = "main"
                                                              }]
         , Vk.vertexInputState = Just zero
         , Vk.inputAssemblyState = Just zero { Vk.topology = Vk.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST }
         , Vk.tessellationState = Nothing
         , Vk.viewportState = Just $ SomeStruct zero { Vk.viewports = V.fromList [zero { Vk.x = 0
                                                                                       , Vk.y = 0
                                                                                       , Vk.width = fromIntegral q.surfaceCapabilities.currentExtent.width
                                                                                       , Vk.height = fromIntegral q.surfaceCapabilities.currentExtent.height
                                                                                       , Vk.minDepth = 0
                                                                                       , Vk.maxDepth = 1
                                                                                       }]
                                                     , Vk.scissors = V.fromList [(zero :: Vk.Rect2D) { Vk.offset = zero
                                                                                                     , Vk.extent = q.surfaceCapabilities.currentExtent
                                                                                                     }]
                                                     }

         , Vk.rasterizationState = Just $ SomeStruct zero { Vk.lineWidth = 1 }
         , Vk.multisampleState = Just $ SomeStruct zero { Vk.rasterizationSamples = Vk.SAMPLE_COUNT_1_BIT }
         , Vk.depthStencilState = Just zero
         , Vk.colorBlendState = Just $ SomeStruct zero { Vk.attachments = V.singleton zero { Vk.colorWriteMask = Vk.COLOR_COMPONENT_R_BIT .|. Vk.COLOR_COMPONENT_G_BIT .|. Vk.COLOR_COMPONENT_B_BIT .|. Vk.COLOR_COMPONENT_A_BIT }}
         , Vk.dynamicState = Just zero { Vk.dynamicStates = V.fromList [Vk.DYNAMIC_STATE_VIEWPORT, Vk.DYNAMIC_STATE_SCISSOR] }
         , Vk.layout = pipelineLayout
         }

pipelineRenderingConfig :: QueriedData -> Vk.PipelineRenderingCreateInfo
pipelineRenderingConfig q =
    zero { Vk.colorAttachmentFormats = V.singleton (V.head q.surfaceFormats).format }

main :: IO ()
main = withRGFW "rgfw instance title" (fromIntegral $ RGFW.unwrapRGFW_initFlags_enum RGFW.RGFW_initVulkan) $ \_ -> do
    exts <- alloca $ \extension_count -> do
        exts <- RGFW.rGFW_getRequiredInstanceExtensions_Vulkan extension_count
        cexts <- peek extension_count
        vexts <- processExtensions cexts exts V.empty
        return vexts

    Vk.withInstance (instanceConfig exts) Nothing bracket $ \i -> do

    withWindow "test window" 0 0 width height ((fromIntegral (RGFW.unwrapRGFW_windowFlags_enum RGFW.RGFW_windowCenter)) .|. (fromIntegral (RGFW.unwrapRGFW_windowFlags_enum RGFW.RGFW_windowNoResize))) $ \window -> do

    surface :: Vk.SurfaceKHR <- alloca $ \surfacePtr -> do
        _ <- RGFW.rGFW_window_createSurface_Vulkan window (coerce $ Vk.instanceHandle i) surfacePtr
        return . unsafeCoerce =<< peek surfacePtr
    (_, pdevs) <- Vk.enumeratePhysicalDevices i
    (pdev, q) <- pickPhysicalDevice pdevs surface
    let gqueueIndex = fromIntegral $ head $ getGraphicsQueues q

    Vk.withDevice pdev (deviceConfig gqueueIndex) Nothing bracket $ \dev -> do
    gqueue <- Vk.getDeviceQueue dev gqueueIndex 0

    Vk.withCommandPool dev (commandPoolConfig gqueueIndex) Nothing bracket $ \gpool -> do
    Vk.withSwapchainKHR dev (swapchainConfig q surface) Nothing bracket $ \swapchain -> do
    
    (_, images) <- Vk.getSwapchainImagesKHR dev swapchain
    withImageViews dev (imageViewConfigs images q) Nothing $ \imageViews -> do

    withSemaphores dev (V.fromList (take (fromIntegral consts.framesInFlight) (repeat zero))) Nothing $ \sImageAcquired -> do
    withSemaphores dev (V.fromList (take (V.length images) (repeat zero))) Nothing $ \sRenderFinisheds -> do
    withFences dev (V.fromList (take (fromIntegral consts.framesInFlight) (repeat ((zero :: Vk.FenceCreateInfo '[]) { Vk.flags = Vk.FENCE_CREATE_SIGNALED_BIT })))) Nothing $ \fences -> do

    Vk.withCommandBuffers dev (commandBufferConfig gpool) bracket $ \cbuffers -> do

    rawVert <- BS.readFile Shaders.vertPath
    rawFrag <- BS.readFile Shaders.fragPath
    withShaderModules dev (V.fromList [ zero { Vk.code = rawVert }, zero { Vk.code = rawFrag } ]) Nothing $ \mods -> do

    let vertMod = V.head mods
    let fragMod = V.head $ V.tail mods
    Vk.withPipelineLayout dev (pipelineLayoutConfig) Nothing bracket $ \pipelineLayout -> do

    Vk.withGraphicsPipelines dev zero (V.singleton $ SomeStruct (graphicsPipelineConfig q vertMod fragMod pipelineLayout)) Nothing bracket $ \(_, pipelines) -> do

    let pipeline = V.head pipelines
    gameloop' dev q pipeline swapchain imageViews images cbuffers gqueue sImageAcquired sRenderFinisheds fences 0 window 0


gameloop' :: Vk.Device -> QueriedData -> Vk.Pipeline -> Vk.SwapchainKHR -> Vector Vk.ImageView -> Vector Vk.Image -> Vector Vk.CommandBuffer -> Vk.Queue -> Vector Vk.Semaphore -> Vector Vk.Semaphore -> Vector Vk.Fence -> Int -> Ptr RGFW.RGFW_window -> RGFW.RGFW_bool -> IO ()
gameloop' dev q pipeline swapchain imageViews images cbuffers queue sImageAcquireds sRenderFinisheds fences frameIndex window 0 = do
    let cbuffer = (V.!) cbuffers frameIndex
        sImageAcquired = (V.!) sImageAcquireds frameIndex
        fence = V.singleton $ (V.!) fences frameIndex
    _ <- Vk.waitForFences dev fence True 18446744073709551615
    Vk.resetFences dev fence
    (_, imageIndex) <- Vk.acquireNextImageKHR dev swapchain 18446744073709551615 sImageAcquired zero
    let imageView = (V.!) imageViews $ fromIntegral imageIndex
        image = (V.!) images $ fromIntegral imageIndex
        sRenderFinished = (V.!) sRenderFinisheds $ fromIntegral imageIndex
    Vk.resetCommandBuffer cbuffer zero
    Vk.useCommandBuffer cbuffer ((zero :: Vk.CommandBufferBeginInfo '[]) { Vk.flags = Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT }) $ do
        Vk.cmdPipelineBarrier2 cbuffer $ zero { Vk.imageMemoryBarriers = V.singleton $ SomeStruct zero { Vk.srcStageMask = Vk.PIPELINE_STAGE_2_NONE
                                                                                                       , Vk.srcAccessMask = Vk.ACCESS_2_NONE
                                                                                                       , Vk.dstStageMask = Vk.PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT
                                                                                                       , Vk.dstAccessMask = Vk.ACCESS_2_COLOR_ATTACHMENT_READ_BIT .|. Vk.ACCESS_2_COLOR_ATTACHMENT_WRITE_BIT
                                                                                                       , Vk.oldLayout = Vk.IMAGE_LAYOUT_UNDEFINED
                                                                                                       , Vk.newLayout = Vk.IMAGE_LAYOUT_ATTACHMENT_OPTIMAL
                                                                                                       , Vk.image = image
                                                                                                       , Vk.subresourceRange = zero { Vk.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT
                                                                                                                                    , Vk.levelCount = 1
                                                                                                                                    , Vk.layerCount = 1
                                                                                                                                    }
                                                                                                       }
                                              }
        Vk.cmdUseRendering cbuffer (zero { Vk.renderArea = zero { Vk.extent = q.surfaceCapabilities.currentExtent }
                                         , Vk.layerCount = 1
                                         , Vk.colorAttachments = V.singleton $ (SomeStruct) zero { Vk.imageView = imageView
                                                                                                 , Vk.imageLayout = Vk.IMAGE_LAYOUT_ATTACHMENT_OPTIMAL
                                                                                                 , Vk.loadOp = Vk.ATTACHMENT_LOAD_OP_CLEAR
                                                                                                 , Vk.storeOp = Vk.ATTACHMENT_STORE_OP_STORE
                                                                                                 , Vk.clearValue = Vk.Color $ Vk.Float32 0 0 1 1
                                                                                                 }
                                         }) $ do
            Vk.cmdSetViewport cbuffer 0 $ V.singleton zero { Vk.x = 0
                                                           , Vk.y = 0
                                                           , Vk.width = fromIntegral q.surfaceCapabilities.currentExtent.width
                                                           , Vk.height = fromIntegral q.surfaceCapabilities.currentExtent.height
                                                           , Vk.minDepth = 0
                                                           , Vk.maxDepth = 1
                                                           }
            Vk.cmdSetScissor cbuffer 0 $ V.singleton zero { Vk.offset = zero
                                                          , Vk.extent = q.surfaceCapabilities.currentExtent
                                                          }
            Vk.cmdBindPipeline cbuffer Vk.PIPELINE_BIND_POINT_GRAPHICS pipeline
            Vk.cmdDraw cbuffer 3 1 0 0
        Vk.cmdPipelineBarrier2 cbuffer $ zero { Vk.imageMemoryBarriers = V.singleton $ SomeStruct zero { Vk.srcStageMask = Vk.PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT
                                                                                                       , Vk.srcAccessMask = Vk.ACCESS_2_COLOR_ATTACHMENT_WRITE_BIT
                                       	                                                               , Vk.dstStageMask = Vk.PIPELINE_STAGE_2_NONE
                                                                                                       , Vk.dstAccessMask = Vk.ACCESS_2_NONE
                                                                                                       , Vk.oldLayout = Vk.IMAGE_LAYOUT_ATTACHMENT_OPTIMAL
                                                                                                       , Vk.newLayout = Vk.IMAGE_LAYOUT_PRESENT_SRC_KHR
                                                                                                       , Vk.image = image
                                                                                                       , Vk.subresourceRange = zero { Vk.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT
                                                                                                                                    , Vk.levelCount = 1
                                                                                                                                    , Vk.layerCount = 1
                                                                                                                                    }
                                                                                                      	}
                                              }
    Vk.queueSubmit2 queue (V.singleton (SomeStruct zero { Vk.waitSemaphoreInfos = V.singleton $ zero { Vk.semaphore = sImageAcquired
                                                                                                     , Vk.stageMask = Vk.PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT
                                                                                                     }
                                                        , Vk.commandBufferInfos = V.singleton $ SomeStruct zero { Vk.commandBuffer = Vk.commandBufferHandle cbuffer }
                                                        , Vk.signalSemaphoreInfos = V.singleton $ zero { Vk.semaphore = sRenderFinished
                                                                                                       , Vk.stageMask = Vk.PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT
                                                                                                       }
                                                        })) $ V.head fence
    _ <- Vk.queuePresentKHR queue (zero { Vk.waitSemaphores = V.singleton sRenderFinished
                                        , Vk.swapchains = V.singleton swapchain
                                        , Vk.imageIndices = V.singleton imageIndex
                                        })
    RGFW.rGFW_pollEvents
    gameloop' dev q pipeline swapchain imageViews images cbuffers queue sImageAcquireds sRenderFinisheds fences (mod (frameIndex + 1) (fromIntegral consts.framesInFlight)) window =<< RGFW.rGFW_window_shouldClose window
gameloop' dev _ _ _ _ _ _ _ _ _ _ _ _ _ =
    Vk.deviceWaitIdle dev

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
    else if [] /= ((V.toList extensions) \\ (V.toList $ V.map (\p -> p.extensionName) q.extensionProperties))
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
    _ <- mapM (\mod -> Vk.destroyShaderModule dev mod alloc) mods
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