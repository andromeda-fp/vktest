{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
import FIR                    (compileTo, runCompilationsTH, CompilerFlag(SPIRV), Version(..))
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

vert = $( runCompilationsTH [("Fragment Shader", compileTo Shaders.vertPath [SPIRV (Version 1 0)] Shaders.vertex)] )
frag = $( runCompilationsTH [("Fragment Shader", compileTo Shaders.fragPath [SPIRV (Version 1 0)] Shaders.fragment)] )

main :: IO ()
main = withRGFW "rgfw instance title" (fromIntegral $ RGFW.unwrapRGFW_initFlags_enum RGFW.RGFW_initVulkan) $ \_ -> do
    exts <- alloca $ \extension_count -> do
        exts <- RGFW.rGFW_getRequiredInstanceExtensions_Vulkan extension_count
        cexts <- peek extension_count
        vexts <- processExtensions cexts exts V.empty
        return vexts
    Vk.withInstance (zero { Vk.applicationInfo = Just (zero :: Vk.ApplicationInfo) { Vk.apiVersion = Vk.API_VERSION_1_0 }
                          , Vk.enabledExtensionNames = exts
                          , Vk.enabledLayerNames = layers
                          }) Nothing bracket $ \i -> do
        withWindow "test window" 0 0 width height ((fromIntegral (RGFW.unwrapRGFW_windowFlags_enum RGFW.RGFW_windowCenter)) .|. (fromIntegral (RGFW.unwrapRGFW_windowFlags_enum RGFW.RGFW_windowNoResize))) $ \window -> do
            surface :: Vk.SurfaceKHR <- alloca $ \surfacePtr -> do
                _ <- RGFW.rGFW_window_createSurface_Vulkan window (coerce $ Vk.instanceHandle i) surfacePtr
                return . unsafeCoerce =<< peek surfacePtr
            (_, pdevs) <- Vk.enumeratePhysicalDevices i
            pdev <- pickPhysicalDevice pdevs surface
            qfprops <- Vk.getPhysicalDeviceQueueFamilyProperties pdev
            let gqueueIndex = fromIntegral $ head $ getGraphicsQueues qfprops
            pqueueIndex <- return . fromIntegral . head =<< getSurfaceSupport pdev surface
            if pqueueIndex /= gqueueIndex then putStrLn "queues not the same index not supported, undefined ahead" else return ()
            Vk.withDevice pdev (zero { Vk.queueCreateInfos = V.singleton $ SomeStruct zero { Vk.queueFamilyIndex = gqueueIndex
                                                                                           , Vk.queuePriorities = V.singleton 1
                                                                                           }
                                     , Vk.enabledExtensionNames = extensions
                                     }) Nothing bracket $ \dev -> do
                gqueue <- Vk.getDeviceQueue dev gqueueIndex 0
                Vk.withCommandPool dev (zero { Vk.queueFamilyIndex = gqueueIndex
                                             , Vk.flags = Vk.COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
                                             }) Nothing bracket $ \gpool -> do
                    Vk.withCommandBuffers dev (zero { Vk.commandPool = gpool
                                                    , Vk.level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY
                                                    , Vk.commandBufferCount = 1
                                                    }) bracket $ \cbuffers -> do
                        let gcbuffer = V.head cbuffers
                        caps <- Vk.getPhysicalDeviceSurfaceCapabilitiesKHR pdev surface
                        (_, forms) <- Vk.getPhysicalDeviceSurfaceFormatsKHR pdev surface
                        Vk.withSwapchainKHR dev zero { Vk.clipped = True
                                                     , Vk.compositeAlpha = Vk.COMPOSITE_ALPHA_OPAQUE_BIT_KHR
                                                     , Vk.imageArrayLayers = 1
                                                     , Vk.imageColorSpace = (V.head forms).colorSpace
                                                     , Vk.imageExtent = caps.currentExtent
                                                     , Vk.imageFormat = (V.head forms).format -- TODO fetch the best format
                                                     , Vk.imageSharingMode = Vk.SHARING_MODE_EXCLUSIVE
                                                     , Vk.imageUsage = Vk.IMAGE_USAGE_COLOR_ATTACHMENT_BIT
                                                     , Vk.minImageCount = caps.minImageCount
                                                     , Vk.presentMode = Vk.PRESENT_MODE_FIFO_KHR
                                                     , Vk.preTransform = caps.currentTransform
                                                     , Vk.queueFamilyIndices = V.singleton gqueueIndex -- TODO what if pqueue and gqueue are different!?
                                                     , Vk.surface = surface
                                                     } Nothing bracket $ \swapchain -> do
                            (_, images) <- Vk.getSwapchainImagesKHR dev swapchain
                            withImageViews dev (V.map (\image -> (zero :: Vk.ImageViewCreateInfo '[]) { Vk.image = image
                                                                                                      , Vk.viewType = Vk.IMAGE_VIEW_TYPE_2D
                                                                                                      , Vk.subresourceRange = zero { Vk.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT
                                                                                                                                   , Vk.levelCount = Vk.REMAINING_MIP_LEVELS
                                                                                                                                   , Vk.layerCount = Vk.REMAINING_ARRAY_LAYERS
                                                                                                                                   }
                                                                                                      , Vk.format = (V.head forms).format -- TODO fetch the best format
                                                                                                      }) images) Nothing $ \imageViews -> do
                                let colorAttachmentRef = zero { Vk.attachment = 0
                                                              , Vk.layout = Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
                                                              }
                                let colorAttachment = zero { Vk.format = (V.head forms).format -- TODO fetch the best format
                                                           , Vk.samples = Vk.SAMPLE_COUNT_1_BIT
                                                           , Vk.loadOp = Vk.ATTACHMENT_LOAD_OP_CLEAR
                                                           , Vk.storeOp = Vk.ATTACHMENT_STORE_OP_STORE
                                                           , Vk.stencilLoadOp = Vk.ATTACHMENT_LOAD_OP_DONT_CARE
                                                           , Vk.stencilStoreOp = Vk.ATTACHMENT_STORE_OP_DONT_CARE
                                                           , Vk.initialLayout = Vk.IMAGE_LAYOUT_UNDEFINED
                                                           , Vk.finalLayout = Vk.IMAGE_LAYOUT_PRESENT_SRC_KHR
                                                           } 
                                Vk.withRenderPass dev zero { Vk.attachments = V.singleton colorAttachment
                                                           , Vk.subpasses = V.singleton zero { Vk.pipelineBindPoint = Vk.PIPELINE_BIND_POINT_GRAPHICS
                                                                                             , Vk.colorAttachments = V.singleton colorAttachmentRef
                                                                                             }
                                                           , Vk.dependencies = V.singleton zero { Vk.srcSubpass = Vk.SUBPASS_EXTERNAL
                                                                                                , Vk.dstSubpass = 0
                                                                                                , Vk.srcStageMask = Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
                                                                                                , Vk.srcAccessMask = Vk.ACCESS_NONE
                                                                                                , Vk.dstStageMask = Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
                                                                                                , Vk.dstAccessMask = Vk.ACCESS_COLOR_ATTACHMENT_WRITE_BIT
                                                                                                }
                                                           } Nothing bracket $ \pass -> do
                                    let fb = zero { Vk.height = caps.currentExtent.height
                                                  , Vk.width = caps.currentExtent.width
                                                  , Vk.renderPass = pass
                                                  , Vk.layers = 1
                                                  }
                                    let infos = V.map (\image -> (fb :: Vk.FramebufferCreateInfo '[]) { Vk.attachments = V.singleton image }) imageViews
                                    withFramebuffers dev infos Nothing $ \fbs -> do
                                        rawVert <- BS.readFile Shaders.vertPath
                                        rawFrag <- BS.readFile Shaders.fragPath
                                        withShaderModules dev (V.fromList [ zero { Vk.code = rawVert }, zero { Vk.code = rawFrag } ]) Nothing $ \mods -> do
                                            let vertMod = V.head mods
                                            let fragMod = V.head $ V.tail mods
                                            Vk.withPipelineLayout dev zero Nothing bracket $ \pipelineLayout -> do
                                                Vk.withGraphicsPipelines dev zero (V.map (SomeStruct) $ V.fromList [zero { Vk.stages = V.map (SomeStruct) $ V.fromList [ zero { Vk.stage = Vk.SHADER_STAGE_VERTEX_BIT
                                                                                                                                                                              , Vk.module' = vertMod
                                                                                                                                                                              , Vk.name = "main"
                                                                                                                                                                              }
                                                                                                                                                                       , zero { Vk.stage = Vk.SHADER_STAGE_FRAGMENT_BIT
                                                                                                                                                                              , Vk.module' = fragMod
                                                                                                                                                                              , Vk.name = "main"
                                                                                                                                                                              }]
                                                                                                                         , Vk.vertexInputState = Just zero
                                                                                                                         , Vk.inputAssemblyState = Just zero { Vk.topology = Vk.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST }
                                                                                                                         , Vk.tessellationState = Just zero
                                                                                                                         , Vk.viewportState = Just $ SomeStruct zero { Vk.viewports = V.fromList [zero { Vk.x = 0
                                                                                                                                                                                                       , Vk.y = 0
                                                                                                                                                                                                       , Vk.width = fromIntegral caps.currentExtent.width
                                                                                                                                                                                                       , Vk.height = fromIntegral caps.currentExtent.height
                                                                                                                                                                                                       , Vk.minDepth = 0
                                                                                                                                                                                                       , Vk.maxDepth = 1
                                                                                                                                                                                                       }]
                                                                                                                                                                     , Vk.scissors = V.fromList [(zero :: Vk.Rect2D) { Vk.offset = zero
                                                                                                                                                                                                                     , Vk.extent = caps.currentExtent
                                                                                                                                                                                                                     }]
                                                                                                                                                                     }
                                                                                                                         , Vk.rasterizationState = Just $ SomeStruct zero { Vk.depthClampEnable = False
                                                                                                                                                                          , Vk.rasterizerDiscardEnable = False
                                                                                                                                                                          , Vk.polygonMode = Vk.POLYGON_MODE_FILL
                                                                                                                                                                          , Vk.cullMode = Vk.CULL_MODE_NONE
                                                                                                                                                                          , Vk.frontFace = Vk.FRONT_FACE_CLOCKWISE
                                                                                                                                                                          , Vk.depthBiasEnable = False
                                                                                                                                                                          , Vk.lineWidth = 1
                                                                                                                                                                          }
                                                                                                                         , Vk.multisampleState = Just $ SomeStruct zero { Vk.sampleShadingEnable = False
                                                                                                                                                                        , Vk.rasterizationSamples = Vk.SAMPLE_COUNT_1_BIT
                                                                                                                                                                        }
                                                                                                                         , Vk.depthStencilState = Nothing
                                                                                                                         , Vk.colorBlendState = Just $ SomeStruct zero { Vk.attachments = V.singleton zero { Vk.blendEnable = False }}
                                                                                                                         , Vk.layout = pipelineLayout
                                                                                                                         , Vk.renderPass = pass
                                                                                                                         , Vk.subpass = 0
                                                                                                                         }]) Nothing bracket $ \(_, pipelines) -> do
                                                    let pipeline = V.head pipelines
                                                    Vk.withSemaphore dev zero Nothing bracket $ \sImageAvailable -> do
                                                        withSemaphores dev (V.fromList (take (V.length imageViews) (repeat zero))) Nothing $ \sRenderFinisheds -> do
                                                            Vk.withFence dev (zero { Vk.flags = Vk.FENCE_CREATE_SIGNALED_BIT }) Nothing bracket $ \fInFlight -> do
                                                                ret <- gameloop dev swapchain gcbuffer gqueue fbs pass pipeline window (V.cons sImageAvailable sRenderFinisheds) fInFlight 0
                                                                putStr "gameloop returned"
                                                                Vk.deviceWaitIdle dev

gameloop :: Vk.Device -> Vk.SwapchainKHR -> Vk.CommandBuffer -> Vk.Queue -> Vector Vk.Framebuffer -> Vk.RenderPass -> Vk.Pipeline -> Ptr RGFW.RGFW_window -> Vector Vk.Semaphore -> Vk.Fence -> RGFW.RGFW_bool -> IO ()
gameloop dev swapchain gcbuffer queue fbs pass pipeline window ss f 0 = do
    RGFW.rGFW_pollEvents
    let fs = V.singleton f
    let sImageAvailable = V.head ss
    let sRenderFinisheds = V.tail ss
    _ <- Vk.waitForFences dev fs True 18446744073709551615
    Vk.resetFences dev fs
    (_, imageIndex) <- Vk.acquireNextImageKHR dev swapchain 18446744073709551615 sImageAvailable zero
    Vk.resetCommandBuffer gcbuffer zero
    Vk.useCommandBuffer gcbuffer zero $ do
        Vk.cmdUseRenderPass gcbuffer (zero { Vk.renderPass = pass
                                           , Vk.framebuffer = (V.!) fbs $ fromIntegral imageIndex
                                           , Vk.renderArea = (zero :: Vk.Rect2D) { Vk.offset = zero
                                                                                 , Vk.extent = zero { Vk.width = fromIntegral width
                                                                                                    , Vk.height = fromIntegral height
                                                                                                    }
                                                                                 }
                                           , Vk.clearValues = V.singleton (Vk.Color $ Vk.Float32 1 0 1 1)
                                           }) Vk.SUBPASS_CONTENTS_INLINE $ do
            Vk.cmdBindPipeline gcbuffer Vk.PIPELINE_BIND_POINT_GRAPHICS pipeline
            Vk.cmdDraw gcbuffer 3 1 0 0
    Vk.queueSubmit queue (V.singleton (SomeStruct zero { Vk.waitSemaphores = V.singleton sImageAvailable
                                                       , Vk.waitDstStageMask = V.singleton Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
                                                       , Vk.commandBuffers = V.singleton $ Vk.commandBufferHandle gcbuffer
                                                       , Vk.signalSemaphores = V.singleton $ (V.!) sRenderFinisheds (fromIntegral imageIndex)
                                                       })) f
    _ <- Vk.queuePresentKHR queue (zero { Vk.waitSemaphores = V.singleton $ (V.!) sRenderFinisheds (fromIntegral imageIndex)
                                        , Vk.swapchains = V.singleton swapchain
                                        , Vk.imageIndices = V.singleton imageIndex
                                        })
    gameloop dev swapchain gcbuffer queue fbs pass pipeline window ss f =<< RGFW.rGFW_window_shouldClose window
gameloop _ _ _ _ _ _ _ _ _ _ _ = return ()

pickPhysicalDevice :: Vector Vk.PhysicalDevice -> Vk.SurfaceKHR -> IO Vk.PhysicalDevice
pickPhysicalDevice pdevs surface = do
    validpdevs <- (V.filterM (\o -> isValidPhysicalDevice o surface) pdevs)
    pickPhysicalDevice' validpdevs V.empty

pickPhysicalDevice' :: Vector Vk.PhysicalDevice -> Vector Vk.PhysicalDevice -> IO Vk.PhysicalDevice
pickPhysicalDevice' pdevs opts =
    if V.null pdevs
    then do
        props <- Vk.getPhysicalDeviceProperties $ V.head opts
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

withSemaphores :: Vk.Device -> Vector (Vk.SemaphoreCreateInfo '[]) -> Maybe Vk.AllocationCallbacks -> (Vector Vk.Semaphore -> IO r) -> IO r
withSemaphores dev infos alloc io = do
    ss <- mapM (\info -> Vk.createSemaphore dev info alloc) infos
    o0 <- io ss
    _ <- mapM (\s -> Vk.destroySemaphore dev s alloc) ss
    return o0

withFramebuffers :: Vk.Device -> Vector (Vk.FramebufferCreateInfo '[]) -> Maybe Vk.AllocationCallbacks -> (Vector Vk.Framebuffer -> IO r) -> IO r
withFramebuffers dev infos alloc io = do
    fbs <- mapM (\info -> Vk.createFramebuffer dev info alloc) infos
    o0 <- io fbs
    _ <- mapM (\fb -> Vk.destroyFramebuffer dev fb alloc) fbs
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