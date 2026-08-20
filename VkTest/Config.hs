{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module VkTest.Config where

import Data.Bits              ((.|.))
import Data.ByteString        (ByteString)
import Data.Int               (Int32)
import Data.Vector            (Vector)
import Data.Word              (Word32)
import FIR                    (compileTo, runCompilationsTH, ModuleRequirements)
import Vulkan.CStruct.Extends (SomeStruct(..))
import Vulkan.Zero            (zero)

import qualified Data.ByteString.Char8              as BSC
import qualified Data.Vector                        as V
import qualified VkTest.Shaders                     as Shaders
import qualified Vulkan.Core10                      as Vk
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

vert :: Either a ModuleRequirements
vert = $( runCompilationsTH [("Fragment Shader", compileTo Shaders.vertPath [] Shaders.vertex)] )

frag :: Either a ModuleRequirements
frag = $( runCompilationsTH [("Fragment Shader", compileTo Shaders.fragPath [] Shaders.fragment)] )

data Consts = Consts
    { title          :: String
    , vkApiVersion   :: Word32
    , framesInFlight :: Word32
    }
    deriving Show

consts :: Consts
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

instance' :: Vector ByteString -> Vk.InstanceCreateInfo '[]
instance' exts =
    zero { Vk.applicationInfo = Just (zero :: Vk.ApplicationInfo) { Vk.apiVersion = consts.vkApiVersion }
         , Vk.enabledExtensionNames = exts
         }

device :: Word32 -> Vk.DeviceCreateInfo '[Vk.PhysicalDeviceVulkan13Features]
device gqueueIndex =
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

commandPool :: Word32 -> Vk.CommandPoolCreateInfo '[]
commandPool gqueueIndex =
    zero { Vk.queueFamilyIndex = gqueueIndex
         , Vk.flags = Vk.COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
         }

commandBuffer :: Vk.CommandPool -> Vk.CommandBufferAllocateInfo
commandBuffer gpool =
    zero { Vk.commandPool = gpool
         , Vk.level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY
         , Vk.commandBufferCount = consts.framesInFlight
         }

swapchain :: QueriedData -> Vk.SurfaceKHR -> Vk.SwapchainCreateInfoKHR '[]
swapchain q surface =
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

imageViews :: Vector Vk.Image -> QueriedData -> Vector (Vk.ImageViewCreateInfo '[])
imageViews images q =
    V.map (\image -> (zero :: Vk.ImageViewCreateInfo '[]) { Vk.image = image
                                                          , Vk.viewType = Vk.IMAGE_VIEW_TYPE_2D
                                                          , Vk.subresourceRange = zero { Vk.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT
                                                                                       , Vk.levelCount = Vk.REMAINING_MIP_LEVELS
                                                                                       , Vk.layerCount = Vk.REMAINING_ARRAY_LAYERS
                                                                                       }
                                                          , Vk.format = (V.head q.surfaceFormats).format
                                                          }) images

pipelineLayout :: Vk.PipelineLayoutCreateInfo
pipelineLayout =
    zero

graphicsPipeline :: QueriedData -> Vk.ShaderModule -> Vk.ShaderModule -> Vk.PipelineLayout -> Vk.GraphicsPipelineCreateInfo '[Vk.PipelineRenderingCreateInfo]
graphicsPipeline q vertMod fragMod pipelineLayout' =
    zero { Vk.next = (pipelineRendering q, ())
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
         , Vk.layout = pipelineLayout'
         }

pipelineRendering :: QueriedData -> Vk.PipelineRenderingCreateInfo
pipelineRendering q =
    zero { Vk.colorAttachmentFormats = V.singleton (V.head q.surfaceFormats).format }

preRenderPipelineBarrier :: Vk.Image -> Vk.DependencyInfo '[]
preRenderPipelineBarrier image =
    zero { Vk.imageMemoryBarriers = V.singleton $ SomeStruct zero { Vk.srcStageMask = Vk.PIPELINE_STAGE_2_NONE
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

postRenderPipelineBarrier :: Vk.Image -> Vk.DependencyInfo '[]
postRenderPipelineBarrier image =
    zero { Vk.imageMemoryBarriers = V.singleton $ SomeStruct zero { Vk.srcStageMask = Vk.PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT
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

rendering :: QueriedData -> Vk.ImageView -> Vk.RenderingInfo '[]
rendering q imageView =
    zero { Vk.renderArea = zero { Vk.extent = q.surfaceCapabilities.currentExtent }
         , Vk.layerCount = 1
         , Vk.colorAttachments = V.singleton $ (SomeStruct) zero { Vk.imageView = imageView
                                                                 , Vk.imageLayout = Vk.IMAGE_LAYOUT_ATTACHMENT_OPTIMAL
                                                                 , Vk.loadOp = Vk.ATTACHMENT_LOAD_OP_CLEAR
                                                                 , Vk.storeOp = Vk.ATTACHMENT_STORE_OP_STORE
                                                                 , Vk.clearValue = Vk.Color $ Vk.Float32 0 0 1 1
                                                                 }
         }

viewport :: QueriedData -> Vector Vk.Viewport
viewport q =
    V.singleton zero { Vk.x = 0
                     , Vk.y = 0
                     , Vk.width = fromIntegral q.surfaceCapabilities.currentExtent.width
                     , Vk.height = fromIntegral q.surfaceCapabilities.currentExtent.height
                     , Vk.minDepth = 0
                     , Vk.maxDepth = 1
                     }

scissor :: QueriedData -> Vector Vk.Rect2D
scissor q =
    V.singleton (zero :: Vk.Rect2D) { Vk.offset = zero
                                    , Vk.extent = q.surfaceCapabilities.currentExtent
                                    }

submitQueue :: Vk.CommandBuffer -> Vk.Semaphore -> Vk.Semaphore -> Vector (SomeStruct Vk.SubmitInfo2)
submitQueue cbuffer sImageAcquired sRenderFinished =
    V.singleton $ SomeStruct zero { Vk.waitSemaphoreInfos = V.singleton $ zero { Vk.semaphore = sImageAcquired
                                                                               , Vk.stageMask = Vk.PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT
                                                                               }
                                  , Vk.commandBufferInfos = V.singleton $ SomeStruct zero { Vk.commandBuffer = Vk.commandBufferHandle cbuffer }
                                  , Vk.signalSemaphoreInfos = V.singleton $ zero { Vk.semaphore = sRenderFinished
                                                                                 , Vk.stageMask = Vk.PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT
                                                                                 }
                                  }

present :: Vk.SwapchainKHR -> Word32 -> Vk.Semaphore -> Vk.PresentInfoKHR '[]
present swapchain' imageIndex sRenderFinished =
    zero { Vk.waitSemaphores = V.singleton sRenderFinished
         , Vk.swapchains = V.singleton swapchain'
         , Vk.imageIndices = V.singleton imageIndex
         }