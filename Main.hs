{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Exception      (bracket)
import Data.Bits              ((.|.))
import Data.Coerce            (coerce)
import Data.Vector            (Vector)
import Foreign.Marshal.Alloc  (alloca)
import Foreign.Ptr            (Ptr)
import Foreign.Storable       (peek)
import Unsafe.Coerce          (unsafeCoerce)
import Vulkan.CStruct.Extends (SomeStruct(..))
import Vulkan.Zero            (zero)

import VkTest.Config (QueriedData(..))

import qualified VkTest.Config  as Config
import qualified VkTest.Shaders as Shaders
import qualified VkTest.Util    as Util

import qualified Data.ByteString                    as BS
import qualified Data.Vector                        as V
import qualified RGFW                               as RGFW
import qualified Vulkan.Core10                      as Vk
import qualified Vulkan.Core13                      as Vk
import qualified Vulkan.Extensions.VK_KHR_surface   as Vk
import qualified Vulkan.Extensions.VK_KHR_swapchain as Vk

main :: IO ()
main = Util.withRGFW "rgfw instance title" (fromIntegral $ RGFW.unwrapRGFW_initFlags_enum RGFW.RGFW_initVulkan) $ \_ -> do
    exts <- alloca $ \extension_count -> do
        exts <- RGFW.rGFW_getRequiredInstanceExtensions_Vulkan extension_count
        cexts <- peek extension_count
        vexts <- Util.processExtensions cexts exts V.empty
        return vexts
    Vk.withInstance (Config.instance' exts) Nothing bracket $ \i -> do
    Util.withWindow "test window" 0 0 Config.width Config.height ((fromIntegral (RGFW.unwrapRGFW_windowFlags_enum RGFW.RGFW_windowCenter)) .|. (fromIntegral (RGFW.unwrapRGFW_windowFlags_enum RGFW.RGFW_windowFloating))) $ \window -> do
    surface :: Vk.SurfaceKHR <- alloca $ \surfacePtr -> do
        _ <- RGFW.rGFW_window_createSurface_Vulkan window (coerce $ Vk.instanceHandle i) surfacePtr
        return . unsafeCoerce =<< peek surfacePtr
    (_, pdevs) <- Vk.enumeratePhysicalDevices i
    (pdev, q) <- Util.pickPhysicalDevice pdevs surface
    let gqueueIndex = fromIntegral $ head $ Util.getGraphicsQueues q
    Vk.withDevice pdev (Config.device gqueueIndex) Nothing bracket $ \dev -> do
    gqueue <- Vk.getDeviceQueue dev gqueueIndex 0
    Vk.withCommandPool dev (Config.commandPool gqueueIndex) Nothing bracket $ \gpool -> do
    swapchain <- Vk.createSwapchainKHR dev (Config.swapchain q surface) Nothing
    (_, images) <- Vk.getSwapchainImagesKHR dev swapchain
    Util.withImageViews dev (Config.imageViews images q) Nothing $ \imageViews -> do
    Util.withSemaphores dev (V.fromList (take (fromIntegral Config.consts.framesInFlight) (repeat zero))) Nothing $ \sImageAcquired -> do
    Util.withSemaphores dev (V.fromList (take (V.length images) (repeat zero))) Nothing $ \sRenderFinisheds -> do
    Util.withFences dev (V.fromList (take (fromIntegral Config.consts.framesInFlight) (repeat ((zero :: Vk.FenceCreateInfo '[]) { Vk.flags = Vk.FENCE_CREATE_SIGNALED_BIT })))) Nothing $ \fences -> do
    Vk.withCommandBuffers dev (Config.commandBuffer gpool) bracket $ \cbuffers -> do
    rawVert <- BS.readFile Shaders.vertPath
    rawFrag <- BS.readFile Shaders.fragPath
    Util.withShaderModules dev (V.fromList [ zero { Vk.code = rawVert }, zero { Vk.code = rawFrag } ]) Nothing $ \mods -> do
    let vertMod = V.head mods
    let fragMod = V.head $ V.tail mods
    Vk.withPipelineLayout dev (Config.pipelineLayout) Nothing bracket $ \pipelineLayout -> do
    Vk.withGraphicsPipelines dev zero (V.singleton $ SomeStruct (Config.graphicsPipeline q vertMod fragMod pipelineLayout)) Nothing bracket $ \(_, pipelines) -> do
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
        Vk.cmdPipelineBarrier2 cbuffer $ Config.preRenderPipelineBarrier image
        Vk.cmdUseRendering cbuffer (Config.rendering q imageView) $ do
            Vk.cmdSetViewport cbuffer 0 $ Config.viewport q
            Vk.cmdSetScissor cbuffer 0 $ Config.scissor q
            Vk.cmdBindPipeline cbuffer Vk.PIPELINE_BIND_POINT_GRAPHICS pipeline
            Vk.cmdDraw cbuffer 3 1 0 0
        Vk.cmdPipelineBarrier2 cbuffer $ Config.postRenderPipelineBarrier image
    Vk.queueSubmit2 queue (Config.submitQueue cbuffer sImageAcquired sRenderFinished) $ V.head fence
    _ <- Vk.queuePresentKHR queue (Config.present swapchain imageIndex sRenderFinished)
    RGFW.rGFW_pollEvents
    gameloop' dev q pipeline swapchain imageViews images cbuffers queue sImageAcquireds sRenderFinisheds fences (mod (frameIndex + 1) (fromIntegral Config.consts.framesInFlight)) window =<< RGFW.rGFW_window_shouldClose window
gameloop' dev _ _ swapchain _ _ _ _ _ _ _ _ _ _ = do
    Vk.deviceWaitIdle dev
    Vk.destroySwapchainKHR dev swapchain Nothing