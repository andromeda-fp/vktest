module Main (main) where

import qualified Vulkan.Core10 as Vk
import Vulkan.Zero (zero)
import Control.Exception (bracket)
import Data.Foldable (traverse_)

main :: IO ()
main = Vk.withInstance zero Nothing bracket $ \i -> do
           putStrLn $ show i
	   (_, layers) <- Vk.enumerateInstanceLayerProperties
           (_, extensions) <- Vk.enumerateInstanceExtensionProperties Nothing
	   putStrLn $ show layers
	   putStrLn $ show extensions
	   (_, devices) <- Vk.enumeratePhysicalDevices i
	   traverse_ deviceInfo devices

deviceInfo :: Vk.PhysicalDevice -> IO ()
deviceInfo p = do
  (_, extensions) <- Vk.enumerateDeviceExtensionProperties p Nothing
  (_, layers) <- Vk.enumerateDeviceLayerProperties p
  traverse_ (putStrLn . show) extensions
  traverse_ (putStrLn . show) layers
  (putStrLn . show) =<< Vk.getPhysicalDeviceFeatures p
  (putStrLn . show) =<< Vk.getPhysicalDeviceProperties p
  (putStrLn . show) =<< Vk.getPhysicalDeviceMemoryProperties p