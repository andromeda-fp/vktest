{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- from the FIR docs
module Shaders where

import FIR
import Math.Linear

type FragmentDefs = '[ "in_pos" ':-> Input       '[ Location 0                 ] (V 2 Float)
                     , "out_col" ':-> Output     '[ Location 0                 ] (V 4 Float)
                     , "image"   ':-> Texture2D  '[ DescriptorSet 0, Binding 0 ] (RGBA8 UNorm)
                     , "main"    ':-> EntryPoint '[ OriginLowerLeft            ] Fragment
                     ]

fragment :: Module FragmentDefs
fragment = Module $ entryPoint @"main" @Fragment do
               pos <- get @"in_pos"
               col <- use @(ImageTexel "image") NilOps pos
               put @"out_col" col