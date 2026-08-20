{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- from the FIR docs
module Shaders where

import FIR
import Math.Linear

vertices = Vec3 (Vec2   0 (-0.5))
                (Vec2   0.5  0.5)
                (Vec2 (-0.5) 0.5)

vertPath = "assets/shaders/vert.spv"
fragPath = "assets/shaders/frag.spv"

type VertexDefs =
    '[ "main" ':-> EntryPoint '[] Vertex ]

vertex :: ShaderModule "main" VertexShader VertexDefs _
vertex = shader  do
    i <- get @"gl_VertexIndex"
    (Vec2 x y) <- let' $ atv3v2f vertices i (Vec2 0 0)
    put @"gl_Position" (Vec4 x y 0 1)

type FragmentDefs =
    '[ "main"      ':-> EntryPoint '[ OriginUpperLeft ] Fragment
     , "out_color" ':-> Output     '[ Location 0      ] (V 4 Float)
     ]

fragment :: ShaderModule "main" FragmentShader FragmentDefs _
fragment = shader do
    put @"out_color" (Vec4 1.0 0.0 1.0 1.0)

atv3v2f :: Code (V 3 (V 2 Float)) -> Code Word32 -> Code (V 2 Float) -> Code (V 2 Float)
atv3v2f (Vec3 x y z) i d =
    if i == 0 then x
    else if i == 1 then y
    else if i == 2 then z
    else d