{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_ebpf_tools (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "ebpf_tools"
version :: Version
version = Version [0,2,0,0] []

synopsis :: String
synopsis = "Assembler and disassembler for eBPF bytecode"
copyright :: String
copyright = "2022-2024 Ken Friis Larsen"
homepage :: String
homepage = "https://github.com/kfl/ebpf-tools"
