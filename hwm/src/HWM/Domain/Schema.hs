module HWM.Domain.Schema (TargetScope (..)) where

import HWM.Core.Pkg (Pkg)

data TargetScope
  = ScopeGlobal -- User typed: hwm build (Build everything)
  | ScopePkgs [Pkg] -- User typed: hwm build -w=libs/... (Build these)
  deriving (Eq, Show)